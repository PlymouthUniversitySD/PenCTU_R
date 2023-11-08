#Define the events table for this function:

url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
formData <- list("token"=token,
                 content='event',
                 format='csv',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
events <- unique(result$unique_event_name)
events_intervention <- events[grepl("^week", events)]
events_control <- events[grepl("^week.*fes", events)]

anchor_dates <- c(NA, "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat")
anchor_event <- c(NA, "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1", 
                  "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1", "week_0_blinded_ass_arm_1")
event_dates <- c("hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat", "hoappointdat")
days_after <- c(NA, 7, 14, 14, 42, 42, 126, 126, 154)
days_upper <- c(NA, 7, 7, 5, 5, 5, 5, 5, 5)
days_lower <- c(NA, 2, 2, 5, 5, 5, 5, 5, 5)

event_list_intervention <- list()

for (i in 1:length(events_intervention)) {
  event_df <- data.frame(
    Event_Name = events_intervention[i],
    Anchor_Date = anchor_dates[i],
    Anchor_event = anchor_event[i],
    Event_Date = event_dates[i],
    Days_After = days_after[i],
    Days_Upper = days_upper[i],
    Days_Lower = days_lower[i]
  )

  event_list_intervention[[events_intervention[i]]] <- event_df
}

condition <- "Allocation == 'Intervention'"

calculate_missing_events <- function(dataset, condition, event_data) {
  
  filtered_data <- dataset %>% filter(eval(parse(text = condition)))
  filtered_data$redcap_event_name <- as.character(filtered_data$redcap_event_name)
  
  calculateFutureDate <- function(custom_record_id, event_data, filtered_data) {
    anchor_event_row <- filtered_data %>%
      filter(record_id == custom_record_id, redcap_event_name == event_data$Anchor_event)
    
    if (nrow(anchor_event_row) == 0) {
      return(data.frame(record_id = custom_record_id, event_name = event_data$Event_Name, missing = NA))
    }
    
    # Check if an event with the same name and record ID already exists
    event_exists <- any(filtered_data$record_id == custom_record_id & filtered_data$redcap_event_name == event_data$Event_Name)
    
    if (event_exists) {
      return(data.frame(record_id = custom_record_id, event_name = event_data$Event_Name, missing = FALSE))
    }
    
    anchor_date <- as.Date(anchor_event_row[[event_data$Anchor_Date]])
    future_date <- anchor_date + event_data$Days_After + event_data$Days_Upper
    
    if (future_date < Sys.Date()) {
      return(data.frame(record_id = custom_record_id, event_name = event_data$Event_Name, missing = TRUE))
    }
    
    return(data.frame(record_id = custom_record_id, event_name = event_data$Event_Name, missing = FALSE))
  }
  
  
  missing_events <- expand.grid(
    record_id = unique(filtered_data$record_id),
    event_name = names(event_data)
  )
  
  calculated_missing <- map2_df(
    missing_events$record_id,
    missing_events$event_name,
    ~ calculateFutureDate(.x, event_data[[.y]], filtered_data)
  )
  
  calculated_missing <- subset(calculated_missing, missing == "TRUE")
  return(calculated_missing)
}
  
add_missing_events <- function(dataset, calculated_missing){  

new_columns <- dataset
  new_columns <- dataset[FALSE, ]
  new_columns <- new_columns[,!colnames(new_columns) %in% "record_id"]
  new_columns <- new_columns[,!colnames(new_columns) %in% "redcap_data_access_group"]
  new_columns <- new_columns[,!colnames(new_columns) %in% "Allocation"]  
  
  missing_rows <- data.frame(
    record_id = integer(0),
    redcap_data_access_group = character(0),
    Allocation = character(0),
    stringsAsFactors = FALSE
  )
  
  unique_record_ids <- unique(calculated_missing$record_id)
  
  for (id in unique_record_ids) {
    matching_rows <- subset(dataset, record_id == id)
    matching_rows <- select(matching_rows, record_id, redcap_data_access_group, Allocation)

    if (nrow(matching_rows) > 0) {
      missing_rows <- rbind(missing_rows, matching_rows)
    }
  }
  
  missing_rows <- unique(missing_rows)
  
  calculated_missing$record_id <- as.character(calculated_missing$record_id)
  missing_rows$record_id <- as.character(missing_rows$record_id)
  
  calculated_missing <- select(calculated_missing, record_id, event_name)
  
  missing_rows <- left_join(missing_rows, calculated_missing %>% select(record_id, event_name), by = "record_id")
  missing_rows <- missing_rows %>% rename(redcap_event_name = event_name)

  desired_columns <- c("record_id", "redcap_event_name", "redcap_data_access_group", "Allocation")
  other_columns <- setdiff(names(dataset), desired_columns)
  datnum_rows <- nrow(missing_rows)
  new_columns <- new_columns[,!colnames(new_columns) %in% "redcap_event_name"]  
  new_rows <- matrix(NA, nrow = datnum_rows, ncol = ncol(new_columns))
  new_rows <- as.data.frame(new_rows)
  colnames(new_rows) <- colnames(new_columns)
  missing_data <- cbind(missing_rows, new_rows)
  data_output <- rbind(dataset, missing_data)
  
  return(data_output)
    }
  
  


