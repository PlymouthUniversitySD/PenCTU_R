# Author: Paigan Aspinall
# Date & version: 09JAN2024 V1.1.0
# R version: 4.2.2

#' Identify events missing from the dataset and, if their expected event date has passed, add them to the data.
#'
#' This function adds a row for any event that is expected in the data and is missing. NOTE this script should be run after allocation_data_cleaning and before encode_site_names.
#'
#' @param dataset A REDCap export dataset.
#' @param event_data A CSV file containing the events and their associated anchors - template located in Github named "20240109_AddMissingEventsTemplate.csv".
#' @param condition Optional parameter that can be applied, e.g. if the events for intervention and control participants are different. Default is NULL.
#' @param allocation Logical, if allocations are used in the study then this should be true. Default to TRUE.
#' @param site Logical, if sites are used in the study then this should be true. Default to TRUE.

#' @return A data frame containing missing events.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example usage:
#' intervention_events <- read.csv("20240108_ExpectedEventsInvervention_V1.0.csv")
#' condition <- "Allocation == 'Intervention'"
#' 
#' complete_dataset <- add_missing_events(dataset, event_data, condition)
#'
#' @export
#'

add_missing_events <- function(dataset, event_data, condition=NULL, allocation=TRUE, site=TRUE){
  
  #calculate the upper limit of days after the anchor date
  event_data$days_total <- event_data$days_after + event_data$days_upper
  
  #if a condition is being used, filter based on this
  if(!is.na(condition)){
  filtered_data <- dataset %>% filter(eval(parse(text = condition)))}
  filtered_data$redcap_event_name <- as.character(filtered_data$redcap_event_name)

  #group the data based on record id
  grouped_large <- filtered_data %>%
    group_by(record_id)
  
  #create a data frame to store all combinations of record_id and event_name
  event_names <- unique(event_data$event_name)
  combinations <- grouped_large %>%
    summarize(record_id = first(record_id)) %>%
    crossing(redcap_event_name = event_names)
  
  #merge necessary columns from event_data with combinations based on redcap_event_name
  combinations <- merge(combinations, event_data, by.x = "redcap_event_name", by.y="event_name", all.x=TRUE )
  names(filtered_data)[which(names(filtered_data)=="redcap_event_name")] <- "anchor_event"
  combinations <- merge(combinations, filtered_data, by=c("record_id", "anchor_event"), all.x=TRUE)

  #apply the function to each row in the combinations dataset
  combinations <- t(apply(combinations, 1, replace_with_date))
  combinations <- as.data.frame(combinations)

  #select necessary columns from the combinations dataset
  combinations <- select(combinations, record_id, anchor_event, redcap_event_name, anchor_date, event_date, days_total)
  
  #calculate expected_event_date
  combinations$anchor_date <- as.Date(combinations$anchor_date, format = "%Y-%m-%d")
  combinations$days_total <- as.numeric(combinations$days_total)
  combinations$expected_event_date <- combinations$anchor_date + combinations$days_total
  
  #identify missing events based on record_id and redcap_event_name not present in dataset
  missing_events <- combinations[!(paste(combinations$record_id, combinations$redcap_event_name) %in% paste(dataset$record_id, dataset$redcap_event_name)), ]
  missing_event_dates <- missing_events

  #create blank dataset with the required columns
  new_columns <- dataset
  new_columns <- dataset[FALSE, ]
  new_columns <- new_columns[,!colnames(new_columns) %in% "record_id"]
  if(site && allocation){
    new_columns <- new_columns[,!colnames(new_columns) %in% "redcap_data_access_group"]
    new_columns <- new_columns[,!colnames(new_columns) %in% "Allocation"]  
    missing_rows <- data.frame(
      record_id = integer(0),
      redcap_data_access_group = character(0),
      Allocation = character(0),
      stringsAsFactors = FALSE
    )
    
    #for record ids from the missing events dataset collect site and allocation data from the dataset
    unique_record_ids <- unique(missing_events$record_id)  
    for (id in unique_record_ids) {
      matching_rows <- subset(dataset, record_id == id)
      matching_rows <- select(matching_rows, record_id, redcap_data_access_group, Allocation)
      
      if (nrow(matching_rows) > 0) {
        missing_rows <- rbind(missing_rows, matching_rows)
      }
    }  
    missing_rows <- unique(missing_rows)
    
    #use the collected parameters and add to each row for a missing event
    missing_events$record_id <- as.character(missing_events$record_id)
    missing_rows$record_id <- as.character(missing_rows$record_id)
    
    missing_events <- select(missing_events, record_id, redcap_event_name)  
    missing_rows <- left_join(missing_rows, missing_events %>% select(record_id, redcap_event_name), by = "record_id")
    desired_columns <- c("record_id", "redcap_event_name", "redcap_data_access_group", "Allocation")
  }
  
  
  if (allocation && !site) {
    new_columns <- new_columns[,!colnames(new_columns) %in% "Allocation"]  
    missing_rows <- data.frame(
      record_id = integer(0),
      Allocation = character(0),
      stringsAsFactors = FALSE
    )
    
    #for record ids from the missing events dataset collect allocation data from the dataset
    unique_record_ids <- unique(missing_events$record_id)  
    for (id in unique_record_ids) {
      matching_rows <- subset(dataset, record_id == id)
      matching_rows <- select(matching_rows, record_id, Allocation)
      
      if (nrow(matching_rows) > 0) {
        missing_rows <- rbind(missing_rows, matching_rows)
      }
    }  
    missing_rows <- unique(missing_rows)
    
    #use the collected parameters and add to each row for a missing event
    missing_events$record_id <- as.character(missing_events$record_id)
    missing_rows$record_id <- as.character(missing_rows$record_id)
    
    missing_events <- select(missing_events, record_id, redcap_event_name)  
    missing_rows <- left_join(missing_rows, missing_events %>% select(record_id, redcap_event_name), by = "record_id")
    desired_columns <- c("record_id", "redcap_event_name", "Allocation")  
  }
  
  if(site && !allocation){
    new_columns <- new_columns[,!colnames(new_columns) %in% "redcap_data_access_group"]
    missing_rows <- data.frame(
      record_id = integer(0),
      redcap_data_access_group = character(0),
      stringsAsFactors = FALSE
    )
    
    #for record ids from the missing events dataset collect site data from the dataset
    unique_record_ids <- unique(missing_events$record_id)  
    for (id in unique_record_ids) {
      matching_rows <- subset(dataset, record_id == id)
      matching_rows <- select(matching_rows, record_id, redcap_data_access_group)
      
      if (nrow(matching_rows) > 0) {
        missing_rows <- rbind(missing_rows, matching_rows)
      }
    }  
    missing_rows <- unique(missing_rows)
    
    #use the collected parameters and add to each row for a missing event
    missing_events$record_id <- as.character(missing_events$record_id)
    missing_rows$record_id <- as.character(missing_rows$record_id)
    
    missing_events <- select(missing_events, record_id, redcap_event_name)  
    missing_rows <- left_join(missing_rows, missing_events %>% select(record_id, redcap_event_name), by = "record_id")
    desired_columns <- c("record_id", "redcap_event_name", "redcap_data_access_group")
  }   
  
  if(!site && !allocation){
    missing_rows <- data.frame(
      record_id = integer(0),
      stringsAsFactors = FALSE
    )
    
    unique_record_ids <- unique(missing_events$record_id)  
    for (id in unique_record_ids) {
      matching_rows <- subset(dataset, record_id == id)
      matching_rows <- select(matching_rows, record_id)
      
      if (nrow(matching_rows) > 0) {
        missing_rows <- rbind(missing_rows, matching_rows)
      }
    }  
    missing_rows <- unique(missing_rows)
    
    #use the collected parameters and add to each row for a missing event
    missing_events$record_id <- as.character(missing_events$record_id)
    missing_rows$record_id <- as.character(missing_rows$record_id)
    
    missing_events <- select(missing_events, record_id, redcap_event_name)  
    missing_rows <- left_join(missing_rows, missing_events %>% select(record_id, redcap_event_name), by = "record_id")
    desired_columns <- c("record_id", "redcap_event_name")
  }   
      
  #add in all other columns from the dataset with NA values
  other_columns <- setdiff(names(dataset), desired_columns)
  datnum_rows <- nrow(missing_rows)
  new_columns <- new_columns[,!colnames(new_columns) %in% "redcap_event_name"]  
  new_rows <- matrix(NA, nrow = datnum_rows, ncol = ncol(new_columns))
  new_rows <- as.data.frame(new_rows)
  colnames(new_rows) <- colnames(new_columns)
  missing_data <- cbind(missing_rows, new_rows)
  
  #bind the new rows of missing events with the original dataset
  data_output <- rbind(dataset, missing_data)
  
  #remove events for which the expected event date is in the future
  data_output_dates <- merge(data_output, missing_event_dates, by=c("record_id", "redcap_event_name"), all.x=TRUE)
  filtered_data <- subset(data_output_dates, is.na(expected_event_date) | expected_event_date <= Sys.Date())
  
  #remove columns that are not required
  filtered_data <- subset(filtered_data, select = -c(anchor_event, anchor_date, event_date, days_total))
  
  return(filtered_data)

}
