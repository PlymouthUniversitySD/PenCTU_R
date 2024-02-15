#Author: Paigan Aspinall
#Date & version: 15FEB2024 V1.0.1
#R version: 4.2.2

#Unblinded weekly report script

#Set your API token
token <- '86CDF603FECAD55D05B27F6D3FA5ACEC'

plot_recruitment_overall <- function(data, recruitment_start, recruitment_length_months, recruitment_target, enrollment_date_column) {
  
  #get today's date
  today_date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
  
  #convert date values to date format
  data[[enrollment_date_column]] <- as.Date(data[[enrollment_date_column]])
  recruitment_start <- as.Date(recruitment_start)
  #calculate the end date of recruitment
  recruitment_end <- recruitment_start + months(recruitment_length_months)
  
  #prepare enrolment data
  enrollment_data <- data %>% 
    #create a year-month column
    mutate(year_month = format(.data[[enrollment_date_column]], "%Y-%m")) %>% 
    #Remove NA values from date column
    filter(!is.na(.data[[enrollment_date_column]])) %>%
    #count enrolments per year-month
    count(year_month, name="count") %>%
    #arrange by date
    arrange(as.Date(paste0(year_month, "-01")))
  
  #generate a sequence of year-months from recruitment start
  year_months <- seq(from = floor_date(recruitment_start, unit = "month"), 
                     to = floor_date(today_date, unit = "month"), by = "1 month")
  year_months <- format(year_months, "%Y-%m")
  #create a dataframe with all year-months
  all_months_data <- data.frame(year_month = year_months)
  #left join enrollment data wkith all months data
  enrollment_data <- all_months_data %>%
    left_join(enrollment_data, by = "year_month") %>%
    #replace NA values with 0
    mutate(count = replace_na(count, 0))
  #calculate a cumulative count of enrollments
  enrollment_data <- enrollment_data %>%
    mutate(cumulative_count = cumsum(count))
  
  #convert year-month column into date format
  enrollment_data$year_month <- as.Date(paste0(enrollment_data$year_month, "-01"), format = "%Y-%m-%d")
  
  #generate the recruitment plot
  recruitment_plot <- ggplot(enrollment_data, aes(x = year_month, y = cumulative_count)) +
    geom_line(aes(color = "Actual"), size = 1) +
    geom_point(aes(color = "Actual"), size = 3) +
    geom_segment(aes(x = year_month[1], y = recruitment_target/recruitment_length_months, xend = recruitment_end, yend = recruitment_target, color = "Target"), linetype = "dashed") +
    ylab("Recruitment") + xlab("Date") +
    geom_text(aes(label=cumulative_count), vjust=-0.7)+
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y")) +
    #Amended breaks=seq
    scale_y_continuous(expand = c(0, 0), limits = c(0, (recruitment_target+10)), breaks=seq(0, 300, 50)) +
    #Amended labels
    scale_color_manual(name = "Key:", values = c("Actual" = "#F8766D", "Target" = "#00B0F6"), labels = c("Actual recruitment", "Target recruitment")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  recruitment_plot
}

plot_recruitment_by_site <- function(recruitment_start, enrolment_date_data, site_data, site_names=NULL, recruitment_end=Sys.Date()){
  
  # Convert parameters to appropriate date formats
  recruitment_start <- as.Date(recruitment_start)
  recruitment_end <- as.Date(recruitment_end)
  enrolment_date_data <- as.Date(enrolment_date_data)
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Initialize an empty dataframe to store plot data
  plot_data <- data.frame()
  
  # If site_names is NULL, generate it from unique non-NA values in site_data
  if (is.null(site_names)) {
    site_names <- unique(na.omit(site_data))
  }
  
  # Calculate recruitment length in months
  recruitment_length_months <- ceiling(difftime(recruitment_end, recruitment_start, units = "days") / 30.44)
  
  # Prepare enrolment data for plotting
  enrolment_data <- tibble(enrolment_date_data = enrolment_date_data, site_data = site_data) %>%
    mutate(year_month = format(enrolment_date_data, "%Y-%m")) %>% 
    filter(!is.na(enrolment_date_data)) %>%
    count(site_data, year_month, name = "count") %>%
    arrange(site_data, as.Date(paste0(year_month, "-01")))
  
  # Iterate over each site to generate plot data
  for (site_name in site_names) {
    month_count_site <- enrolment_data %>%
      filter(site_data == site_name)
    recruits_site <- data.frame(year_month = seq(as.Date(format(recruitment_start, "%Y-%m-01")), 
                                                 by = "1 month", 
                                                 length.out = recruitment_length_months),
                                count = rep(0, recruitment_length_months), # Initialize count with zeros
                                Site = site_name)%>% 
      mutate(year_month = format(year_month, "%Y-%m"))
    # Check if there are matching year_month values before assignment
    matching_indices <- match(month_count_site$year_month, recruits_site$year_month)
    # Assign values only if matching indices are found and not NA
    if (!any(is.na(matching_indices))) {
      recruits_site$count[matching_indices[!is.na(matching_indices)]] <- month_count_site$count
    }
    recruits_site$cumulative_count <- cumsum(recruits_site$count)
    plot_data <- rbind(plot_data, recruits_site)
  }
  
  
  # Define custom colors for sites
  custom_colors <- c("#F8766D", "#00BFC4", "#CD9600", "#C77CFF","#7CAE00","#FB61D7","#619CFF","#00BA38","#A3A500")
  
  # Prepare plot data for plotting
  plot_data <- plot_data %>%
    arrange(Site, year_month) %>%
    group_by(Site) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
    mutate(cumulative_count = ifelse(year_month > as.Date(today_date), NA, cumulative_count))
  
  # Generate recruitment by site plot
  recruitment_by_site_plot <- ggplot(data = plot_data, aes(x = year_month, y = cumulative_count, group = Site)) +
    geom_line(aes(color = Site), size = 2) +
    geom_point(aes(color = Site, shape = Site), size = 3) +
    labs(x = "Month", y = "Number recruited", title = "Patients recruited by site") +
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"))+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust x-axis labels angle
  
  # Return recruitment by site plot
  return(recruitment_by_site_plot)
}

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
  filtered_data <- subset(data_output_dates, 
                          (is.na(anchor_event)) | 
                            (!is.na(anchor_event) & expected_event_date <= Sys.Date()))
  
  
  #remove columns that are not required
  filtered_data <- subset(filtered_data, select = -c(anchor_event, anchor_date, event_date, days_total))
  
  return(filtered_data)
  
}