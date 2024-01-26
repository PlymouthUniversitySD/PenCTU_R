# Author: Paigan Aspinall
# Date: 09JAN2024
# R version: 4.2.2

#' Identify events that occur after withdrawal date and remove from dataset.
#'
#' This function is used after add_missing_events and confirms that any added events are not expected to occur after withdrawal dates, any events occurring after withdrawal are removed.
#' 
#' @param dataset A REDcap export dataset.
#' @param withdrawal_event The name of the event where withdrawals are reported.
#' @param withdrawal_date_field Field in the data where the withdrawal date is defined.
#' @param scheduled_event_data Dataset containing the paramaters for each event - template located in Github named "20240109_EventsTemplate.csv".
#' @param early_discontinuation Logical, can be set to TRUE if early discontinuations are also recorded in the withdrawal form. Default is FALSE.
#' @param early_discontinuation_field Field in the data where the user defines whether the withdrawal is an early discontinuation or not. Default is NULL.
#' @param early_discontinuation_value The value in the early discontinuation field that indicates the form is for a full withdrawal. Default is NULL.
#'
#' @return A dataframe without events scheduled to occur after withdrawal.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example useage:
#' 
#' scheduled_event_data <- read.csv("20240110_ScheduledEvents_V1.0.csv")
#' 
#' complete_dataset <- remove_withdrawn_events(dataset, "withdraw_arm_1", "wiwithdrawdat", scheduled_event_data, early_discontinuation=TRUE, "wiearlydiscon", "Entire study")
#'
#' @export
#'

remove_withdrawn_events <- function(dataset, withdrawal_event, withdrawal_date_field, scheduled_event_data, early_discontinuation=FALSE, early_discontinuation_field=NULL, early_discontinuation_value=NULL){
  
  #calculate the upper limit of days after the anchor date
  scheduled_event_data$days_total <- scheduled_event_data$days_after + scheduled_event_data$days_upper

  #group the data based on record id
  grouped_large <- dataset %>%
    group_by(record_id)
  
  #create a data frame to store all combinations of record_id and event_name
  event_names <- unique(scheduled_event_data$event_name)
  combinations <- grouped_large %>%
    summarize(record_id = first(record_id)) %>%
    crossing(redcap_event_name = event_names)
  
  filtered_data <- dataset
  
  #merge necessary columns from event_data with combinations based on redcap_event_name
  combinations <- merge(combinations, scheduled_event_data, by.x = "redcap_event_name", by.y="event_name", all.x=TRUE )
  names(filtered_data)[which(names(filtered_data)=="redcap_event_name")] <- "anchor_event"
  combinations <- merge(combinations, filtered_data, by=c("record_id", "anchor_event"), all.x=TRUE)
  
  #function to replace the 'anchor_date' column with the corresponding date value
  replace_with_date <- function(row) {
    anchor_date_col <- as.character(row[['anchor_date']])
    if (!is.na(anchor_date_col) && anchor_date_col %in% names(row)) {
      row[['anchor_date']] <- row[[anchor_date_col]]
    }
    return(row)
  }
  
  #apply the function to each row in the combinations dataset
  combinations <- t(apply(combinations, 1, replace_with_date))
  combinations <- as.data.frame(combinations)
  
  #select necessary columns from the combinations dataset
  combinations <- select(combinations, record_id, anchor_event, redcap_event_name, anchor_date, event_date, days_total)
  
  #calculate expected_event_date
  combinations$anchor_date <- as.Date(combinations$anchor_date, format = "%Y-%m-%d")
  combinations$days_total <- as.numeric(combinations$days_total)
  combinations$expected_event_date <- combinations$anchor_date + combinations$days_total
  
  #merge the expected_event_date with the dataset
  dataset <- merge(dataset, combinations, by=c("record_id", "redcap_event_name"), all.x=TRUE)
  
  #subset dataset to isolate withdsrawal events
  withdrawal_data <- subset(dataset, redcap_event_name == withdrawal_event)
  
  #remove any withdrawal events that are early discontinuations
  if(early_discontinuation){
    withdrawal_data[[early_discontinuation_field]] <- as.character(withdrawal_data[[early_discontinuation_field]])
    withdrawal_data[[early_discontinuation_field]] <- trimws(withdrawal_data[[early_discontinuation_field]])
    
    withdrawal_data <- subset(
      withdrawal_data,
      withdrawal_data[[early_discontinuation_field]] == early_discontinuation_value
    )
  }

  #format the withdrawal data
  withdrawal_data <- select(withdrawal_data, record_id, withdrawal_date_field)
  withdrawal_data[[withdrawal_date_field]] <- as.Date(withdrawal_data[[withdrawal_date_field]])
  names(withdrawal_data)[names(withdrawal_data) == withdrawal_date_field] <- "withdrawal_date"
  
  #merge the withdrawal data with the dataset
  dataset_merge <- merge(dataset, withdrawal_data, by="record_id", all.x=TRUE)

  #identify rows where withdrawal date is before expected event date
  rows_to_remove <- subset(dataset_merge, withdrawal_date < expected_event_date)
  
  #remove identified rows
  dataset_merge <- dataset_merge[!(rownames(dataset_merge) %in% rownames(rows_to_remove)), ]
  
  dataset_merge <- subset(dataset_merge, select = -c(anchor_event, anchor_date, event_date, days_total, expected_event_date))
  
  return(dataset_merge)
  
}
