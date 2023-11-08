# Author: Paigan Aspinall
# Date: 20SEP2023
# R version: 4.2.2

#' Apply coded allocations to every row
#'
#' This function applies allocations to all rows for a participant
#'
#' @param data A REDcap export dataset.
#' @param data_dictionaryionary A REDcap export data dictionary.
#' @param allocation_column Column name where allocation data is stored.
#' @param allocation_event_name Event name where allocations are assigned to participants.

#' @return A dataframe containing allocations.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example useage:
#' dataset <- allocation_data_cleaning(data, data_dictionary, "pdallocation", "randomisation_arm_1")
#' 
#'
#' @export
#'


allocation_data_cleaning <- function(data, data_dictionaryionary, allocation_column, allocation_event_name) {
  
  pdallocation_row <- data_dictionary[data_dictionary$field_name == allocation_column, ]

  select_choices <- pdallocation_row$select_choices_or_calculations
  
  choices <- unlist(strsplit(select_choices, "\\|"))

  raw_values <- numeric()
  level_values <- character()

  for (choice in choices) {
    parts <- unlist(strsplit(trimws(choice), ","))
    if (length(parts) == 2) {
      raw_values <- c(raw_values, parts[1])
      level_values <- c(level_values, parts[2])
    }
  }

  responses_df <- data.frame(raw_value = raw_values, level_value = trimws(level_values))
  
  new_row <- data.frame(raw_value = 777, level_value = 'Awaiting randomisation')
  
  responses_df <- rbind(responses_df, new_row)
  
  allocation_mapping <-  data %>%
    filter(redcap_event_name == allocation_event_name) %>%
    select(record_id, allocation_column)
  
  data <- data %>%
    left_join(allocation_mapping, by = "record_id") %>%
    mutate(!!allocation_column := ifelse(!is.na(!!sym(paste0(allocation_column, '.y'))), !!sym(paste0(allocation_column, '.y')), !!sym(paste0(allocation_column, '.x')))) %>%
    select(-matches(paste0(allocation_column, '.x|', allocation_column, '.y')))
  
  data[[allocation_column]][is.na(data[[allocation_column]])] <- 777
  
  names(data)[which(names(data)==allocation_column)] <- "Allocation"  
  
  data$Allocation <- factor(data$Allocation, levels = responses_df$raw_value, labels = responses_df$level_value)
  
  return(data)
}
