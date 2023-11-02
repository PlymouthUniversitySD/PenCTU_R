# Author: Paigan Aspinall
# Date: 20SEP2023
# R version: 4.2.2

library(dplyr) #1.1.0

#' Apply coded allocations to every row
#'
#' This function applies allocations to all rows for a participant
#'
#' @param data A REDcap export dataset.
#' @param allocation_column Column name where allocation data is stored.
#' @param allocation_event_name Event name where allocations are assigned to participants.

#' @return A dataframe containing allocations.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example useage:
#' dataset <- allocation_data_cleaning(data, "pdallocation", "randomisation_arm_1")
#' 
#'
#' @export
#'


allocation_data_cleaning <- function(data, allocation_column, allocation_event_name) {
  clean_data <- data
  
  allocation_mapping <-  clean_data %>%
    filter(redcap_event_name == allocation_event_name) %>%
    select(record_id, allocation_column)
  
  clean_data <- clean_data %>%
    left_join(allocation_mapping, by = "record_id") %>%
    mutate(!!allocation_column := ifelse(!is.na(!!sym(paste0(allocation_column, '.y'))), !!sym(paste0(allocation_column, '.y')), !!sym(paste0(allocation_column, '.x')))) %>%
    select(-matches(paste0(allocation_column, '.x|', allocation_column, '.y')))
  
  clean_data[[allocation_column]][is.na(clean_data[[allocation_column]])] <- 777
  
  names(clean_data)[which(names(clean_data)==allocation_column)] <- "Allocation"  
  
  clean_data$Allocation <- factor(clean_data$Allocation, levels = reformatted_df$raw_value, labels = reformatted_df$level_value)
  
  return(clean_data)
}
