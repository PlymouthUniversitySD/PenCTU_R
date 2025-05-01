# Author: Paigan Aspinall
# Date: 20SEP2023
# R version: 4.2.2

#' Apply coded allocations to every row
#'
#' This function applies allocations to all rows for a participant
#'
#' @param data A REDcap export dataset.
#' @param data_dictionary A REDcap export data dictionary.
#' @param field_name_column Column name where field names are stored in data dictionary
#' @param select_choices_column Column name where allocation data codes (1 for Control, 2 for Intervention) will be retrieved
#' @param allocation_column Column name where allocation data is stored.
#' @param allocation_event_name Event name where allocations are assigned to participants.

#' @return A dataframe containing allocations.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example useage:
#' dataset <- allocation_data_cleaning(data, data_dictionary, "Variable...Field.Name", "Choices..Calculations..OR.Slider.Labels", "pdallocation", "randomisation_arm_1")
#' 
#'
#' @export
#'


allocation_data_cleaning <- function(data, data_dictionary, field_name_column, select_choices_column, allocation_column, allocation_event_name) {
  # Check for null parameters
  params <- list(data = data, data_dictionary = data_dictionary, 
                 allocation_column = allocation_column, allocation_event_name = allocation_event_name)
  
  null_params <- sapply(params, is.null)
  
  if(any(null_params)) {
    null_param_names <- names(null_params)[null_params]
    stop(paste("Null values provided for parameters:", paste(null_param_names, collapse = ", ")))
  }
  
  # Check that data and data_dictionary are valid filetypes
  if(!is.data.frame(data) || !is.data.frame(data_dictionary)) {
    stop("Files provided are not valid")
  }
  
  if(!field_name_column %in% colnames(data_dictionary)) {
    stop("Field Name Column provided doesn't match any existing columns in the data dictionary!")
  }
  
  if(!select_choices_column %in% colnames(data_dictionary)) {
    stop("Select Choices Column provided doesn't match any existing columns in the data dictionary!")
  }
  
  # Check that field name provided matches with existing field names in data dictionary
  if (!allocation_column %in% data_dictionary[[field_name_column]]) {
    stop(paste(allocation_column, " provided for allocation_column doesn't match existing field names in data dictionary"))
  }
  
  # Check that event name provided matches existing event names in dataset.
  if(!(allocation_event_name %in% data$redcap_event_name)){
    stop("Value provided for allocation_event_name doesn't match existing event names in dataset")
  }
  
  # Function as normal after all checks have been passed
  
  pdallocation_row <- data_dictionary[data_dictionary[[field_name_column]] == allocation_column, ]
  
  select_choices <- pdallocation_row[[select_choices_column]]
  
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
