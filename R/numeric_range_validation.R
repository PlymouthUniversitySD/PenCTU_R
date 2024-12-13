#' Author: Paigan Aspinall
#' Date & version: 16FEB2024 V1.1.0
#' R version: 4.2.2
#'
#' Generate a dataset that returns all instances where the numeric validation rules defined in the CSV are broken.
#'
#' Produces a dataset of potentially invalid numeric values.
#'
#' @param dataset A complete dataset.
#' @param rules A CSV file that is imported and contains all of the numeric range checks to be performed. Created using the template: NumericValidationTemplate_V1.0
#'
#' @return A dataset summarising potentially invalid numeric data.
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples

#' numeric_range_validation_output <- numeric_range_validation(dataset, rules)
#'
#' @export
#'


numeric_range_validation <- function(dataset, rules) {
  if(is.null(dataset)) {
    stop("Dataset not provided!")
  }
  
  if(is.null(rules)) {
    stop(".csv file for rules has not been provided!")
  }
  
  # Initialize an empty dataframe to store the results
  output_df <- data.frame(record_id = numeric(),
                          field_name = character(),
                          event_name = character(),
                          error_message = character(),
                          stringsAsFactors = FALSE)
  
  # Loop through each rule
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, ]
    # Extract rule parameters
    if("field_name" %in% colnames(rules)) {
      field_name <- rule$field_name
    } else {
      stop("'field_name' column not present in rules .CSV file")
    }
    
    if("event_name" %in% colnames(rules)) {
      event_name <- rule$event_name
    } else {
      stop("'event_name' column not present in rules .CSV file")
    }
    
    if("range_check_type" %in% colnames(rules)) {
      range_check_type <- rule$range_check_type
    } else {
      stop("'range_check_type' column not present in rules .CSV file")
    }
    
    if("lower_value" %in% colnames(rules)) {
      lower_value <- ifelse(is.numeric(rule$lower_value), as.numeric(rule$lower_value), stop("Lower Value is in non-numerical format!"))
    } else {
      stop("'lower_value' column not present in rules .CSV file")
    }
    
    if("upper_value" %in% colnames(rules)) {
      upper_value <- ifelse(is.numeric(rule$upper_value), as.numeric(rule$upper_value), stop("Upper Value is in non-numerical format!"))
    } else {
      stop("'upper_value' column not present in rules .CSV file")
    }
   
    if ("error_message" %in% colnames(rules)) {
      error_message <- rule$error_message
    } else {
      stop("'error_message' not present in rules .CSV file") 
    }
    
    # Subset dataset based on the rule and range check type
    if (range_check_type == "upper") {
      subset_data <- subset(dataset, redcap_event_name == event_name)
      subset_data <- subset_data %>%
        filter(!is.na(!!sym(field_name)), !!sym(field_name) > upper_value)
      
    } else if (range_check_type == "lower") {
      subset_data <- subset(dataset, redcap_event_name == event_name)
      subset_data <- subset_data %>%
        filter(!is.na(!!sym(field_name)), !!sym(field_name) < lower_value)
    }
    
    # Append results to the output dataframe
    if (nrow(subset_data) > 0) {
      output_df <- rbind(output_df, data.frame(record_id = subset_data$record_id,
                                               field_name = field_name,
                                               event_name = event_name,
                                               error_message = error_message,
                                               stringsAsFactors = FALSE))
    }
  }
  
  return(output_df)
}
