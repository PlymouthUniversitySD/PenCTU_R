#' Author: Paigan Aspinall
#' Date & version: 19FEB2024 V1.0.0
#' R version: 4.2.2
#'
#' Generate a dataset that returns all instances where the date validation rules defined in an external CSV are returned.Note that both dates being checked must be within
#' the repeating instrument, otherwise the regular date validation functions can be used.
#'
#' Produces a dataset of potentially invalid dates from dates within repeating instruments.
#'
#' @param dataset A complete dataset.
#' @param rules A CSV file that is imported and contains all of the date range checks to be performed. Created using the template: DateValidationTemplateRepeating_V1.0
#'
#' @return A dataset summarising potentially invalid date data.
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples

#' date_validation_output <- date_range_validation_repeating(dataset, rules)
#'
#' @export
#'

library(lubridate)

date_range_validation_repeating <- function(dataset, rules) {
  if(!is.null(dataset)) {
    if(!is.null(rules)){
      #initialize an empty dataframe to store the results
      output_df <- data.frame(record_id = numeric(),
                              field_name = character(),
                              event_name = character(),
                              error_message = character(),
                              stringsAsFactors = FALSE)
      
      if(
        "field_name" %in% colnames(rules) && 
        "event_name" %in% colnames(rules) &&
        "range_check_type" %in% colnames(rules) &&
        "lower_field_name" %in% colnames(rules) &&
        "upper_field_name" %in% colnames(rules) &&
        "error_message" %in% colnames(rules)
      ){
        #loop through each rule
        for (i in seq_len(nrow(rules))) {
          rule <- rules[i, ]
          #extract rule parameters
          field_name <- rule$field_name
          event_name <- rule$event_name
          range_check_type <- rule$range_check_type
          lower_field_name <- rule$lower_field_name
          upper_field_name <- rule$upper_field_name
          error_message <- rule$error_message
          
          #subset dataset based on the rule and range check type
          if (range_check_type == "upper") {
            suppressWarnings(invalid_rows <- which(!is.na(dataset[[upper_field_name]]) & is.na(dmy(dataset[[upper_field_name]])) & dataset$redcap_event_name == event_name))
            if (length(invalid_rows) == 0) {
              subset_data <- dataset[dataset$redcap_event_name == event_name & 
                                       !is.na(dataset[[field_name]]) & 
                                       !is.na(dataset[[upper_field_name]]) & 
                                       dataset[[field_name]] > dataset[[upper_field_name]], ]
            } else {
              stop(paste0("Invalid date format (expected dd/mm/yyyy) in rows: ", 
                          paste(invalid_rows, collapse = ", "), "; column: ",  upper_field_name, "; Range check type : Upper."))
            }
          } else if (range_check_type == "lower") {
            suppressWarnings(invalid_rows <- which(!is.na(dataset[[lower_field_name]]) & is.na(dmy(dataset[[lower_field_name]])) & dataset$redcap_event_name == event_name))
            if (length(invalid_rows) == 0) {
              subset_data <- dataset[dataset$redcap_event_name == event_name & 
                                       !is.na(dataset[[field_name]]) & 
                                       !is.na(dataset[[lower_field_name]]) & 
                                       dataset[[field_name]] > dataset[[lower_field_name]], ]
            } else {
              stop(paste0("Invalid date format (expected dd/mm/yyyy) in rows: ", 
                          paste(invalid_rows, collapse = ", "), "; column: ",  lower_field_name, "; Range check type : Lower."))
            }
          }
          
          #append results to the output dataframe
          if (nrow(subset_data) > 0) {
            output_df <- rbind(output_df, data.frame(record_id = subset_data$record_id,
                                                     field_name = field_name,
                                                     event_name = event_name,
                                                     error_message = error_message,
                                                     stringsAsFactors = FALSE))
          }
        }
        
        return(output_df)
      } else {
        stop("Rules CSV file does not have correct columns")
      }
    } else {
      stop("Rules not provided!")
    }
  } else {
    stop("Dataset not provided!")
  }
}