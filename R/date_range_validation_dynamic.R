#' Author: Paigan Aspinall
#' Date & version: 10FEB2024 V1.0.1
#' R version: 4.2.2
#'
#' Generate a dataset that returns all instances where the date validation rules defined in an external CSV are returned.
#'
#' Produces a dataset of potentially invalid date data.
#'
#' @param dataset A complete dataset.
#' @param rules A CSV file that is imported and contains all of the date range checks to be performed. Use template STU_DM_023_DateValidationTemplate_dynamic.R
#'
#' @return A dataset summarising potentially invalid date data.
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' 
#' 
#' date_validation_output_dynamic <- date_range_validation(dataset, rules)
#'
#' @export
#'

date_range_validation_dynamic <- function(dataset, rules) {
  if(!is.null(dataset)) {
    if(!is.null(rules)){
      
      # Helper function to check if a value is a valid date
      is_valid_date <- function(date_string) {
        !is.na(as.Date(date_string, format = "%d/%m/%Y"))
      }
      
      #create an output dataframe
      
      final_subset <- data.frame()
      
      if("field_name" %in% colnames(rules) &&
         "event_name" %in% colnames(rules) &&
         "range_check_type" %in% colnames(rules) &&
         "lower_field_name" %in% colnames(rules) &&
         "lower_event_name" %in% colnames(rules) &&
         "upper_field_name" %in% colnames(rules) &&
         "upper_event_name" %in% colnames(rules) && 
         "error_message" %in% colnames(rules) &&
         "offset" %in% colnames(rules)
      ) {
        for (i in 1:nrow(rules)) {
          
          #set parameters from the rules dataset
          
          field_name <- as.character(rules$field_name[i])
          
          event_name <- as.character(rules$event_name[i])
          
          #extract relevant fields from the rules data frame
          
          range_check_type <- as.character(rules$range_check_type[i])
          
          lower_field_name <- as.character(rules$lower_field_name[i])
          
          lower_event_name <- as.character(rules$lower_event_name[i])
          
          upper_field_name <- as.character(rules$upper_field_name[i])
          
          upper_event_name <- as.character(rules$upper_event_name[i])
          
          error_message <- as.character(rules$error_message[i])
          
          offset <- as.numeric(rules$offset[i])
          
          #filter the dataset based on event and field
          
          filtered_data <- dataset %>%
            
            filter(record_id %in% unique(dataset$record_id[dataset$redcap_event_name == event_name]) &
                     
                     redcap_event_name == event_name & !is.na(get(field_name)))
          
          invalid_dates <- filtered_data[!sapply(filtered_data[[field_name]], is_valid_date), ]
          
          if (nrow(invalid_dates) > 0) {
            
            error_messages <- c()
            
            for (i in 1:nrow(invalid_dates)) {
              # Extract values for the current row
              record_id <- invalid_dates$record_id[i]
              event_name <- event_name
              value <- invalid_dates[record_id, field_name]
              
              # Check for NULL or NA values
              if (is.null(value) || is.na(value) || value == "") {
                error_messages <- c(error_messages, paste0(
                  "Invalid date string found at record_id: ", record_id, 
                  "; field_name: ", field_name, 
                  "; event_name: ", event_name, 
                  "; value: NULL or NA; Error: Null value"
                ))
              } else if (!is.null(value) && !is.na(value)) {
                if(!grepl("^\\d{2}/\\d{2}/\\d{4}$", value)) {
                  error_messages <- c(error_messages, paste0(
                    "Invalid date string found at record_id: ", record_id, 
                    "; field_name: ", field_name, 
                    "; event_name: ", event_name, 
                    "; value: ", value,
                    "; Error: Cannot be converted to date"
                  ))
                }
              }
            }
            
            # If there are any error messages, stop and display them all
            if (length(error_messages) > 0) {
              stop(paste("The following errors were found within this dataset:\n", paste(error_messages, collapse = "\n")))
            }
          }
          
          #apply date range checks based on 'range_check_type'
          
          if (range_check_type == "lower") {
            
            #create a new filtered dataset where redcap_event_name == lower_event_name
            
            lower_event_data <- dataset %>%
              
              filter(redcap_event_name == lower_event_name)
            
            lower_event_data[[lower_field_name]] <- as.Date(lower_event_data[[lower_field_name]])
            
            lower_event_data$lower_date <- as.Date(lower_event_data[[lower_field_name]])+offset
            
            #merge the data from filtered_data and lower_event_data by record_id
            
            merged_data <- merge(filtered_data, lower_event_data, by = "record_id", all.x = TRUE)
            
            #filter the data to only keep rows where Field < lower_field_name
            
            field_name <- paste0(field_name, '.x')
            
            lower_field_name <- paste0(lower_field_name, '.y')
            
            merged_data[[field_name]] <- as.Date(merged_data[[field_name]])
            
            subset_data <- subset(merged_data, merged_data[[field_name]] < merged_data$lower_date)
            
            if (nrow(subset_data) > 0) {
              
              subset_data$error <- error_message
              
              #add error_message, field_name, and event_name to the subset_data
              
              subset_data$field_name <- field_name
              
              subset_data$event_name <- event_name
              
              subset_data <- select(subset_data, record_id, event_name, field_name, error)
              
            }
            
          } else if (range_check_type == "upper") {
            
            #create a new filtered dataset where redcap_event_name == lower_event_name
            
            upper_event_data <- dataset %>%
              
              filter(redcap_event_name == upper_event_name)
            
            upper_event_data[[upper_field_name]] <- as.Date(upper_event_data[[upper_field_name]])
            
            upper_event_data$upper_date <- as.Date(upper_event_data[[upper_field_name]])+offset
            
            #merge the data from filtered_data and lower_event_data by record_id
            
            merged_data <- merge(filtered_data, upper_event_data, by = "record_id", all.x = TRUE)
            
            #filter the data to only keep rows where Field < lower_field_name
            
            field_name <- paste0(field_name, '.x')
            
            upper_field_name <- paste0(upper_field_name, '.y')
            
            merged_data[[field_name]] <- as.Date(merged_data[[field_name]])
            
            merged_data$upper_date <- as.Date(merged_data$upper_date)
            
            subset_data <- subset(merged_data, merged_data[[field_name]] > merged_data$upper_date)
            
            if (nrow(subset_data) > 0) {
              
              subset_data$error <- error_message
              
              #add error_message, field_name, and event_name to the subset_data
              
              subset_data$field_name <- field_name
              
              subset_data$event_name <- event_name
              
              subset_data <- select(subset_data, record_id, event_name, field_name, error)
              
            }
            
          }
          
          if (nrow(subset_data) > 0) {
            
            final_subset <- rbind(final_subset, subset_data)
            
          }
          
        }
        
        final_subset$field_name <- gsub("\\.x$", "", final_subset$field_name)
        
        return(final_subset)
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