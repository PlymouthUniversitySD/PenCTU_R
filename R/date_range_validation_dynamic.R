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
library('lubridate')

date_range_validation_dynamic <- function(dataset, rules) {
  if(!is.null(dataset)) {
    if(!is.null(rules)){
        #create an output dataframe
        final_subset <- data.frame()
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
            
            #apply date range checks based on 'range_check_type'
            
            if (range_check_type == "lower") {
              
              #create a new filtered dataset where redcap_event_name == lower_event_name
              
              lower_event_data <- dataset %>%
                
                filter(redcap_event_name == lower_event_name)
        
              if(grepl("^\\d{2}\\/\\d{2}\\/(\\d{2}|\\d{4})\\b", lower_event_data[[lower_field_name]])) {
                stop(paste0("Lower: ", lower_field_name, ": record ID ", lower_event_data$record_id, " is not a valid date"))
              }
              
              invalid_dates <- which(is.na(as.Date(lower_event_data[[lower_field_name]], format = "%d/%m/%Y")))
              if (length(invalid_dates) > 0) {
                stop(paste0("Invalid date strings found at record_ids: ", 
                            paste(lower_event_data$record_id[invalid_dates], collapse = ", "), "; field_name: ", lower_field_name, 
                            "; event_name: ", lower_event_name, "; range_check_type: lower \n"))
              }
              
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
              
              if(grepl("^\\d{2}\\/\\d{2}\\/(\\d{2}|\\d{4})\\b", upper_event_data[[upper_field_name]])) {
                stop(paste0("Upper: ", upper_field_name, ": record ID ", upper_event_data$record_id, " is not a valid date"))
              }
              
              if(is.Date(as.Date(upper_event_data[[upper_field_name]], "%d/%m/%Y")) == FALSE) {
                stop(paste0("Upper: ", upper_field_name, ": record ID ", upper_event_data$record_id, " is not a valid date"))
              }
              
              invalid_dates <- which(is.na(as.Date(upper_event_data[[upper_field_name]], format = "%d/%m/%Y")))
              if (length(invalid_dates) > 0) {
                stop(paste0("Invalid date strings found at record_ids: ", 
                            paste(upper_event_data$record_id[invalid_dates], collapse = ", "), "; field_name: ", upper_field_name, 
                            "; event_name: ", upper_event_name, "; range_check_type: upper \n"))
              }
              
              
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