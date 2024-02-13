# Author: Matthew Bailey
# Date & version: 23JAN2024 V1.1.0
# R version: 4.3.1
#' Get data from REDCap using an API token
#'
#' This function retrieves data from REDCap using the REDCap API and an API token.
#'
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A data frame containing the requested data.
#'
#' @details This function sends an HTTP POST request to the REDCap API to retrieve data.
#' Depending on the parameters, it can label both variables and values based on metadata.
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom stringr str_remove_all str_split_1 str_trim
#' @importFrom expss var_lab
#' @importFrom haven write_dta
#'
#' @examples
#' # Example usage:
#' data <- get_redcap_data(api_token = "your_api_token", test = TRUE)
#'
#' @export
#' 
get_redcap_data <- function(api_token, test = FALSE){
  
  ## Set URL based on environment
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }
  
  ## Get participant CRF data
  record_request <- list("token"=api_token,
                         content='record',
                         action='export',
                         format='csv',
                         type='flat',
                         csvDelimiter='',
                         rawOrLabel= 'raw',
                         rawOrLabelHeaders='raw',
                         exportCheckboxLabel='false',
                         exportSurveyFields='false',
                         exportDataAccessGroups='true',
                         returnFormat='csv'
  )
  
  record_response <- httr::POST(url, body = record_request, encode = "form")
  record_data <- httr::content(record_response, show_col_types = FALSE)
  
  ##LABEL VARIABLES
  ##Get data dictionary
  
  metadata_request <- list(token=api_token,
                           content='metadata',
                           format='csv',
                           returnFormat='csv'
  )
  metadata_response <- httr::POST(url, body = metadata_request, encode = "form")
  
  data_dictionary <- httr::content(metadata_response, show_col_types = FALSE)
  
  # Extract field names and labels from data_dictionary and pivot them into wide format
  variable_labels <- data_dictionary %>%
    dplyr::select(field_name, field_label) %>%
    tidyr::pivot_wider(names_from = field_name, values_from = field_label)
  
  # Function to remove HTML tags, asterisks, vertical bars, and colons from labels
  remove_html <- function(htmlString) {
    return(sub("[*|:]*$", "", gsub("<.*?>", "", htmlString)))
  }
  
  # Apply remove_html function to labels in variable_labels and store the result in label_names
  label_names <- lapply(as.list(variable_labels[1, intersect(names(record_data), names(variable_labels)), with=FALSE]), remove_html)
  
  # Assign labels to variables in record_data using expss::var_lab function
  for (each_name in names(record_data)) {
    expss::var_lab(record_data[[each_name]]) <- label_names[[each_name]]
  }
  
  # Ensure that any empty strings in data_dictionary are replaced with NA
  data_dictionary <<- dplyr::mutate(data_dictionary, across(everything(), ~ ifelse(. == '', NA, .)))
  
  # Loop through each row in data_dictionary and process select_choices_or_calculations
  for (i in 1:nrow(data_dictionary)) {
    var_name <- data_dictionary[["field_name"]][i]
    
    if (!is.null(record_data[[var_name]])) {
      if (!is.na(data_dictionary[["select_choices_or_calculations"]][i]) && 
          data_dictionary[["field_type"]][i] != "calc" && 
          data_dictionary[["field_type"]][i] != "slider") {
        
        # Extract choices and labels, then convert record_data to factors with specified levels and labels
        value <- stringr::str_remove_all(data_dictionary[["select_choices_or_calculations"]][i], '\"') %>% 
          stringr::str_trim()
        
        if (grepl("\\|", value)) {
          value <- stringr::str_split_1(value, ' \\| ') %>% 
            stringr::str_trim() %>% 
            stringr::str_split(", ", n = 2)
          
          levels <- sapply(value, `[[`, 1)
          labels <- sapply(value, `[[`, 2)
          
          record_data[[var_name]] <- factor(record_data[[var_name]], levels = levels, labels = labels)
        } else {
          # Handle the case where there's no delimiter
          record_data[[var_name]] <- factor(record_data[[var_name]], levels = c(value), labels = c(value))
        }
      }
    }
  }
  
  # Return modified record_data
  return(record_data)
}
