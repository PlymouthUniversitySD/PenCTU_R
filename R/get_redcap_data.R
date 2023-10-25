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
  
  variable_labels <- data_dictionary %>%
    dplyr::select(field_name, field_label) %>%
    tidyr::pivot_wider(names_from = field_name, values_from = field_label)
  
 
  ## Find matching labels and remove any html tags and colons
  
  remove_html <- function(htmlString) {
    return(sub("[*|:]*$", "", gsub("<.*?>", "", htmlString)))
  }
  
  label_names <- lapply(as.list(variable_labels[1, intersect(names(record_data), names(variable_labels)), with=FALSE]), remove_html)
  
  for (each_name in names(record_data)) {
    expss::var_lab(record_data[[each_name]]) <- label_names[[each_name]]
  }
  
  ##LABEL VALUES
  data_dictionary <<- dplyr::mutate(data_dictionary, across(everything(), ~ ifelse(. == '', NA, .))) # Ensure blank dictionary items are marked as NA
  
  for (i in 1:nrow(data_dictionary)) {
    var_name <- data_dictionary[["field_name"]][i]
    if (!is.null(record_data[[var_name]])) {
      if ( !is.na(data_dictionary[["select_choices_or_calculations"]][i]) && data_dictionary[["field_type"]][i] != "calc" && data_dictionary[["field_type"]][i] != "slider") {
        value <- stringr::str_remove_all(data_dictionary[["select_choices_or_calculations"]][i], '\"') %>% 
          stringr::str_split_1(' \\| ') %>% 
          stringr::str_trim() %>% 
          stringr::str_split(", ", n = 2)
        
        levels <- sapply(value, `[[`, 1)
        labels <- sapply(value, `[[`, 2)
        
        record_data[[var_name]] <- factor(record_data[[var_name]], levels = levels, labels = labels)
      }
    }
  }
  
  return(record_data)
}