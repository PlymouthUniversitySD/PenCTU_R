#' Get data from REDCap using an API token
#'
#' This function retrieves data from REDCap using the REDCap API and an API token.
#'
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#' @param labelVariables Logical, indicating whether to label variables (default is TRUE).
#' @param labelValues Logical, indicating whether to label values (default is FALSE).
#'
#' @return A data frame containing the requested data.
#'
#' @details This function sends an HTTP POST request to the REDCap API to retrieve data.
#' Depending on the parameters, it can label both variables and values based on metadata.
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import expss
#'
#' @examples
#' # Example usage:
#' data <- get_redcap_data(api_token = "your_api_token")
#'
#' @export
#' 
get_redcap_data <- function(api_token, test = FALSE, labelVariables = TRUE, labelValues = FALSE){
  
  ## Set URL based on environment
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }
  
  ## 
  if(labelValues){
    rawOrLabelValues <- 'label'
  }else {
    rawOrLabelValues <- 'raw'
  }
  
  ## Get form data
  record_request <- list("token"=api_token,
                         content='record',
                         action='export',
                         format='csv',
                         type='flat',
                         csvDelimiter='',
                         rawOrLabel= rawOrLabelValues,
                         rawOrLabelHeaders='raw',
                         exportCheckboxLabel='false',
                         exportSurveyFields='false',
                         exportDataAccessGroups='true',
                         returnFormat='csv'
  )
  
  record_response <- httr::POST(url, body = record_request, encode = "form")
  record_data <- httr::content(record_response, show_col_types = FALSE)
  
  ##Getting the participant record metadata from REDCap
  
  ## If labelling variables call API again for metadata 
  if(labelVariables){
    metadata_request <- list(token=api_token,
                             content='metadata',
                             format='csv',
                             returnFormat='csv'
    )
    metadata_response <- httr::POST(url, body = metadata_request, encode = "form")
    metadata <- httr::content(metadata_response, show_col_types = FALSE) %>%
      dplyr::select(field_name, field_label) %>%
      tidyr::pivot_wider(names_from = field_name, values_from = field_label)
    
    ## Find matching labels and remove any html tags and colons
    
    remove_html <- function(htmlString) {
      return(sub("[*|:]*$", "", gsub("<.*?>", "", htmlString)))
    }
    
    label_names <- lapply(as.list(metadata[1, intersect(names(record_data), names(metadata)), with=FALSE]), remove_html)
    
    for (each_name in names(record_data)) {
      expss::var_lab(record_data[[each_name]]) <- label_names[[each_name]]
    }
  }
  
  return(record_data)
}