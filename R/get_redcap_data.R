library(dplyr)
library(tidyr)
library(httr)

get_redcap_data <- function(api_token, test = FALSE, label = TRUE){
  
  ## Set URL based on environment
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }
  
  ## Get form data
  record_request <- list("token"=api_token,
                   content='record',
                   action='export',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='true',
                   returnFormat='csv'
  )
  
  record_response <- httr::POST(url, body = record_request, encode = "form")
  record_data <- httr::content(record_response, show_col_types = FALSE)
  
  ##Getting the participant record metadata from REDCap
  
  ## If labelling call API again for metadata 
  if(label){
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
      var_lab(record_data[[each_name]]) <- label_names[[each_name]]
    }
  }
  
  return(record_data)
}



