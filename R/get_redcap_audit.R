# Author: Paigan Aspinall
# Date: 09JAN2024
# R version: 4.2.2

#' Import the REDCap audit log data using API token.
#'
#' This function retrieves audit log data from REDCap using the REDCap API and an API token.
#' 
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A dataframe containing the requested data.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example useage:
#' 
#' audit_data <- get_redcap_audit("your_api_token", test=TRUE)
#'
#' @export
#'

get_redcap_audit <- function(api_token, test = FALSE){
  
  #set URL based on environment
    if(test){
      url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
    } else{
      url <- "https://clinicaltrials.plymouth.ac.uk/api/"
    }
    
  #get audit log data
  formData <- list("token"=token,
                 content='log',
                 logtype='',
                 user='',
                 record='',
                 beginTime='2023-06-21 12:18',
                 endTime='',
                 format='csv',
                 returnFormat='csv'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  return(result)
}
