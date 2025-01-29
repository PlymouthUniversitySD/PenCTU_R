# Author: Paigan Aspinall
# Date: 23JAN2024
# R version: 4.2.2
#' Add site names to the dataset
#'
#' This function retrieves DAG data from REDcap and adds a Site column to the dataset which contains the DAG name.
#'
#' @param dataset The dataset where the new column is being added.
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A data frame containing the new column.
#'
#' @details This function sends an HTTP POST request to the REDCap API to retrieve data.
#' It will then add items from this REDcap data to the specified dataset.
#'
#' @import dplyr
#' @import httr
#'
#' @examples
#' # Example usage:
#' data <- get_redcap_data(your_dataset, api_token = "your_api_token", test = TRUE)
#'
#' @export
#' 

encode_site_names <- function(dataset, api_token, test = FALSE){
  if(!is.null(dataset)) {
    #set URL based on environment
    if(test){
      url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
    } else{
      url <- "https://clinicaltrials.plymouth.ac.uk/api/"
    }
    
    if(!is.null(api_token)) {
      #get data access group data
      formData <- list("token"=api_token,
                       content='dag',
                       format='csv',
                       returnFormat='csv'
      )
      response <- httr::POST(url, body = formData, encode = "form")
      dag_data <- httr::content(response)
      
      valid_format <- grepl("^[0-9]{2}_{2}.+$", dataset$redcap_data_access_group)
      match <- dataset$redcap_data_access_group %in% dag_data$unique_group_name
      
      if(all(valid_format)) {
        if(all(match)) {
          #if the dag name starts with 'XX - ' where X is a number, this is removed
          dag_data$data_access_group_name <- gsub("^[0-9]+ - ", "", dag_data$data_access_group_name)
          
          #select only required columns from dag data
          dag_data <- select(dag_data, data_access_group_name, unique_group_name)
          
          #add the dag name column to the dataset
          merged_data <- merge(dataset, dag_data, by.x = "redcap_data_access_group", by.y = "unique_group_name", all.x = TRUE)
          
          #rename the dag name column
          names(merged_data)[names(merged_data) == "data_access_group_name"] <- "Site"
          
          return(merged_data)
        } else {
          stop("One of the data access group names provided doesn't match DAG names retrieved from REDCap!")
        }
      } else {
        stop("One of the data access group names provided does not match the format (XX__Site Name)")
      }
    } else {
      stop("API Token needed to retrieve DAG data from REDCap!")
    }
  } else {
    stop("Dataset has not been provided!")
  }
}
  