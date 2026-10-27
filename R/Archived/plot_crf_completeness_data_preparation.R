# Author: Paigan Aspinall
# Date: 23JAN2024
# R version: 4.2.2
#' Generate a dataset of timepoint completeness to create a completeness plot.
#'
#' This function produces a dataset which gives the completeness of each defined timepoint for each participant.
#'
#' @param data A complete dataset.
#' @param timepoint_names A vector containing the list of timepoint names.
#' @param category The category by which data will be organised, must be "Site" or "Allocation".
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A dataset summarising event completeness.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' timepoint_names <- c("week_0_blinded_ass_arm_1", "week_2_blinded_ass_arm_1", "week_6_blinded_ass_arm_1", "week_8_blinded_as_arm_1", "week_22_blinded_as_arm_1")
#' 
#' plot_crf_completeness_dataset_preparation <- function(dataset, timepoint_names, "Allocation", your_api_token, test=FALSE)
#'
#' @export
#'

plot_crf_completeness_dataset_preparation <- function(dataset, timepoint_names, category, api_token, test=FALSE){
  if(is.null(category)) {
    stop("Category not provided!")
  } else if(!is.null(category)) {
    if (!(category %in% c('Allocation', 'Site'))) {
      stop("category must be 'Allocation' or 'Site'")
    }
    
    if(is.null(timepoint_names)) {
      stop("Timepoint names not provided!")
    }
    
    if(is.null(dataset)) {
      stop("Dataset not provided!")
    }
    
    if(is.null(api_token)) {
      stop("API Token not provided!")
    }
    
    #select the correct URL for the test or live database
    if(test){
      url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
    } else{
      url <- "https://clinicaltrials.plymouth.ac.uk/api/"
    }
    
    #load the instrument data
    formData <- list("token"=api_token,
                     content='instrument',
                     format='csv',
                     returnFormat='csv'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    crf_data <- httr::content(response)
    
    #load the instrument-event mapping data
    formData <- list("token"=api_token,
                     content='formEventMapping',
                     format='csv',
                     'arms[0]'='1',
                     returnFormat='csv'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    event_instrument_mapping <- httr::content(response)
    names(event_instrument_mapping)[names(event_instrument_mapping) == "form"] <- "instrument_name"
    all_event_data <- merge(event_instrument_mapping, crf_data, by = "instrument_name", all.x = TRUE)
    
    #add completeness column names to the dataset
    all_event_data <- all_event_data %>%
      mutate(completeness_column_name = paste0(instrument_name, "_complete"))
    
    #load event data from API
    formData <- list("token"=api_token,
                     content='event',
                     format='csv',
                     returnFormat='csv'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    event_data <- httr::content(response)
    
    all_event_data <- merge(all_event_data, event_data, by = "unique_event_name", all.x = TRUE)
    
    #subset the dataset so that only the record_id, category and required completeness columns are present
    table_dataset <- dataset %>%
      select(c(category,  "redcap_event_name", all_event_data$completeness_column_name))  
    
    result_list <- list()
    for (timepoint_name in timepoint_names) {
      subset_rows <- all_event_data[all_event_data$unique_event_name == timepoint_name, ]
      
      completeness_columns <- subset_rows$completeness_column_name
      
      subset_table <- table_dataset[table_dataset$redcap_event_name == timepoint_name, ]
      completeness_values <- apply(subset_table[, completeness_columns], 1, calculate_completeness)
      
      subset_table$timepoint_completeness <- completeness_values
      
      result_list[[timepoint_name]] <- subset_table
    }
    
    final_dataset <- do.call(rbind, result_list)
    
    names(event_data)[names(event_data) == "unique_event_name"] <- "redcap_event_name"
    final_dataset <- merge(final_dataset, event_data, by = "redcap_event_name", all.x = FALSE)
    selected_columns <- c(category, "event_name", "timepoint_completeness")
    final_dataset <- final_dataset %>%
      select(all_of(selected_columns))  
  }
}
