# Author: Paigan Aspinall
# Date: 10OCT2024
# R version: 4.2.2
#' Generate a table of CRF completeness at a specified time point.
#'
#' This function produces a flextable that summarises the number of complete and incomplete CRFs at a defined timepoint.
#'
#' @param data A complete dataset.
#' @param timepoint_name The name of the timepoint for which we are measuring CRF completeness, e.g. "facetoface_screeni_arm_1".
#' @param category The category by which data will be organised, must be "Site" or "Allocation".
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A flextable summarising CRF completeness.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' timepoint_completeness_table <- table_crf_completeness(dataset, "facetoface_screeni_arm_1", "Site", your_api_token, test=FALSE)
#'
#' @export
#'


table_crf_completeness <- function(dataset, timepoint_name, category, api_token, test=FALSE){
  
  #run the data preparation function
  timepoint_dataset <- table_crf_completeness_data_preparation(dataset, timepoint_name, category, api_token, test)
  
  #select the correct URL for the test or live database
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }
  
  #load event data from API
  formData <- list("token"=token,
                   content='event',
                   format='csv',
                   returnFormat='csv'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  event_data <- httr::content(response)
  
  #isolate the rows in the event data for the timepoint of interest
  event_data <- subset(event_data , unique_event_name == timepoint_name)
  
  #define the table name
  timepoint_label <- event_data$event_name
  tablename <- paste0("**", timepoint_label, " CRF completeness**")
  
  #define the crf list
  crf_names <- names(timepoint_dataset)
  crf_names <- crf_names[-1]
  
  timepoint_dataset <- timepoint_dataset %>%
    mutate(across(where(is.character), ~ replace_na(., "Not applicable")),
           across(where(is.factor), ~ fct_explicit_na(., na_level = "Not applicable")))
  
  completeness_table <- timepoint_dataset %>%
    gtsummary::tbl_summary(by = category,
                           type = c(crf_names) ~ "categorical",
                           statistic = all_categorical() ~ "{n} / {N} ({p}%)",
                           missing='no')%>%
    italicize_levels() %>%
    bold_labels() %>%
    modify_caption(tablename) %>%
    modify_header(label ~ "**CRF**") %>%
    as_flex_table()
}
  
