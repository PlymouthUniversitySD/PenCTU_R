#' Generate a dataset from RCA data to be used in combination with the table_crf_completeness function. 
#'
#' This function produces datset ready to be processed into a crf completeness by timepoint summary figure.
#'
#' @param data A complete dataset.
#' @param timepoint_name The column name of the timepoint for which we are measuring CRF completeness, e.g. "facetoface_screeni_arm_1".
#' @param category The category by which data will be organised, must be "Site" or "Allocation".
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#' 
#'
#' @return A dataset ready for downstream processing.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom dplyr "%>%"
#'
#'@examples
#' example_dataset<- table_crf_completeness_data_prep(dataset, "facetoface_screeni_arm_1", "Allocation", your_api_token, test=TRUE)

#' 
#' @export
#'

table_crf_completeness_data_prep <- function(dataset, timepoint_name, category, api_token, test=FALSE){
  
  #check a correct category option has been selected
  if (!(category %in% c('Allocation', 'Site'))) {
    stop("category must be 'Allocation' or 'Site'")
  }
  
  #select the correct URL for the test or live database
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }

  #load the instrument data
  formData <- list("token"=token,
                  content='instrument',
                  format='csv',
                  returnFormat='csv'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  crf_data <- httr::content(response)

  #load the instrument-event mapping data
  formData <- list("token"=token,
                  content='formEventMapping',
                  format='csv',
                  'arms[0]'='1',
                  returnFormat='csv'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  event_instrument_mapping <- httr::content(response)
  
  #merge the instrument data with the instrument-event mapping dataset
  names(event_instrument_mapping)[names(event_instrument_mapping) == "form"] <- "instrument_name"
  all_event_data <- merge(event_instrument_mapping, crf_data, by = "instrument_name", all.x = TRUE)
  
  #add completeness column names to the dataset
  all_event_data <- all_event_data %>%
    mutate(completeness_column_name = paste0(instrument_name, "_complete"))
  
  #select only rows where the event name is that which is defined 
  selected_event_data <- subset(all_event_data, unique_event_name == timepoint_name)
  table_dataset <- subset(dataset, redcap_event_name == timepoint_name)

  #subset the dataset so that only the record_id, category and required completeness columns are present
  if(category == "Allocation"){
  selected_columns <- c("Allocation", selected_event_data$completeness_column_name)
  table_dataset <- table_dataset %>%
    select(all_of(selected_columns))  
  }
  if(category == "Site"){
    selected_columns <- c("Site", selected_event_data$completeness_column_name)
    table_dataset <- table_dataset %>%
      select(all_of(selected_columns))  
  }
  
  #Encode completeness values
  crf_complete_columns <- selected_event_data$completeness_column_name
  encode_values_completeness <- function(x) {
    case_when(
      x == 0 ~ 'Incomplete',
      x == 1 ~ 'Unverified',
      x == 2 ~ 'Complete',
      TRUE ~ as.character(x)
    )
  }
  table_dataset <- table_dataset %>%
    mutate_at(vars(crf_complete_columns), ~encode_values_completeness(.))

  #Encode CRF names
  column_name_mapping <- selected_event_data %>%
    select(completeness_column_name, instrument_label)
  table_dataset <- table_dataset %>%
    rename(!!!setNames(as.character(column_name_mapping$completeness_column_name),
                       as.character(column_name_mapping$instrument_label)))
  
  #Return the dataset
  return(table_dataset)
}




