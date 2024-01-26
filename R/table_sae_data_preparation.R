# Author: Paigan Aspinall
# Date: 26JAN2024
# R version: 4.2.2
#' Produce a dataset of SAEs ready for use in a flextable
#'
#' This function cleans the SAE data and produces a dataframe.
#'
#' @param dataset A REDCap dataset.
#' @param sae_event_name Name of the SAE event.
#' @param sae_date_column Column where the date of the SAE is recorded.
#' @param sae_description_column Column where the SAE is described.
#' @param classification_columns A vector of the SAE classification columns.
#' @param classification_text A vector of the classification descriptions, in the order they appear in the classification_columns vector.
#' @param report_outcome_column Column where the outcome at the time of reporting is recorded.
#' @param resolution_outcome_column Column where the outcome at the time of resolution is recorded.
#' @param outcome_date_columns A vector of column names where the outcome date(s) are recorded.
#' @param severity_column Column where the severity of the SAE is recorded.
#' @param expectedness_column Column where the expectedness of the SAE is recorded.
#' @param relatedness_column Column where the relatedness of the SAE is recorded.
#' @param allocation Logical, determines whether allocation is included in the table (default is TRUE).
#' @param previous_report_date Date from which the SAEs will be reported in the table (default is NULL).
#'
#' @return A dataset containing SAE data.
#'
#' @import dplyr
#' @import httr
#'
#' @examples
#' # Example usage:
#' 
#' classification_columns <- c("aeclass___1", "aeclass___2", "aeclass___3", "aeclass___4", "aeclass___5")
#' classification_text <- c("Resulted in death", "Life-threatening event", "Prolonged/required hospitalisation", "Persistent/significant disability/incapacity", "Significant or important medical event")
#' outcome_date_columns <- c("aerecover1dat", "aedeath1dat", "aerecover2dat", "aedeath2dat")
#' 
#' sae_table <- table_sae_data_preparation(your_dataset, "sae_reporting_arm_1", "aeonsetdat", "aedescribeft", classification_columns, 
#'                                          classification_text, "aeoutcome1", "aeoutcome2", outcome_date_columns, "aepiseverity",
#'                                          "aeciexpect", "aepitrialyn", allocation = TRUE, "2023-09-01")
#'
#' @export
#' 

table_sae_data_preparation <- function(dataset, sae_event_name, sae_date_column, sae_description_column, classification_columns, classification_text, report_outcome_column, 
                                       resolution_outcome_column, outcome_date_columns, severity_column, expectedness_column, relatedness_column,
                                       allocation=TRUE, previous_report_date = NULL){

  #extract the SAE data
  sae_data <- subset(dataset, redcap_event_name == sae_event_name)
  if(nrow(sae_data > 0)){
    
    #move all final outcomes to a single column and encode them
    sae_data[[report_outcome_column]]<- as.character(sae_data[[report_outcome_column]])
    sae_data[[resolution_outcome_column]]<- as.character(sae_data[[resolution_outcome_column]])
    sae_data$outcome <- ifelse((sae_data[[report_outcome_column]] != "Ongoing" & !is.null( sae_data[[resolution_outcome_column]])), sae_data[[report_outcome_column]], sae_data[[resolution_outcome_column]])
    sae_data$outcome <- ifelse(is.na(sae_data$outcome), 'Unresolved', sae_data$outcome)

    #convert outcome date columns into date format
    sae_data[outcome_date_columns] <- lapply(sae_data[outcome_date_columns], ymd)
      
    #populate an outcome date column with the outcome date given accross the date fields
    sae_data <- sae_data %>%
      rowwise() %>%
      mutate(outcome_date = {
        date_columns <- c_across(all_of(outcome_date_columns))
        first_non_na_date <- na.omit(date_columns)[1]
        if (!is.na(first_non_na_date)) {
          as.Date(first_non_na_date, format = "%Y-%m-%d")
        } else {
          NA
        }
      }) %>%
      ungroup()
  
    #create a function to replace values in the classification columns
    replace_values <- function(data, col_names, text_values) {
      for (i in seq_along(col_names)) {
        data[[col_names[i]]] <- ifelse(data[[col_names[i]]] == 1, text_values[i], "")
      }
      return(data)
    }
  
    #replace classification values using the function
    sae_data <- replace_values(sae_data, classification_columns, classification_text)
    
    #colate the classifications into a single column
    sae_data$classification <- apply(sae_data[, classification_columns], 1, function(row) {
      non_empty_values <- row[row != ""]
      paste(non_empty_values, collapse = ", ")
    })

    #if a previous report date is being used, remove rows where the outcome date is before the specified date
    if(!is.null(previous_report_date)){
      previous_report_date <- as.Date(previous_report_date )
      sae_data <- sae_data %>%
        filter(outcome_date > previous_report_date)
    }
    
    #if allocation is being included select required columns including allocation
    if(allocation){
      sae_data <- select(sae_data, record_id, Site, Allocation, sae_date_column, sae_description_column, outcome, outcome_date, classification, severity_column, relatedness_column, expectedness_column)  
    } else {
      #if allocation is not being included select required columns excluding allocation
      sae_data <- select(sae_data, record_id, Site, sae_date_column, sae_description_column, outcome, outcome_date, classification, severity_column, relatedness_column, expectedness_column)  
    }
    
    #rename the columns for final table
    names(sae_data)[names(sae_data) == "record_id"] <- "Participant ID"
    names(sae_data)[names(sae_data) == sae_date_column] <- "Onset date"
    names(sae_data)[names(sae_data) == sae_description_column] <- "Description"
    names(sae_data)[names(sae_data) == "outcome"] <- "Final outcome"
    names(sae_data)[names(sae_data) == "outcome_date"] <- "Outcome date"
    names(sae_data)[names(sae_data) == "classification"] <- "Classification(s)"
    names(sae_data)[names(sae_data) == severity_column] <- "Severity"
    names(sae_data)[names(sae_data) == relatedness_column] <- "Related to trial procedure?"
    names(sae_data)[names(sae_data) == expectedness_column] <- "Expectedness"
    
    return(sae_data)
  } else{
    #if no SAE data is present then print that no SAEs have been reported
    print("No SAEs have been reported in the study to date.")
  }
}
