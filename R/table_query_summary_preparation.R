# Author: Paigan Aspinall
# Date: 12JAN2024
# R version: 4.2.2

#' Produce a query dataset suitable for downstream use in flex tables and in con junction with plot_query_data
#'
#' This function cleans the audit data from REDCap for downstream use.
#' 
#' @param query_data REDCap export of data resolution workflow.
#' @param DAGs Logical, to specify whether DAGs are used in the study (default is TRUE).
#' @param start_date Used to specify the date from which queries should be included in the output (default is NULL).
#'
#' @return A data frame containing the cleaned query data.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example usage:
#' 
#' query_data <- read.csv("DataResolutionDashboard.csv")
#' 
#' queries <- table_query_summary_preparation(query_data, DAGs=TRUE, start_date = "2023-09-01")
#'
#' @export
#'

table_query_summary_preparation <- function(query_data, DAGs=TRUE, start_date = NULL){
  #if there is data present in the query file perform the following
  if (nrow(query_data) > 0) {
    if(DAGs){
      #select the required columns
      queries <- select(query_data, Time.Raised, Record..Sorted.by.DAG., Data.Access.Group, Current.Query.Status, Event, Field, Days.Open, First.Update, Last.Update)
      #formal the updates columns to remove email addresses, dates and unnecessary punctuation
      queries$First.Update <- sub(".*: ", "", queries$First.Update)
      queries$First.Update <- gsub("^\"|\"$", "", queries$First.Update)
      queries$Last.Update <- sub(".*: ", "", queries$Last.Update)    
      queries$Last.Update <- gsub("\\[.*?\\]", "", queries$Last.Update)
      queries$Last.Update <- gsub("^\"|\"$", "", queries$Last.Update)
      #round the number of days open down to the nearest integer
      queries$Days.Open <- as.numeric(queries$Days.Open)
      queries$Days.Open <- floor(queries$Days.Open)
      #include only the field name not the label
      queries$Field <- sub(" \\(.*", "", queries$Field)
      #update the date raised to include only date not time
      queries$Time.Raised <- substr(queries$Time.Raised, 1, 10)
      queries$Time.Raised <- as.Date(queries$Time.Raised)
      #sort queries by date (newest to oldest)
      queries <- queries[order(queries$Time.Raised, decreasing = TRUE), ]
      #rename the fields
      names(queries)[names(queries) == "Current.Query.Status"] <- "Status"
      names(queries)[names(queries) == "Record..Sorted.by.DAG."] <- "Record"
      names(queries)[names(queries) == "Data.Access.Group"] <- "Site"
      names(queries)[names(queries) == "Days.Open"] <- "Days open"
      names(queries)[names(queries) == "First.Update"] <- "First update"
      names(queries)[names(queries) == "Last.Update"] <- "Last update"
      names(queries)[names(queries) == "Time.Raised"] <- "Date"
    } else {
      #select the required columns
      queries <- select(query_data, Time.Raised, Record, Current.Query.Status, Event, Field, Days.Open, First.Update, Last.Update)
      #formal the updates columns to remove email addresses, dates and unnecessary punctuation
      queries$First.Update <- sub(".*: ", "", queries$First.Update)
      queries$First.Update <- gsub("^\"|\"$", "", queries$First.Update)
      queries$Last.Update <- sub(".*: ", "", queries$Last.Update)    
      queries$Last.Update <- gsub("\\[.*?\\]", "", queries$Last.Update)
      queries$Last.Update <- gsub("^\"|\"$", "", queries$Last.Update)
      #round the number of days open down to the nearest integer
      queries$Days.Open <- as.numeric(queries$Days.Open)
      queries$Days.Open <- floor(queries$Days.Open)
      #include only the field name not the label
      queries$Field <- sub(" \\(.*", "", queries$Field)
      #update the date raised to include only date not time
      queries$Time.Raised <- substr(queries$Time.Raised, 1, 10)
      queries$Time.Raised <- as.Date(queries$Time.Raised)
      #sort queries by date (newest to oldest)
      queries <- queries[order(queries$Time.Raised, decreasing = TRUE), ]
      #rename the fields
      names(queries)[names(queries) == "Current.Query.Status"] <- "Status"
      names(queries)[names(queries) == "Record"] <- "Record"
      names(queries)[names(queries) == "Days.Open"] <- "Days open"
      names(queries)[names(queries) == "First.Update"] <- "First update"
      names(queries)[names(queries) == "Last.Update"] <- "Last update"
      names(queries)[names(queries) == "Time.Raised"] <- "Date"
    }
    if(!is.na(start_date)){
      #if start date is being used subset data to include only those with Date after start date
      start_date <- as.Date(start_date)
      queries <- subset(queries, Date > start_date)
      if (nrow(query_data) == 0){
        #if there is no start date after filtering by date print below message
        print("No queries reported since start date.")
      }
    }
    return(queries)
  } else {
    #if there is no data in the query file, return the message
    print("No queries reported to date.")
  }
}
