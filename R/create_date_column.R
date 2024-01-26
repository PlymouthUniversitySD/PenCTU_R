# Author: Paigan Aspinall
# Date: 26JAN2024
# R version: 4.2.2
#' Takes partial date fields and creates a single date field
#'
#' This function takes the fields used to make a partial date field and combines them to give a field in date format
#'
#' @param dataset The dataset containing the date fields.
#' @param year_column The column where the year is specified.
#' @param month_column The column where the month is specified.
#' @param day_colummns A vector where the day columns are specified.
#' @param new_date_column The name of the new column containing the date.
#' @param is_month_numeric Logical, allows the user to specify if the month is given numerically (TRUE) or as words (e.g. June; FALSE). Default is FALSE.
#'
#' @return A column containing the combined date.
#'
#'
#'
#' @examples
#' # Example usage:
#' day_columns <- c("pdday1", "pdday2", "pdday2", "pdday4")
#' dataset <- create_date_column(dataset, "pdyear", "pdmonth", day_columns, "pddat", is_month_numeric = FALSE)
#'
#' @export
#' 

create_date_column <- function(dataset, year_column, month_column, day_columns, new_date_column, is_month_numeric = FALSE) {
  #create a copy of the dataset
  dataset_copy <- dataset
  
  #replace unknown values in day and month columns
  dataset_copy[, day_columns] <- lapply(dataset_copy[, day_columns], function(x) {
    ifelse(x == 'unknown', '01', x)
  })
  
  if (!is_month_numeric) {
    dataset_copy[[month_column]] <- match(tolower(dataset_copy[[month_column]]), tolower(month.name))
  } else {
    dataset_copy[[month_column]] <- sprintf("%02d", as.integer(dataset_copy[[month_column]]))
  }
  
  #combine year, month, and day columns into a single date column in the copied dataset
  date_components <- paste(dataset_copy[[year_column]], dataset_copy[[month_column]], apply(dataset_copy[day_columns], 1, function(x) max(as.numeric(x), na.rm = TRUE)), sep = "-")
  date_column <- as.Date(date_components, "%Y-%m-%d")
  
  dataset_copy[[new_date_column]] <- date_column
  
  return(dataset_copy)
}