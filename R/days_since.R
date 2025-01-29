# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Creates a new column in the dataset, which is the time difference (in days) between two given dates.
#' 
#' @param dataset Dataset to be assessed.
#' @param date_column Later date column, which will be deleted and replaced by a number of days
#' @param start_date Start date column, for example baseline, which will act as day 0.
#'
#' @return A dataset with a date column in days rather than in date format
#' 
#' @examples:
#' Example usage:
#' dataset_clean <- days_since(dataset, week_1_dat, baseline_dat)
#' @export
#'

days_since <- function(dataset, date_column, start_date) {
  # Ensure the date_column is in Date format
  dataset[[date_column]] <- as.Date(dataset[[date_column]])
  dataset[[start_date]] <- as.Date(dataset[[start_date]])
  
  # Calculate the days since the given date
  dataset[[date_column]] <- as.numeric(dataset[[date_column]],dataset[[start_date]], units = "days")
  
  return(dataset)
}