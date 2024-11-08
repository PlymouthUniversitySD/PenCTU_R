# Author: Paigan Aspinall
# Date & version: 16FEB2024 V2.1
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
  if(!is.null(dataset)) {
    dataset_copy <- dataset
    
    if(!is.null(day_columns) && !is.null(month_column) && !is.null(year_column)) {
      if(year_column %in% dataset && month_column %in% dataset && any(day_columns) %in% dataset){
        day_values <- rowSums(!is.na(dataset_copy[, day_columns])) > 0
        dataset_copy$day <- NA
        dataset_copy$day[day_values] <- apply(dataset_copy[day_values, day_columns], 1, function(x) x[which.min(is.na(x))])
        
        
        if (!is_month_numeric) {
          dataset_copy$day <- ifelse(dataset_copy$day == '999', '01', dataset_copy$day)
          
          dataset_copy <- dataset_copy %>%
            mutate(month = case_when(
              !!sym(month_column) == 'Unknown' ~ "January",
              is.na(!!sym(month_column)) ~ NA_character_,
              TRUE ~ as.character(!!sym(month_column))
            ))
        } else {
          
          dataset_copy$day <- ifelse(dataset_copy$day == '999', '01', dataset_copy$day)
          
          
          dataset_copy <- dataset_copy %>%
            mutate(month = case_when(
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 1 ~ "January",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 2 ~ "February",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 3 ~ "March",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 4 ~ "April",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 5 ~ "May",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 6 ~ "June",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 7 ~ "July",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 8 ~ "August",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 9 ~ "September",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 10 ~ "October",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 11 ~ "November",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 12 ~ "December",
              ifelse(is.na(!!sym(month_column)), FALSE, !!sym(month_column)) == 999 ~ "January",
              TRUE ~ NA_character_  # Add this line if you want to handle other cases
            ))
          
        }
        
        #combine year, month, and day columns into a single date column in the copied dataset
        date_components <- paste(dataset_copy[[year_column]], dataset_copy$month, dataset_copy$day, sep = "-")
        date_column <- as.Date(date_components, format = "%Y-%B-%d")
        if(!is.null(new_date_column)){
          dataset_copy[[new_date_column]] <- date_column
          
          return(dataset_copy)
        } else {
          stop("Column name not provided for new_date_column!")
        }
      } else {
        stop("Date columns not present in dataset!")
      }
    } else {
      stop("Date parameters not provided!")
    }
  } else {
    stop("Dataset is null!")
  }
}