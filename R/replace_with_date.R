# Author: Paigan Aspinall
# Date & version: 14FEB2024 V1.0.0
# R version: 4.2.2

#' Replace anchor_date with value from corresponding column
#' 
#' This function takes a row of data and replaces the value of the 'anchor_date'
#' column with the value from another column specified by the value in the 
#' 'anchor_date' column itself.
#' 
#' @param row A named list or data frame row containing the data.
#' 
#' @return The modified row with the 'anchor_date' column replaced if specified.

replace_with_date <- function(row) {
  if(is.null(row)) {
    stop("Row not provided!")
  }
  # Extract the anchor_date column value and convert it to character
  anchor_date_col <- as.character(row[['anchor_date']])
  
  # Check if the anchor_date column is not NA and exists as a column name in the row
  if (!is.na(anchor_date_col) && anchor_date_col %in% names(row)) {
    # Replace the value of the anchor_date column with the value from the corresponding column
    row[['anchor_date']] <- row[[anchor_date_col]]
  }
  
  # Return the modified row
  return(row)
}
