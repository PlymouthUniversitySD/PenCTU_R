# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Removed multiple columns at a time from a dataset (for anonymisation).
#' 
#' @param dataset Dataset to beanonymised
#' @param fields_to_remove Vector defining the columns to be removed

#' @return A dataset with defined columns removed
#' 
#' @examples:
#' Example usage:
#' 
#' fields_to_remove <- c("ID", "DateOfBirth")
#' data_clean <- remove_fields(dataset, fields_to_remove)
#' @export
#'

remove_fields <- function(dataset, fields_to_remove) {
  dataset <- dataset %>% select(-all_of(fields_to_remove))
  return(dataset)
}
