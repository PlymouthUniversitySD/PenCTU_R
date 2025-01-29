# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Categorise age data in a dataset into age ranges (for anonymisation)
#' 
#' @param dataset Dataset containing the age field.
#' @param age_column Name of age column to be categorised.
#' @param breaks Size of age category brackets.
#' @param labels Labels of age categories.

#' @return an age column with categorised ages.
#' 
#' @examples:
#' Example usage:
#' 
#' breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, 99)
#' labels <- c("0-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-99")
#' anonymised_dataset <- categorise_ages(dataset, dmage, breaks, labels)
#' @export
#'


categorise_ages <- function(dataset, age_column, breaks, labels){
  
  # Ensure the age column is numeric
  dataset[[age_column]] <- as.numeric(dataset[[age_column]])
  
  # Categorise the ages
  dataset[[age_column]] <- cut(
    dataset[[age_column]], 
    breaks = breaks, 
    labels = labels, 
    right = FALSE, 
    include.lowest = TRUE
  )
  
  return(dataset)
}