# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Categorise age data in a dataset into age ranges (for anonymisation) from a date of birth column
#' 
#' @param dataset Dataset containing the age field.
#' @param age_column Name of date of birth column
#' @param breaks Size of age category brackets.
#' @param labels Labels of age categories.

#' @return an age column with categorised ages.
#' 
#' @examples:
#' Example usage:
#' 
#' breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, 99)
#' labels <- c("0-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-99")
#' anonymised_dataset <- categorise_ages(dataset, dmdob, breaks, labels)
#' @export
#'


categorise_age_from_dob <- function(dataset, dob_column, breaks, labels) {
  
  # Ensure the DOB column is in Date format
  dataset[[dob_column]] <- as.Date(dataset[[dob_column]])
  
  # Calculate age in years using the current date
  dataset$Age <- as.numeric(difftime(Sys.Date(), dataset[[dob_column]], units = "days")) %/% 365
  
  # Categorise ages
  dataset$AgeCategory <- cut(
    dataset$Age, 
    breaks = breaks, 
    labels = labels, 
    right = FALSE, 
    include.lowest = TRUE
  )
  
  # Remove DateOfBirth and Age columns
  dataset <- dataset %>% select(-all_of(c(dob_column, "Age")))
  
  return(data)
}