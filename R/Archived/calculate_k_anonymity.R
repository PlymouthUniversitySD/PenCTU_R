# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Calculates the k-anonymity score for a dataset based on quasi-identifiers
#' 
#' @param dataset Dataset to be assessed.
#' @param quasi_identifiers Vector defining the quasi-identifiers in the dataset

#' @return A k-anonymity score
#' 
#' @examples:
#' Example usage:
#' 
#' quasi_identifiers <- c("age", "place_of_birth", "gender")
#' k_anonymity_value <- calculate_k_anonymity(dataset, quasi_identifiers)
#' @export
#'

calculate_k_anonymity <- function(dataset, quasi_identifiers) {
  k_anonymity <- dataset %>%
    group_by(across(all_of(quasi_identifiers))) %>%
    summarise(count = n(), .groups = 'drop') %>%
    summarise(min_count = min(count)) %>%
    pull(min_count)
  return(k_anonymity)
}