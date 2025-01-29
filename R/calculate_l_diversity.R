# Author: Ryan Ashford
# Date: 29JAN2025
# R version: 4.4.2

#' Calculates the l-diversity score for a dataset based on quasi-identifiers and sensitive attributes.
#' 
#' @param dataset Dataset to be assessed.
#' @param quasi_identifiers Vector defining the quasi-identifiers in the dataset
#' @param sensitive_attributes Vector defining all of the sensitive attributes in a dataset.

#' @return A a-diversity score
#' 
#' @examples:
#' Example usage:
#' 
#' quasi_identifiers <- c("age", "place_of_birth", "gender")
#' sensitive_attributes <- c("conviction_reason", "suicidal_thoughts") 
#' l_diversity_value <- calculate_l_diversity(dataset, quasi_identifiers, sensitive_attributes)
#' 
#' @export

calculate_l_diversity <- function(dataset, quasi_identifiers, sensitive_attributes) {
  l_diversity <- dataset %>%
    group_by(across(all_of(quasi_identifiers))) %>%
    summarise(distinct_sensitive_values = n_distinct(.data[[sensitive_attributes]]), .groups = 'drop') %>%
    summarise(min_diversity = min(distinct_sensitive_values)) %>%
    pull(min_diversity)
  return(l_diversity)
}