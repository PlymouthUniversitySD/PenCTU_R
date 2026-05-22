#Title: Missing Data Function
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24APR2026
#R version: 4.4.3

#' Calculate site-level missing data rates for required fields
#'
#' This function calculates missing data rates by site for required fields,
#' using expected event definitions from the metadata and excluding repeating instances.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#'
#' @return A dataframe containing missing counts and proportions by site and variable.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' missing_results <- missing_data_analysis(data, metadata)
#'
#' @export
#'
missing_data_analysis <- function(data, metadata) {
  
  # Prepare metadata (expand events + keep required only)
  metadata_events <- metadata %>%
    tidyr::separate_rows(event_names, sep = ";") %>%
    dplyr::mutate(
      event_names = stringr::str_trim(event_names),
      required_yn = tolower(required_yn)
    ) %>%
    dplyr::filter(required_yn == "y")
  
  required_vars <- unique(metadata_events$field_name)
  required_vars <- required_vars[required_vars %in% names(data)]
  
  missing_results <- list()
  
  for (var in required_vars) {
    
    expected_events <- metadata_events %>%
      dplyr::filter(field_name == var) %>%
      dplyr::pull(event_names) %>%
      unique()
    
    if (length(expected_events) == 0) next
    
    df <- data %>%
      dplyr::filter(
        redcap_event_name %in% expected_events,
        is.na(redcap_repeat_instance)
      )
    
    if (nrow(df) == 0) next
    
    site_missing <- df %>%
      dplyr::group_by(redcap_data_access_group) %>%
      dplyr::summarise(
        n_expected = dplyr::n(),
        missing_n = sum(is.na(.data[[var]])),
        missing_prop = missing_n / n_expected,
        .groups = "drop"
      ) %>%
      dplyr::mutate(variable = var)
    
    missing_results[[var]] <- site_missing
  }
  
  dplyr::bind_rows(missing_results)
}