#Title: Z-scores Function
#Author: Paigan Aspinall
#Version & Date: V1.1.0 12AUG2026
#R version: 4.4.3

#' Calculate site-level Z-scores for required numeric variables
#'
#' This function calculates site-level Z-scores for required continuous and
#' derived numeric variables. Z-scores compare each site's mean with the overall
#' study mean, standardised by the overall standard deviation.
#'
#' Non-numeric variables, including Date variables, are excluded automatically.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param variable_type_column Column name in metadata containing variable type.
#' @param required_column Column name in metadata indicating whether the field is required.
#' @param field_name_column Column name in metadata containing REDCap field names.
#' @param site_column Column name in dataset representing site/group.
#' @param small_n_threshold Minimum site-level n before results are considered meaningful.
#' @param moderate_threshold Absolute Z-score threshold for moderate flags.
#' @param large_threshold Absolute Z-score threshold for large flags.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{z_scores}: all calculated Z-scores
#'   \item \code{z_flags}: meaningful moderate/large Z-score flags
#'   \item \code{excluded_variables}: eligible metadata variables excluded because
#'   they were absent from the dataset or were not numeric
#' }
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' z_score_outputs <- z_score_analysis(data, metadata)
#' z_scores <- z_score_outputs$z_scores
#' z_flags <- z_score_outputs$z_flags
#'
#' @export

z_score_analysis <- function(data,
                             metadata,
                             variable_type_column = "variable_type",
                             required_column = "required_yn",
                             field_name_column = "field_name",
                             site_column = "redcap_data_access_group",
                             small_n_threshold = 5,
                             moderate_threshold = 2,
                             large_threshold = 3) {
  
  # Identify variables requested by the metadata
  requested_vars <- metadata %>%
    dplyr::filter(
      .data[[variable_type_column]] %in% c("continuous", "derived"),
      tolower(.data[[required_column]]) == "y"
    ) %>%
    dplyr::pull(.data[[field_name_column]]) %>%
    unique()
  
  # Identify variables that are actually present in the dataset
  present_vars <- requested_vars[requested_vars %in% names(data)]
  
  # Retain numeric variables only
  vars_for_summary <- present_vars[
    vapply(data[present_vars], is.numeric, logical(1))
  ]
  
  # Record excluded variables
  excluded_variables <- data.frame(
    variable = requested_vars,
    reason = dplyr::case_when(
      !requested_vars %in% names(data) ~ "Not present in dataset",
      requested_vars %in% present_vars &
        !requested_vars %in% vars_for_summary ~ "Not numeric",
      TRUE ~ NA_character_
    )
  ) %>%
    dplyr::filter(!is.na(reason))
  
  # Stop if there are no eligible variables
  if (length(vars_for_summary) == 0) {
    stop(
      "No eligible numeric continuous or derived variables were found."
    )
  }
  
  # Check site column exists
  if (!site_column %in% names(data)) {
    stop(
      paste0(
        "Site column '",
        site_column,
        "' was not found in the dataset."
      )
    )
  }
  
  # Calculate overall means and SDs
  overall_stats <- data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars_for_summary),
        list(
          mean = ~ {
            x <- .x[!is.na(.x)]
            if (length(x) == 0) NA_real_ else mean(x)
          },
          sd = ~ {
            x <- .x[!is.na(.x)]
            if (length(x) < 2) NA_real_ else stats::sd(x)
          }
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
  # Calculate site-level means
  site_means <- data %>%
    dplyr::group_by(.data[[site_column]]) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars_for_summary),
        ~ {
          x <- .x[!is.na(.x)]
          if (length(x) == 0) NA_real_ else mean(x)
        },
        .names = "{.col}"
      ),
      .groups = "drop"
    )
  
  # Convert site means to long format
  site_long <- site_means %>%
    tidyr::pivot_longer(
      -dplyr::all_of(site_column),
      names_to = "variable",
      values_to = "site_mean"
    )
  
  # Convert overall statistics to long format
  overall_long <- overall_stats %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.+)_(mean|sd)$"
    )
  
  # Calculate Z-scores
  z_scores <- site_long %>%
    dplyr::left_join(
      overall_long,
      by = "variable"
    ) %>%
    dplyr::mutate(
      z_score = dplyr::if_else(
        !is.na(sd) & sd != 0,
        (site_mean - mean) / sd,
        NA_real_
      )
    )
  
  # Calculate site-level non-missing n
  site_n <- data %>%
    dplyr::group_by(.data[[site_column]]) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars_for_summary),
        ~ sum(!is.na(.x)),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      -dplyr::all_of(site_column),
      names_to = "variable",
      values_to = "n"
    )
  
  # Add flags
  z_scores <- z_scores %>%
    dplyr::left_join(
      site_n,
      by = c(site_column, "variable")
    ) %>%
    dplyr::mutate(
      flag_large = !is.na(z_score) &
        abs(z_score) > large_threshold,
      
      flag_moderate = !is.na(z_score) &
        abs(z_score) > moderate_threshold,
      
      small_n_flag = n < small_n_threshold,
      
      invalid_flag = is.na(sd) | sd == 0
    )
  
  # Retain meaningful moderate/large flags
  z_flags <- z_scores %>%
    dplyr::filter(
      !small_n_flag,
      !invalid_flag,
      flag_moderate
    )
  
  return(
    list(
      z_scores = z_scores,
      z_flags = z_flags,
      excluded_variables = excluded_variables
    )
  )
}