#' Prepare Data for the Completeness Tracker
#'
#' Converts REDCap form-completion data into a site-level summary table suitable
#' for use in the completeness-tracker Excel report.
#'
#' The function identifies all fields ending in \code{"_complete"} and converts
#' them from wide format into a long-form dataset containing one row per record,
#' form, and completion status.
#'
#' Missing completion statuses are classified as \code{"Incomplete"}. Records
#' without an assigned REDCap Data Access Group are classified under the site
#' label \code{"Unknown"}.
#'
#' A complete grid of all expected combinations of form, site, and completion
#' status is then generated. This ensures that combinations with no observed
#' records are retained in the output and assigned a count of zero.
#'
#' The resulting summary table contains:
#'
#' \itemize{
#'   \item One row per form
#'   \item One column for each site and completion-status combination
#'   \item Counts of records within each site-status combination
#' }
#'
#' Output columns are ordered using a site-major, status-minor structure. This
#' means that all status columns for the first site are displayed together,
#' followed by all status columns for the second site, and so on.
#'
#' Form rows are ordered according to \code{form_list}, rather than alphabetically.
#'
#' @param data A data frame containing REDCap form-completion fields. The dataset
#'   must include a \code{redcap_data_access_group} column and one or more fields
#'   whose names end in \code{"_complete"}.
#'
#' @param form_list A character vector defining the forms to include in the
#'   summary and the order in which they should appear.
#'
#' @param sites A character vector defining the sites to include and their
#'   display order. Include \code{"Unknown"} where records without a Data Access
#'   Group should be represented explicitly.
#'
#' @param statuses A character vector defining the completion statuses to
#'   include and their display order. Expected values may include
#'   \code{"Complete"}, \code{"Partially complete"}, and \code{"Incomplete"}.
#'
#' @return A data frame containing one row per form and one column for each
#'   site-status combination. Counts are returned as whole numbers, with zero
#'   used where no records exist for a given combination.
#'
#' @examples
#' completeness_summary <- completeness_tracker_data_prep(
#'   data = week_12_data,
#'   form_list = c(
#'     "baseline_complete",
#'     "follow_up_complete",
#'     "outcome_complete"
#'   ),
#'   sites = c(
#'     "Site 1",
#'     "Site 2",
#'     "Unknown"
#'   ),
#'   statuses = c(
#'     "Complete",
#'     "Partially complete",
#'     "Incomplete"
#'   )
#' )
#'
#' @export

completeness_tracker_data_prep <- function(data, form_list, sites, statuses){
  
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_complete"),
      names_to = "forms",
      values_to = "status"
    ) %>%
    dplyr::mutate(
      status = dplyr::if_else(is.na(status), "Incomplete", as.character(status)),
      redcap_data_access_group = dplyr::if_else(
        is.na(redcap_data_access_group),
        "Unknown",
        as.character(redcap_data_access_group)
      )
    )
  
  # Ensure all combinations exist
  complete_grid <- expand.grid(
    forms = unique(form_list),
    redcap_data_access_group = sites,
    status = statuses,
    stringsAsFactors = FALSE
  )
  
  summary_table <- long_data %>%
    dplyr::count(forms, redcap_data_access_group, status, name = "n") %>%
    dplyr::right_join(complete_grid, by = c("forms", "redcap_data_access_group", "status")) %>%
    tidyr::replace_na(list(n = 0)) %>%
    dplyr::mutate(col = paste(redcap_data_access_group, status, sep = "_")) %>%
    dplyr::select(forms, col, n) %>%
    tidyr::pivot_wider(names_from = col, values_from = n)
  
  # Reorder summary_table columns to match header order (site-major, status-minor)
  ordered_cols <- c(
    "forms",
    paste(
      rep(sites, each = length(statuses)),
      rep(statuses, times = length(sites)),
      sep = "_"
    )
  )
  
  # Ensure all expected columns exist (in case some site/status combo never appears)
  missing_cols <- setdiff(ordered_cols, names(summary_table))
  if (length(missing_cols) > 0) summary_table[missing_cols] <- 0
  
  summary_table <- summary_table %>%
    dplyr::select(dplyr::all_of(ordered_cols)) %>%
    dplyr::mutate(.ord = match(forms, form_list)) %>%
    dplyr::arrange(.ord) %>%
    dplyr::select(-.ord)
  
  summary_table
}
