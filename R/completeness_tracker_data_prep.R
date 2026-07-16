#' Prepare Data for the Completeness Tracker
#'
#' Converts REDCap form-completion data into a site-level summary table suitable
#' for use in the completeness-tracker Excel report.
#'
#' The function identifies all fields ending in \code{"_complete"} and converts
#' them from wide format into a long-form dataset containing one row per record,
#' form, and completion status.
#'
#' REDCap completion-status values are mapped to the labels supplied through
#' \code{statuses}. The \code{statuses} argument must contain exactly three
#' values in the following order:
#'
#' \enumerate{
#'   \item The label to use for incomplete forms
#'   \item The label to use for unverified or partially complete forms
#'   \item The label to use for complete forms
#' }
#'
#' Raw REDCap values of \code{0} or \code{"Incomplete"} are mapped to the first
#' status label. Values of \code{1}, \code{"Unverified"}, or
#' \code{"Partially complete"} are mapped to the second status label. Values of
#' \code{2} or \code{"Complete"} are mapped to the third status label.
#'
#' Missing completion statuses are mapped to the first value supplied in
#' \code{statuses}. Records without an assigned REDCap Data Access Group are
#' classified under the site label \code{"Unknown"}.
#'
#' Repeat-instance rows can optionally be excluded before the summary is
#' calculated. When \code{exclude_repeat_instances = TRUE}, rows where
#' \code{redcap_repeat_instance} is equal to or greater than \code{1} are
#' removed. Non-repeating rows, represented by a missing, blank, or zero repeat
#' instance, are retained.
#'
#' If repeat instances are to be excluded, the input dataset must contain a
#' \code{redcap_repeat_instance} column.
#'
#' A complete grid of all expected combinations of form, site, and completion
#' status is generated. This ensures that combinations with no observed records
#' are retained in the output and assigned a count of zero.
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
#' Form rows are ordered according to \code{form_list}, rather than
#' alphabetically.
#'
#' @param data A data frame containing REDCap form-completion fields. The dataset
#'   must include a \code{redcap_data_access_group} column and one or more fields
#'   whose names end in \code{"_complete"}. When
#'   \code{exclude_repeat_instances = TRUE}, the dataset must also contain a
#'   \code{redcap_repeat_instance} column.
#'
#' @param form_list A character vector defining the forms to include in the
#'   summary and the order in which they should appear.
#'
#' @param sites A character vector defining the sites to include and their
#'   display order. Include \code{"Unknown"} where records without a Data Access
#'   Group should be represented explicitly.
#'
#' @param statuses A character vector containing exactly three display labels.
#'   Values must be supplied in the following order: incomplete, unverified or
#'   partially complete, and complete.
#'
#' @param exclude_repeat_instances A logical value indicating whether repeating
#'   rows should be excluded. When \code{TRUE}, rows with a
#'   \code{redcap_repeat_instance} value of \code{1} or greater are removed.
#'   Defaults to \code{FALSE}.
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
#'     "Incomplete",
#'     "Partially complete",
#'     "Complete"
#'   ),
#'   exclude_repeat_instances = TRUE
#' )
#'
#' @export

completeness_tracker_data_prep <- function(
    data,
    form_list,
    sites,
    statuses,
    exclude_repeat_instances = FALSE
) {
  
  # Validate the status labels
  if (length(statuses) != 3) {
    stop(
      "`statuses` must contain exactly three values in this order: ",
      "Incomplete, Unverified, Complete."
    )
  }
  
  # Validate the repeat-instance argument
  if (
    !is.logical(exclude_repeat_instances) ||
    length(exclude_repeat_instances) != 1 ||
    is.na(exclude_repeat_instances)
  ) {
    stop(
      "`exclude_repeat_instances` must be a single TRUE or FALSE value."
    )
  }
  
  # Check that the repeat-instance field is available when required
  if (
    exclude_repeat_instances &&
    !"redcap_repeat_instance" %in% names(data)
  ) {
    stop(
      "`exclude_repeat_instances` is TRUE, but the dataset does not contain ",
      "a `redcap_repeat_instance` column."
    )
  }
  
  # Optionally remove rows representing repeat instances
  if (exclude_repeat_instances) {
    
    data <- data %>%
      dplyr::mutate(
        .repeat_instance = suppressWarnings(
          as.numeric(as.character(redcap_repeat_instance))
        )
      ) %>%
      dplyr::filter(
        is.na(.repeat_instance) |
          .repeat_instance < 1
      ) %>%
      dplyr::select(-.repeat_instance)
  }
  
  # Convert form-completion fields to long format and map REDCap statuses
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_complete"),
      names_to = "forms",
      values_to = "status"
    ) %>%
    dplyr::mutate(
      raw_status = stringr::str_to_lower(
        stringr::str_trim(as.character(status))
      ),
      status = dplyr::case_when(
        is.na(status) |
          raw_status == "" ~ statuses[[1]],
        
        raw_status %in% c(
          "0",
          "incomplete"
        ) ~ statuses[[1]],
        
        raw_status %in% c(
          "1",
          "unverified",
          "unverified/partially complete",
          "partially complete"
        ) ~ statuses[[2]],
        
        raw_status %in% c(
          "2",
          "complete"
        ) ~ statuses[[3]],
        
        TRUE ~ NA_character_
      ),
      redcap_data_access_group = dplyr::if_else(
        is.na(redcap_data_access_group) |
          stringr::str_trim(
            as.character(redcap_data_access_group)
          ) == "",
        "Unknown",
        as.character(redcap_data_access_group)
      )
    ) %>%
    dplyr::select(-raw_status)
  
  # Generate every expected form, site, and status combination
  complete_grid <- expand.grid(
    forms = form_list,
    redcap_data_access_group = sites,
    status = statuses,
    stringsAsFactors = FALSE
  )
  
  # Count records and reshape the summary into wide format
  summary_table <- long_data %>%
    dplyr::filter(
      forms %in% form_list,
      redcap_data_access_group %in% sites,
      status %in% statuses
    ) %>%
    dplyr::count(
      forms,
      redcap_data_access_group,
      status,
      name = "n"
    ) %>%
    dplyr::right_join(
      complete_grid,
      by = c(
        "forms",
        "redcap_data_access_group",
        "status"
      )
    ) %>%
    tidyr::replace_na(
      list(n = 0)
    ) %>%
    dplyr::mutate(
      col = paste(
        redcap_data_access_group,
        status,
        sep = "_"
      )
    ) %>%
    dplyr::select(
      forms,
      col,
      n
    ) %>%
    tidyr::pivot_wider(
      names_from = col,
      values_from = n,
      values_fill = 0
    )
  
  # Define the required site-major, status-minor column order
  ordered_cols <- c(
    "forms",
    paste(
      rep(
        sites,
        each = length(statuses)
      ),
      rep(
        statuses,
        times = length(sites)
      ),
      sep = "_"
    )
  )
  
  # Add any expected columns that are absent from the observed data
  missing_cols <- setdiff(
    ordered_cols,
    names(summary_table)
  )
  
  if (length(missing_cols) > 0) {
    summary_table[missing_cols] <- 0
  }
  
  # Apply the required column and form ordering
  summary_table %>%
    dplyr::select(
      dplyr::all_of(ordered_cols)
    ) %>%
    dplyr::mutate(
      .ord = match(
        forms,
        form_list
      )
    ) %>%
    dplyr::arrange(.ord) %>%
    dplyr::select(-.ord)
}
