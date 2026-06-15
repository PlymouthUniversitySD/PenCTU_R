#Title: ANON - Remove participants meeting specified logic
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Remove Participants Meeting Specified Logic
#'
#' Removes all rows for participants where at least one row meets the specified
#' exclusion logic.
#'
#' This is intended for REDCap datasets where participants may have multiple
#' rows, for example because of longitudinal events or repeating instruments.
#' If one row for a participant meets the exclusion logic, all rows for that
#' participant are removed.
#'
#' For example, if participant 5 has four rows and one row has
#' \code{duplicate_record_1 == "Checked"}, all four rows for participant 5
#' will be removed.
#'
#' @param data A data frame containing the dataset.
#' @param participant_id_field Character string specifying the participant ID
#'   field. Default is \code{"record_id"}.
#' @param exclusion_logic A logical expression used to identify rows that
#'   should trigger participant-level exclusion.
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{Dataset with excluded participants removed}
#'     \item{exclusion_log}{Summary of excluded participants and row counts}
#'   }
#'
#' @examples
#' result <- remove_participants_meeting_logic(
#'   data = dataset,
#'   participant_id_field = "record_id",
#'   exclusion_logic = duplicate_record_1 == "Checked"
#' )
#'
#' clean_dataset <- result$data
#' exclusion_log <- result$exclusion_log
#'
#' @export
remove_participants_meeting_logic <- function(data,
                                              participant_id_field = "record_id",
                                              exclusion_logic) {
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  
  if (!participant_id_field %in% names(data)) {
    stop(
      paste0("participant_id_field not found in data: ", participant_id_field),
      call. = FALSE
    )
  }
  
  logic_expr <- rlang::enquo(exclusion_logic)
  
  flagged_data <- data |>
    dplyr::mutate(.exclusion_flag = !!logic_expr)
  
  if (!is.logical(flagged_data$.exclusion_flag)) {
    stop("exclusion_logic must evaluate to TRUE/FALSE.", call. = FALSE)
  }
  
  excluded_ids <- flagged_data |>
    dplyr::filter(.exclusion_flag %in% TRUE) |>
    dplyr::pull(dplyr::all_of(participant_id_field)) |>
    unique()
  
  cleaned_data <- data |>
    dplyr::filter(!.data[[participant_id_field]] %in% excluded_ids)
  
  exclusion_log <- data |>
    dplyr::filter(.data[[participant_id_field]] %in% excluded_ids) |>
    dplyr::count(
      .data[[participant_id_field]],
      name = "excluded_rows"
    ) |>
    dplyr::rename(participant_id = .data[[participant_id_field]]) |>
    dplyr::mutate(
      exclusion_reason = rlang::as_label(logic_expr)
    )
  
  list(
    data = cleaned_data,
    exclusion_log = exclusion_log
  )
}