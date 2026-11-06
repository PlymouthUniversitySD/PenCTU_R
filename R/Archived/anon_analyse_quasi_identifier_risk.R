#Title: ANON - Analyse quasi-identifier disclosure risk
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Analyse Quasi-Identifier Disclosure Risk
#'
#' Runs participant-level disclosure risk checks for fields marked as
#' quasi-identifiers in an annotated REDCap data dictionary.
#'
#' This function is intended to be run before anonymisation. It helps identify
#' variables or response options that may need to be removed, combined,
#' generalised, or suppressed.
#'
#' Quasi-identifiers are identified from the data dictionary using the
#' \code{quasi_identifier} column. Fields marked \code{"Y"} are included in the
#' analysis.
#'
#' Because REDCap longitudinal datasets may contain multiple rows per
#' participant, the analysis is performed at participant level. Each participant
#' contributes one value per quasi-identifier.
#'
#' If \code{event_field} and \code{preferred_event_name} are supplied, values
#' from that event are preferred. Otherwise, the first non-missing value per
#' participant is used.
#'
#' REDCap checkbox fields are handled automatically. For example, a checkbox
#' field called \code{symptoms} with response options \code{1}, \code{2}, and
#' \code{3} will be analysed using the exported columns:
#' \itemize{
#'   \item \code{symptoms___1}
#'   \item \code{symptoms___2}
#'   \item \code{symptoms___3}
#' }
#'
#' @param data A data frame containing the REDCap dataset.
#' @param data_dictionary A data frame containing the annotated REDCap data
#'   dictionary.
#' @param participant_id_field Character string specifying the participant ID
#'   field. Default is \code{"record_id"}.
#' @param quasi_identifier_column Character string specifying the column in the
#'   data dictionary used to identify quasi-identifiers. Default is
#'   \code{"quasi_identifier"}.
#' @param event_field Optional character string specifying the REDCap event
#'   field, usually \code{"redcap_event_name"}.
#' @param preferred_event_name Optional character string specifying the event to
#'   prefer when deriving participant-level quasi-identifier values.
#' @param k_threshold Numeric value specifying the minimum acceptable group size
#'   for k-anonymity. Default is \code{5}.
#' @param missing_label Character string used to label missing values in outputs.
#'   Default is \code{"[Missing]"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{participant_level_data}{One-row-per-participant dataset used for analysis}
#'     \item{quasi_identifier_lookup}{Lookup showing which fields were analysed}
#'     \item{missing_quasi_identifiers}{Quasi-identifiers marked in the dictionary but not found in the dataset}
#'     \item{response_option_summary}{Participant counts for each value of each quasi-identifier}
#'     \item{variable_risk_summary}{Variable-level risk summary}
#'     \item{k_anonymity_report}{Participant counts for each unique quasi-identifier combination}
#'     \item{overall_risk_summary}{Overall k-anonymity and re-identification risk summary}
#'   }
#'
#' @examples
#' qi_risk <- analyse_quasi_identifier_risk(
#'   data = dataset,
#'   data_dictionary = data_dictionary,
#'   participant_id_field = "record_id",
#'   quasi_identifier_column = "quasi_identifier",
#'   event_field = "redcap_event_name",
#'   preferred_event_name = "baseline_arm_1",
#'   k_threshold = 5
#' )
#'
#' qi_risk$response_option_summary
#' qi_risk$variable_risk_summary
#' qi_risk$k_anonymity_report
#' qi_risk$overall_risk_summary
#'
#' @export
analyse_quasi_identifier_risk <- function(data,
                                          data_dictionary,
                                          participant_id_field = "record_id",
                                          quasi_identifier_column = "quasi_identifier",
                                          event_field = NULL,
                                          preferred_event_name = NULL,
                                          k_threshold = 5,
                                          missing_label = "[Missing]") {
  
  is_marked_yes <- function(x) {
    toupper(trimws(as.character(x))) == "Y"
  }
  
  first_non_missing <- function(x) {
    x <- x[!is.na(x) & as.character(x) != ""]
    if (length(x) == 0) {
      return(NA)
    }
    x[[1]]
  }
  
  parse_redcap_response_options <- function(options_text) {
    
    if (
      is.null(options_text) ||
      is.na(options_text) ||
      trimws(as.character(options_text)) == ""
    ) {
      return(
        tibble::tibble(
          code = character(),
          label = character()
        )
      )
    }
    
    parts <- unlist(
      strsplit(
        as.character(options_text),
        "\\s*\\|\\s*"
      )
    )
    
    parsed <- lapply(parts, function(part) {
      
      m <- regexec(
        "^\\s*([^,]+?)\\s*,\\s*(.*)\\s*$",
        part
      )
      
      r <- regmatches(part, m)[[1]]
      
      if (length(r) == 3) {
        tibble::tibble(
          code = trimws(r[2]),
          label = trimws(r[3])
        )
      } else {
        tibble::tibble(
          code = NA_character_,
          label = trimws(part)
        )
      }
    })
    
    dplyr::bind_rows(parsed)
  }
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(data_dictionary)) {
    stop("data_dictionary must be a data frame.", call. = FALSE)
  }
  
  if (!participant_id_field %in% names(data)) {
    stop(
      paste0("participant_id_field not found in data: ", participant_id_field),
      call. = FALSE
    )
  }
  
  required_dictionary_columns <- c(
    "field_name",
    "data_type",
    "response_options_calculations_sliders",
    quasi_identifier_column
  )
  
  missing_dictionary_columns <- setdiff(
    required_dictionary_columns,
    names(data_dictionary)
  )
  
  if (length(missing_dictionary_columns) > 0) {
    stop(
      paste0(
        "data_dictionary is missing required columns: ",
        paste(missing_dictionary_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if (!is.null(event_field) && !event_field %in% names(data)) {
    stop(
      paste0("event_field not found in data: ", event_field),
      call. = FALSE
    )
  }
  
  qi_rows <- data_dictionary |>
    dplyr::filter(is_marked_yes(.data[[quasi_identifier_column]]))
  
  if (nrow(qi_rows) == 0) {
    stop("No fields are marked as quasi-identifiers.", call. = FALSE)
  }
  
  quasi_identifier_lookup <- purrr::map_dfr(seq_len(nrow(qi_rows)), function(i) {
    
    field_name <- as.character(qi_rows$field_name[[i]])
    data_type <- tolower(trimws(as.character(qi_rows$data_type[[i]])))
    response_options <- qi_rows$response_options_calculations_sliders[[i]]
    
    if (data_type == "checkbox") {
      
      checkbox_options <- parse_redcap_response_options(response_options)
      
      if (nrow(checkbox_options) == 0) {
        return(
          tibble::tibble(
            field_name = field_name,
            analysis_field_name = NA_character_,
            data_type = data_type,
            checkbox_code = NA_character_,
            checkbox_label = NA_character_,
            found_in_data = FALSE
          )
        )
      }
      
      exported_fields <- paste0(field_name, "___", checkbox_options$code)
      
      tibble::tibble(
        field_name = field_name,
        analysis_field_name = exported_fields,
        data_type = data_type,
        checkbox_code = checkbox_options$code,
        checkbox_label = checkbox_options$label,
        found_in_data = exported_fields %in% names(data)
      )
      
    } else {
      
      tibble::tibble(
        field_name = field_name,
        analysis_field_name = field_name,
        data_type = data_type,
        checkbox_code = NA_character_,
        checkbox_label = NA_character_,
        found_in_data = field_name %in% names(data)
      )
    }
  })
  
  missing_quasi_identifiers <- quasi_identifier_lookup |>
    dplyr::filter(!.data$found_in_data | is.na(.data$analysis_field_name))
  
  analysis_fields <- quasi_identifier_lookup |>
    dplyr::filter(.data$found_in_data, !is.na(.data$analysis_field_name)) |>
    dplyr::pull(.data$analysis_field_name) |>
    unique()
  
  if (length(analysis_fields) == 0) {
    stop("No quasi-identifier fields were found in the dataset.", call. = FALSE)
  }
  
  if (!is.null(event_field) && !is.null(preferred_event_name)) {
    
    preferred_data <- data |>
      dplyr::filter(.data[[event_field]] == preferred_event_name) |>
      dplyr::group_by(.data[[participant_id_field]]) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(analysis_fields),
          first_non_missing
        ),
        .groups = "drop"
      )
    
    fallback_data <- data |>
      dplyr::group_by(.data[[participant_id_field]]) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(analysis_fields),
          first_non_missing
        ),
        .groups = "drop"
      )
    
    participant_level_data <- fallback_data |>
      dplyr::rows_update(
        preferred_data,
        by = participant_id_field,
        unmatched = "ignore"
      )
    
  } else {
    
    participant_level_data <- data |>
      dplyr::group_by(.data[[participant_id_field]]) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(analysis_fields),
          first_non_missing
        ),
        .groups = "drop"
      )
  }
  
  n_participants <- nrow(participant_level_data)
  
  response_option_summary <- purrr::map_dfr(analysis_fields, function(field) {
    
    lookup_row <- quasi_identifier_lookup |>
      dplyr::filter(.data$analysis_field_name == field) |>
      dplyr::slice(1)
    
    is_checkbox <- lookup_row$data_type[[1]] == "checkbox"
    
    participant_level_data |>
      dplyr::mutate(
        value = as.character(.data[[field]]),
        value = ifelse(
          is.na(.data$value) | .data$value == "",
          missing_label,
          .data$value
        ),
        value_label = dplyr::case_when(
          is_checkbox & .data$value %in% c("1", "Checked", "checked", "TRUE", "True", "true") ~ "Checked",
          is_checkbox & .data$value %in% c("0", "Unchecked", "unchecked", "FALSE", "False", "false") ~ "Unchecked",
          TRUE ~ .data$value
        )
      ) |>
      dplyr::count(.data$value, .data$value_label, name = "participants") |>
      dplyr::mutate(
        field_name = lookup_row$field_name[[1]],
        analysis_field_name = field,
        data_type = lookup_row$data_type[[1]],
        checkbox_code = lookup_row$checkbox_code[[1]],
        checkbox_label = lookup_row$checkbox_label[[1]],
        percentage = round(100 * .data$participants / n_participants, 2),
        below_k_threshold = .data$participants < k_threshold
      ) |>
      dplyr::select(
        .data$field_name,
        .data$analysis_field_name,
        .data$data_type,
        .data$checkbox_code,
        .data$checkbox_label,
        .data$value,
        .data$value_label,
        .data$participants,
        .data$percentage,
        .data$below_k_threshold
      )
  })
  
  variable_risk_summary <- response_option_summary |>
    dplyr::group_by(.data$field_name, .data$analysis_field_name, .data$data_type) |>
    dplyr::summarise(
      distinct_values = dplyr::n(),
      smallest_group_n = min(.data$participants, na.rm = TRUE),
      values_below_k = sum(.data$participants < k_threshold, na.rm = TRUE),
      recommended_review = .data$values_below_k > 0,
      suggested_action = dplyr::case_when(
        .data$smallest_group_n < k_threshold & .data$distinct_values > 20 ~
          "remove or heavily generalise",
        .data$smallest_group_n < k_threshold ~
          "combine categories or suppress small groups",
        TRUE ~
          "likely safe as single variable; check combined k-anonymity"
      ),
      .groups = "drop"
    )
  
  k_anonymity_report <- participant_level_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(analysis_fields),
        ~ as.character(.x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(analysis_fields),
        ~ ifelse(is.na(.x) | .x == "", missing_label, .x)
      )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(analysis_fields)
      )
    ) |>
    dplyr::summarise(
      participants = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      k = .data$participants,
      below_k_threshold = .data$k < k_threshold
    ) |>
    dplyr::arrange(.data$k)
  
  participants_in_groups_below_k <- sum(
    k_anonymity_report$participants[k_anonymity_report$below_k_threshold],
    na.rm = TRUE
  )
  
  overall_reidentification_risk <- participants_in_groups_below_k / n_participants
  
  overall_risk_summary <- tibble::tibble(
    participants = n_participants,
    quasi_identifier_count = length(analysis_fields),
    k_threshold = k_threshold,
    minimum_k = min(k_anonymity_report$k, na.rm = TRUE),
    groups_below_k = sum(k_anonymity_report$below_k_threshold, na.rm = TRUE),
    participants_in_groups_below_k = participants_in_groups_below_k,
    percentage_participants_in_groups_below_k = round(
      100 * overall_reidentification_risk,
      2
    ),
    overall_reidentification_risk = round(
      overall_reidentification_risk,
      4
    ),
    overall_reidentification_risk_percentage = round(
      100 * overall_reidentification_risk,
      2
    ),
    overall_reidentification_risk_band = dplyr::case_when(
      overall_reidentification_risk == 0 ~ "very low",
      overall_reidentification_risk < 0.05 ~ "low",
      overall_reidentification_risk < 0.20 ~ "moderate",
      TRUE ~ "high"
    )
  )
  
  list(
    participant_level_data = participant_level_data,
    quasi_identifier_lookup = quasi_identifier_lookup,
    missing_quasi_identifiers = missing_quasi_identifiers,
    response_option_summary = response_option_summary,
    variable_risk_summary = variable_risk_summary,
    k_anonymity_report = k_anonymity_report,
    overall_risk_summary = overall_risk_summary
  )
}
