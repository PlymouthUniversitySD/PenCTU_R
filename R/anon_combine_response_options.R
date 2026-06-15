#Title: ANON - Combine coded response options
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Combine Coded Response Options
#'
#' Combines coded categorical response options in a REDCap dataset and updates
#' the corresponding REDCap data dictionary response options.
#'
#' Combination rules are supplied in a \code{rule_spec} JSON column. Each rule
#' should specify old code, old label, new code, and new label.
#'
#' This function is intended for single-column categorical REDCap fields such as
#' \code{radio}, \code{dropdown}, \code{yesno}, and \code{truefalse}.
#'
#' REDCap checkbox fields are not combined by this function because checkbox
#' options are exported as separate columns, for example
#' \code{field_name___1}, \code{field_name___2}. Checkbox combinations should be
#' handled by a separate checkbox-specific function.
#'
#' @param dataset A data frame containing the REDCap dataset.
#' @param data_dictionary A data frame containing the annotated REDCap data
#'   dictionary.
#' @param field_name_column Character string specifying the field name column in
#'   the data dictionary. Default is \code{"field_name"}.
#' @param data_type_column Character string specifying the REDCap field type
#'   column. Default is \code{"data_type"}.
#' @param action_column Character string specifying the anonymisation action
#'   column. Default is \code{"action"}.
#' @param rule_spec_column Character string specifying the JSON rule column.
#'   Default is \code{"rule_spec"}.
#' @param response_options_column Character string specifying the REDCap response
#'   options column. Default is
#'   \code{"response_options_calculations_sliders"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with response options combined}
#'     \item{data_dictionary}{Data dictionary with updated response options}
#'     \item{change_log}{Summary of response option combinations}
#'   }
#'
#' @examples
#' result <- combine_response_options(
#'   dataset = erase_data,
#'   data_dictionary = erase_data_dictionary
#' )
#'
#' @export
combine_response_options <- function(dataset,
                                     data_dictionary,
                                     field_name_column = "field_name",
                                     data_type_column = "data_type",
                                     action_column = "action",
                                     rule_spec_column = "rule_spec",
                                     response_options_column = "response_options_calculations_sliders") {
  
  parse_rule_spec <- function(rule_spec) {
    if (is.null(rule_spec) || is.na(rule_spec) || trimws(as.character(rule_spec)) == "") {
      return(list())
    }
    
    jsonlite::fromJSON(
      txt = as.character(rule_spec),
      simplifyVector = FALSE
    )
  }
  
  format_redcap_response_options <- function(mapping) {
    new_options <- mapping |>
      dplyr::distinct(.data$new_code, .data$new_label) |>
      dplyr::arrange(.data$new_code) |>
      dplyr::mutate(option = paste0(.data$new_code, ", ", .data$new_label)) |>
      dplyr::pull(.data$option)
    
    paste(new_options, collapse = " | ")
  }
  
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(data_dictionary)) {
    stop("data_dictionary must be a data frame.", call. = FALSE)
  }
  
  required_columns <- c(
    field_name_column,
    data_type_column,
    action_column,
    rule_spec_column,
    response_options_column
  )
  
  missing_columns <- setdiff(required_columns, names(data_dictionary))
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "data_dictionary is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  combine_rows <- data_dictionary |>
    dplyr::filter(
      tolower(trimws(as.character(.data[[action_column]]))) == "combine"
    )
  
  updated_dataset <- dataset
  updated_data_dictionary <- data_dictionary
  
  change_log <- tibble::tibble(
    action = character(),
    field_name = character(),
    old_code = character(),
    old_label = character(),
    new_code = character(),
    new_label = character(),
    status = character()
  )
  
  if (nrow(combine_rows) == 0) {
    return(
      list(
        dataset = updated_dataset,
        data_dictionary = updated_data_dictionary,
        change_log = change_log
      )
    )
  }
  
  for (i in seq_len(nrow(combine_rows))) {
    
    field_name <- as.character(combine_rows[[field_name_column]][[i]])
    data_type <- tolower(trimws(as.character(combine_rows[[data_type_column]][[i]])))
    rule_spec <- parse_rule_spec(combine_rows[[rule_spec_column]][[i]])
    
    if (data_type == "checkbox") {
      change_log <- dplyr::bind_rows(
        change_log,
        tibble::tibble(
          action = "combine",
          field_name = field_name,
          old_code = NA_character_,
          old_label = NA_character_,
          new_code = NA_character_,
          new_label = NA_character_,
          status = "skipped: checkbox fields require checkbox-specific combine function"
        )
      )
      next
    }
    
    if (!field_name %in% names(updated_dataset)) {
      change_log <- dplyr::bind_rows(
        change_log,
        tibble::tibble(
          action = "combine",
          field_name = field_name,
          old_code = NA_character_,
          old_label = NA_character_,
          new_code = NA_character_,
          new_label = NA_character_,
          status = "skipped: field not found in dataset"
        )
      )
      next
    }
    
    if (is.null(rule_spec$combine)) {
      stop(
        paste0("combine rule_spec missing 'combine' object for field: ", field_name),
        call. = FALSE
      )
    }
    
    mapping <- dplyr::bind_rows(rule_spec$combine) |>
      dplyr::mutate(
        old_code = as.character(.data$old_code),
        old_label = as.character(.data$old_label),
        new_code = as.character(.data$new_code),
        new_label = as.character(.data$new_label)
      )
    
    required_mapping_columns <- c(
      "old_code",
      "old_label",
      "new_code",
      "new_label"
    )
    
    missing_mapping_columns <- setdiff(required_mapping_columns, names(mapping))
    
    if (length(missing_mapping_columns) > 0) {
      stop(
        paste0(
          "combine mapping for ",
          field_name,
          " is missing columns: ",
          paste(missing_mapping_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    value_lookup <- stats::setNames(
      mapping$new_code,
      mapping$old_code
    )
    
    original_values <- as.character(updated_dataset[[field_name]])
    
    recoded_values <- dplyr::if_else(
      original_values %in% names(value_lookup),
      unname(value_lookup[original_values]),
      original_values
    )
    
    updated_dataset[[field_name]] <- recoded_values
    
    updated_response_options <- format_redcap_response_options(mapping)
    
    updated_data_dictionary[[response_options_column]][
      updated_data_dictionary[[field_name_column]] == field_name
    ] <- updated_response_options
    
    change_log <- dplyr::bind_rows(
      change_log,
      mapping |>
        dplyr::mutate(
          action = "combine",
          field_name = field_name,
          status = "applied"
        ) |>
        dplyr::select(
          .data$action,
          .data$field_name,
          .data$old_code,
          .data$old_label,
          .data$new_code,
          .data$new_label,
          .data$status
        )
    )
  }
  
  list(
    dataset = updated_dataset,
    data_dictionary = updated_data_dictionary,
    change_log = change_log
  )
}