#Title: ANON - Recode field values
#Author: Paigan Aspinall
#Version & Date: V1.0.0 01JUN2026
#R version: 4.4.3

#' Recode Field Values
#'
#' Recodes values in REDCap dataset fields using mappings specified in an
#' annotated data dictionary.
#'
#' This function is intended for straightforward value replacement where the
#' meaning of values changes, but response options are not being combined. For
#' example, it can recode \code{"Yes"} to \code{"1"} and \code{"No"} to
#' \code{"0"}, or recode old category codes into revised category codes.
#'
#' Recode rules are supplied in a \code{rule_spec} JSON column. The expected
#' structure is:
#'
#' \preformatted{
#' {
#'   "mapping": {
#'     "old_value_1": "new_value_1",
#'     "old_value_2": "new_value_2"
#'   },
#'   "unmapped": "keep"
#' }
#' }
#'
#' The \code{unmapped} option controls what happens to values not included in
#' the mapping:
#' \itemize{
#'   \item \code{"keep"} keeps unmapped values unchanged
#'   \item \code{"missing"} converts unmapped values to \code{NA}
#'   \item \code{"error"} stops the function if unmapped values are found
#' }
#'
#' REDCap checkbox fields are handled automatically. If a checkbox parent field
#' is marked for recoding, each exported checkbox column is recoded separately.
#' For example, \code{symptoms___1}, \code{symptoms___2}, and
#' \code{symptoms___3} will be processed if \code{symptoms} is marked as
#' \code{recode}.
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
#'
#' @return A list containing:
#'   \describe{
#'     \item{dataset}{Dataset with values recoded}
#'     \item{data_dictionary}{Data dictionary unchanged}
#'     \item{change_log}{Summary of value recoding}
#'   }
#'
#' @examples
#' result <- recode_values(
#'   dataset = erase_data,
#'   data_dictionary = erase_data_dictionary
#' )
#'
#' updated_data <- result$dataset
#' change_log <- result$change_log
#'
#' @export
recode_values <- function(dataset,
                          data_dictionary,
                          field_name_column = "field_name",
                          data_type_column = "data_type",
                          action_column = "action",
                          rule_spec_column = "rule_spec") {
  
  parse_rule_spec <- function(rule_spec) {
    if (is.null(rule_spec) || is.na(rule_spec) || trimws(as.character(rule_spec)) == "") {
      return(list())
    }
    
    jsonlite::fromJSON(
      txt = as.character(rule_spec),
      simplifyVector = FALSE
    )
  }
  
  recode_vector <- function(x, mapping, unmapped, field_name) {
    
    old_values <- names(mapping)
    new_values <- unlist(mapping, use.names = FALSE)
    
    value_lookup <- stats::setNames(
      as.character(new_values),
      as.character(old_values)
    )
    
    original_values <- as.character(x)
    
    mapped_values <- unname(
      value_lookup[original_values]
    )
    
    unmapped_values <- unique(
      original_values[
        is.na(mapped_values) &
          !is.na(original_values) &
          original_values != ""
      ]
    )
    
    if (unmapped == "error" && length(unmapped_values) > 0) {
      stop(
        paste0(
          "Unmapped values found in ",
          field_name,
          ": ",
          paste(unmapped_values, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    if (unmapped == "missing") {
      return(mapped_values)
    }
    
    if (unmapped == "keep") {
      return(
        dplyr::if_else(
          is.na(mapped_values),
          original_values,
          mapped_values
        )
      )
    }
    
    stop(
      paste0(
        "Invalid unmapped option for ",
        field_name,
        ". Use 'keep', 'missing', or 'error'."
      ),
      call. = FALSE
    )
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
    rule_spec_column
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
  
  recode_rows <- data_dictionary |>
    dplyr::filter(
      tolower(trimws(as.character(.data[[action_column]]))) == "recode"
    )
  
  updated_dataset <- dataset
  updated_data_dictionary <- data_dictionary
  
  change_log <- tibble::tibble(
    action = character(),
    field_name = character(),
    dataset_field = character(),
    old_value = character(),
    new_value = character(),
    status = character()
  )
  
  if (nrow(recode_rows) == 0) {
    return(
      list(
        dataset = updated_dataset,
        data_dictionary = updated_data_dictionary,
        change_log = change_log
      )
    )
  }
  
  for (i in seq_len(nrow(recode_rows))) {
    
    field_name <- as.character(recode_rows[[field_name_column]][[i]])
    data_type <- tolower(trimws(as.character(recode_rows[[data_type_column]][[i]])))
    rule_spec <- parse_rule_spec(recode_rows[[rule_spec_column]][[i]])
    
    if (is.null(rule_spec$mapping)) {
      stop(
        paste0("recode rule_spec missing 'mapping' object for field: ", field_name),
        call. = FALSE
      )
    }
    
    mapping <- rule_spec$mapping
    unmapped <- tolower(trimws(as.character(rule_spec$unmapped %||% "keep")))
    
    if (data_type == "checkbox") {
      
      dataset_fields <- names(updated_dataset)[
        grepl(paste0("^", field_name, "___"), names(updated_dataset))
      ]
      
    } else {
      
      dataset_fields <- field_name
    }
    
    dataset_fields_found <- dataset_fields[dataset_fields %in% names(updated_dataset)]
    
    if (length(dataset_fields_found) == 0) {
      change_log <- dplyr::bind_rows(
        change_log,
        tibble::tibble(
          action = "recode",
          field_name = field_name,
          dataset_field = NA_character_,
          old_value = NA_character_,
          new_value = NA_character_,
          status = "skipped: field not found in dataset"
        )
      )
      next
    }
    
    for (dataset_field in dataset_fields_found) {
      
      updated_dataset[[dataset_field]] <- recode_vector(
        x = updated_dataset[[dataset_field]],
        mapping = mapping,
        unmapped = unmapped,
        field_name = dataset_field
      )
      
      mapping_log <- tibble::tibble(
        action = "recode",
        field_name = field_name,
        dataset_field = dataset_field,
        old_value = names(mapping),
        new_value = as.character(unlist(mapping, use.names = FALSE)),
        status = "applied"
      )
      
      change_log <- dplyr::bind_rows(
        change_log,
        mapping_log
      )
    }
  }
  
  list(
    dataset = updated_dataset,
    data_dictionary = updated_data_dictionary,
    change_log = change_log
  )
}
