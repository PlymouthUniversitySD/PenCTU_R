#Title: ATR - Identify unstable fields
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026

#R version: 4.4.3
#' Identify Unstable Fields
#'
#' Identifies fields within the audit trail dataset that have one or more
#' qualifying data changes and summarises the total number of changes per field.
#'
#' A qualifying change is identified where:
#' \itemize{
#'   \item \code{action == "Update record"}
#'   \item \code{old_value} is not missing
#'   \item \code{old_value != "NA"}
#'   \item \code{old_value != new_value}
#' }
#'
#' The following fields are excluded:
#' \itemize{
#'   \item Fields annotated within the metadata dataset using:
#'   \itemize{
#'     \item \code{#DataManagement}
#'     \item \code{#SystemFunctionality}
#'     \item \code{#TrialManagement}
#'   }
#'   \item Fields with names ending in \code{"_complete"}
#'   \item Data Access Group assignment fields
#' }
#'   \item Fields with names ending in \code{"_complete"}
#' }
#'
#' @param atr_data A data frame containing REDCap audit trail data. Expected
#'   columns include:
#'   \describe{
#'     \item{field}{Field/variable name}
#'     \item{action}{Audit action performed}
#'     \item{old_value}{Previous value}
#'     \item{new_value}{Updated value}
#'   }
#'
#' @param metadata A data frame containing REDCap metadata. Expected columns
#'   include:
#'   \describe{
#'     \item{field_name}{Field/variable name}
#'     \item{field_annotation}{Field annotation text}
#'   }
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{field}{Field/variable name}
#'     \item{n_changes}{Total number of qualifying changes for the field}
#'     \item{n_affected_records}{Number of unique affected record-event combinations}
#'     \item{affected_records}{Semicolon-separated list of affected records and events}
#'   }
#'
#' Results are ordered from the highest to lowest number of changes.
#'
#' @examples
#' unstable_fields <- identify_unstable_fields(
#'   atr_data = atr_data,
#'   metadata = metadata
#' )
#'
#' @export
identify_unstable_fields <- function(
    atr_data,
    metadata
) {
  
  excluded_fields <- metadata %>%
    filter(
      grepl(
        "#DataManagement|#SystemFunctionality|#TrialManagement",
        field_annotation
      )
    ) %>%
    pull(field_name)
  
  atr_data %>%
    filter(
      action == "Update record",
      !is.na(old_value),
      old_value != "NA",
      old_value != new_value,
      !field %in% excluded_fields,
      !grepl("_complete$", field),
      !grepl("Assign record to Data Access Group", field, ignore.case = TRUE)
    ) %>%
    group_by(
      field,
      record,
      event
    ) %>%
    summarise(
      record_change_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      affected_record = if_else(
        record_change_count > 1,
        paste0(record, " (", event, ") [", record_change_count, " changes]"),
        paste0(record, " (", event, ")")
      )
    ) %>%
    group_by(field) %>%
    summarise(
      n_changes = sum(record_change_count),
      n_affected_records = n(),
      affected_records = paste(affected_record, collapse = "; "),
      .groups = "drop"
    ) %>%
    filter(n_changes > 0) %>%
    arrange(desc(n_changes), field)
}
