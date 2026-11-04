#Title: ATR - Identify critical data item changes
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026
#R version: 4.4.3

#' Identify Changes to Critical Data Items
#'
#' Identifies updates to predefined critical data items within the audit trail
#' dataset where the recorded value has changed from a previous non-missing
#' value. The function summarises the number of changes per critical field and
#' provides a list of affected records and events.
#'
#' A change is identified where:
#' \itemize{
#'   \item \code{action == "Update record"}
#'   \item \code{old_value} is not missing
#'   \item \code{old_value != "NA"}
#'   \item \code{old_value != new_value}
#' }
#'
#' Only fields listed within \code{critical_data_items$field_name} are
#' evaluated.
#'
#' If multiple changes occur for the same record-event combination, the number
#' of changes is displayed within the \code{affected_records} output column.
#'
#' @param atr_data A data frame containing REDCap audit trail data. Expected
#'   columns include:
#'   \describe{
#'     \item{field}{Field/variable name}
#'     \item{action}{Audit action performed}
#'     \item{record}{Record identifier}
#'     \item{event}{Event name}
#'     \item{old_value}{Previous value}
#'     \item{new_value}{Updated value}
#'   }
#'
#' @param critical_data_items A data frame defining critical data items.
#'   Expected columns include:
#'   \describe{
#'     \item{field_name}{Variable/field name}
#'     \item{critical_data_item}{Critical data item description}
#'     \item{critical_data_item_type}{Critical data item category/type}
#'   }
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{critical_data_item_type}{Critical data item category}
#'     \item{critical_data_item}{Critical data item description}
#'     \item{field}{Field name}
#'     \item{n_changes}{Total number of qualifying changes}
#'     \item{n_affected_records}{Number of unique affected record-event combinations}
#'     \item{affected_records}{Semicolon-separated list of affected records and events}
#'   }
#'
#' @examples
#' critical_data_changes <- identify_critical_data_changes(
#'   atr_data = atr_data,
#'   critical_data_items = critical_data_items
#' )
#'
#' @export

identify_critical_data_changes <- function(atr_data, critical_data_items) {
  
  library(dplyr)
  
  atr_data %>%
    inner_join(
      critical_data_items,
      by = c("field" = "field_name")
    ) %>%
    filter(
      action == "Update record",
      !is.na(old_value),
      old_value != "NA",
      old_value != new_value
    ) %>%
    group_by(
      critical_data_item_type,
      critical_data_item,
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
    group_by(
      critical_data_item_type,
      critical_data_item,
      field
    ) %>%
    summarise(
      n_changes = sum(record_change_count),
      n_affected_records = n(),
      affected_records = paste(affected_record, collapse = "; "),
      .groups = "drop"
    ) %>%
    arrange(
      critical_data_item_type,
      desc(n_changes),
      field
    )
}

