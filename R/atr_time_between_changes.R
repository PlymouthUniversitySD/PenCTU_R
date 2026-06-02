#Title: ATR - Time between changes
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026
#R version: 4.4.3

#' Identify Delayed Data Changes
#'
#' Returns one row per qualifying data change within the audit trail dataset
#' where the time difference between the previous and updated timestamps exceeds
#' one day.
#'
#' The function is intended to identify delayed modifications to study data
#' which may warrant further review.
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
#'
#' The time difference is calculated as the number of days between
#' \code{old_timestamp} and \code{new_timestamp}.
#'
#' @param atr_data A data frame containing REDCap audit trail data. Expected
#'   columns include:
#'   \describe{
#'     \item{record}{Record identifier}
#'     \item{event}{Event name}
#'     \item{field}{Field/variable name}
#'     \item{action}{Audit action performed}
#'     \item{old_value}{Previous value}
#'     \item{new_value}{Updated value}
#'     \item{reason}{Reason for change}
#'     \item{old_timestamp}{Previous timestamp}
#'     \item{new_timestamp}{Updated timestamp}
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
#'     \item{record}{Record identifier}
#'     \item{event}{Event name}
#'     \item{field}{Field/variable name}
#'     \item{old_value}{Previous value}
#'     \item{new_value}{Updated value}
#'     \item{reason}{Reason for change}
#'     \item{days_between_changes}{Number of days between timestamps}
#'   }
#'
#' Results are ordered from the largest to smallest time difference.
#'
#' @examples
#' delayed_data_changes <- identify_individual_data_changes(
#'   atr_data = atr_data,
#'   metadata = metadata
#' )
#'
#' @export

time_between_changes <- function(
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
      !grepl(
        "Assign record to Data Access Group|Data Access Group",
        field,
        ignore.case = TRUE
      )
    ) %>%
    mutate(
      old_timestamp = as.POSIXct(old_timestamp),
      new_timestamp = as.POSIXct(new_timestamp),
      days_between_changes = as.numeric(
        difftime(new_timestamp, old_timestamp, units = "days")
      )
    ) %>%
    filter(
      days_between_changes > 1
    ) %>%
    select(
      record,
      event,
      field,
      old_value,
      new_value,
      reason,
      days_between_changes
    ) %>%
    arrange(
      desc(days_between_changes),
      record,
      event,
      field
    )
}
