#Title: ATR - Identify changes per user per month
#Author: Paigan Aspinall
#Version & Date: V1.1.0 12AUG2026
#R version: 4.4.3

#' Identify Monthly User Data Changes
#'
#' Summarises the number of qualifying data changes made by each user per month
#' within the audit trail dataset. Output is returned in wide format with one
#' row per user and one column per month.
#'
#' A qualifying change is identified where:
#' \itemize{
#'   \item \code{action == "Update record"}
#'   \item \code{old_value} is not missing
#'   \item \code{old_value != "NA"}
#'   \item \code{old_value != new_value}
#' }
#'
#' The following usernames are excluded:
#' \itemize{
#'   \item \code{"SYSTEM"}
#'   \item \code{"[survey respondent]"}
#'   \item Missing usernames
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
#' }
#'
#' Monthly summaries are derived from the \code{timestamp} field and returned
#' in \code{YYYY-MM} format. Month columns are ordered chronologically from
#' earliest to latest.
#'
#' @param atr_data A data frame containing REDCap audit trail data. Expected
#'   columns include:
#'   \describe{
#'     \item{timestamp}{Date/time of the audit action}
#'     \item{username}{Username associated with the audit action}
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
#'     \item{username}{Username associated with qualifying changes}
#'     \item{YYYY-MM}{One column per month containing the number of qualifying
#'     changes, ordered chronologically}
#'   }
#'
#' @examples
#' monthly_user_changes <- identify_monthly_user_data_changes(
#'   atr_data = atr_data,
#'   metadata = metadata
#' )
#'
#' @export
identify_monthly_user_data_changes <- function(
    atr_data,
    metadata
) {
  
  library(dplyr)
  library(tidyr)
  
  # Identify fields to exclude
  excluded_fields <- metadata %>%
    filter(
      grepl(
        "#DataManagement|#SystemFunctionality|#TrialManagement",
        field_annotation
      )
    ) %>%
    pull(field_name)
  
  # Identify qualifying changes and summarise by user and month
  monthly_changes <- atr_data %>%
    filter(
      !is.na(username),
      username != "SYSTEM",
      username != "[survey respondent]",
      action == "Update record",
      !is.na(old_value),
      old_value != "NA",
      old_value != new_value,
      !field %in% excluded_fields,
      !grepl("_complete$", field)
    ) %>%
    mutate(
      timestamp = as.POSIXct(timestamp),
      month = format(timestamp, "%Y-%m")
    ) %>%
    group_by(
      username,
      month
    ) %>%
    summarise(
      n_changes = n(),
      .groups = "drop"
    ) %>%
    arrange(month)
  
  # Convert to wide format
  monthly_user_changes <- monthly_changes %>%
    pivot_wider(
      names_from = month,
      values_from = n_changes,
      values_fill = 0,
      names_sort = TRUE
    ) %>%
    arrange(username)
  
  return(monthly_user_changes)
}