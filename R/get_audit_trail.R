#Title: Get processed audit trail data
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026
#R version: 4.4.3

#' Download and process REDCap audit trail data
#'
#' This function retrieves REDCap audit trail (log) data via the REDCap API
#' and processes "Update record" and "Update Response" actions into a
#' structured row-level format. Repeating instrument instances are extracted
#' where present, and field-level changes are separated into old and new
#' values with corresponding timestamps.
#'
#' Processed audit trail data can optionally be exported as a CSV file.
#'
#' @param token REDCap API token with permission to access log data.
#' @param study_name Character string used when naming the exported CSV file.
#' @param begin_time Start datetime for log extraction in `"YYYY-MM-DD HH:MM"`
#'   format.
#' @param end_time Optional end datetime for log extraction in
#'   `"YYYY-MM-DD HH:MM"` format. Defaults to all available records after
#'   `begin_time`.
#' @param url REDCap API URL.
#' @param write_file Logical indicating whether to export the processed audit
#'   trail as a CSV file.
#'
#' @return A processed audit trail dataframe containing:
#' \itemize{
#'   \item Original audit log variables
#'   \item Extracted repeating instance numbers
#'   \item Event names
#'   \item Field names
#'   \item Old and new values
#'   \item Old and new timestamps
#' }
#'
#' @details
#' "Update record" and "Update Response" log entries are expanded so each
#' modified field is represented as a separate row. This allows easier
#' downstream analysis of audit trail activity and field-level changes.
#'
#' @importFrom dplyr %>% filter mutate arrange group_by ungroup select
#'   summarise bind_rows all_of lag coalesce
#' @importFrom tidyr unnest extract
#' @importFrom stringr str_detect str_trim str_extract str_remove
#'   str_remove_all str_split
#' @importFrom lubridate parse_date_time
#' @importFrom httr POST content
#'
#' @examples
#' audit_data <- get_audit_trail(
#'   token = token,
#'   study_name = "ExampleStudy"
#' )
#'
#' @export

get_audit_trail <- function(token,
                            study_name,
                            begin_time = "",
                            end_time = "",
                            url = "https://clinicaltrials.plymouth.ac.uk/api/",
                            write_file = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(httr)
  
  formData <- list(
    token = token,
    content = "log",
    logtype = "",
    user = "",
    record = "",
    beginTime = begin_time,
    endTime = end_time,
    format = "csv",
    returnFormat = "json"
  )
  
  response <- httr::POST(url, body = formData, encode = "form")
  audit_data <- httr::content(response)
  
  process_updates <- function(data, action_pattern, action_label) {
    data %>%
      filter(str_detect(action, action_pattern)) %>%
      filter(!(is.na(details) & is.na(reason))) %>%
      filter(
        !str_detect(
          str_trim(coalesce(details, "")),
          "^\\[instance\\s*=\\s*\\d+\\]$"
        )
      ) %>%
      mutate(
        instance = str_extract(details, "^\\[instance\\s*=\\s*\\d+\\]") %>%
          str_extract("\\d+") %>%
          as.integer(),
        details = str_remove(details, "^\\[instance\\s*=\\s*\\d+\\],\\s*"),
        event = str_extract(action, "(?<=\\().*(?=\\))"),
        action = action_label,
        details_split = str_split(
          details,
          ",\\s*(?=[A-Za-z0-9_]+(?:\\([0-9]+\\))?\\s*=)"
        )
      ) %>%
      unnest(details_split) %>%
      filter(!is.na(details_split)) %>%
      filter(str_detect(details_split, "=")) %>%
      extract(
        details_split,
        into = c("field", "value"),
        regex = "^\\s*([^=]+?)\\s*=\\s*(.*)\\s*$",
        remove = TRUE
      ) %>%
      mutate(
        field = str_trim(field),
        value = str_trim(value),
        value = str_remove_all(value, "^'|'$"),
        timestamp = parse_date_time(timestamp, orders = c("ymd HMS", "ymd HM", "ymd"))
      ) %>%
      arrange(record, event, instance, field, timestamp) %>%
      group_by(record, event, instance, field) %>%
      mutate(
        old_value = lag(value),
        new_value = value,
        old_timestamp = lag(timestamp),
        new_timestamp = timestamp
      ) %>%
      ungroup() %>%
      select(-value)
  }
  
  align_columns <- function(x, template) {
    missing_cols <- setdiff(names(template), names(x))
    for (col in missing_cols) {
      x[[col]] <- NA
    }
    x %>% select(all_of(names(template)))
  }
  
  updated_records <- process_updates(audit_data, "Update record", "Update record")
  
  audit_data <- audit_data %>%
    filter(!str_detect(action, "Update record")) %>%
    mutate(
      instance = NA_integer_,
      event = NA_character_,
      field = NA_character_,
      old_value = NA_character_,
      new_value = NA_character_,
      old_timestamp = as.POSIXct(NA),
      new_timestamp = as.POSIXct(NA)
    )
  
  updated_records <- align_columns(updated_records, audit_data)
  audit_data <- bind_rows(audit_data, updated_records)
  
  updated_responses <- process_updates(audit_data, "Update Response", "Update response")
  updated_responses <- align_columns(updated_responses, audit_data)
  
  audit_data <- audit_data %>%
    filter(!str_detect(action, "Update Response"))
  
  audit_data <- bind_rows(audit_data, updated_responses)
  
  if (write_file) {
    doc_title <- paste0(study_name, "_AuditTrail_", Sys.Date(), ".csv")
    write.csv(audit_data, doc_title, row.names = FALSE)
  }
  
  return(audit_data)
}
