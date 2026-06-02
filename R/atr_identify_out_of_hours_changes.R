#Title: ATR - Identify out of horus changes
#Author: Paigan Aspinall
#Version & Date: V1.0.0 22MAY2026
#R version: 4.4.3

#' Identify Out-of-Hours Data Changes
#'
#' Identifies qualifying data changes that occurred outside standard working
#' hours. Out-of-hours changes are defined as changes occurring on weekends,
#' before 09:00, or after 17:00.
#'
#' A qualifying change is identified where:
#' \itemize{
#'   \item \code{action == "Update record"}
#'   \item \code{old_value} is not missing
#'   \item \code{old_value != "NA"}
#'   \item \code{old_value != new_value}
#' }
#'
#' @export
identify_out_of_hours_changes <- function(atr_data) {
  
  atr_data %>%
    mutate(
      timestamp = as.POSIXct(timestamp),
      weekday = weekdays(timestamp),
      hour = as.numeric(format(timestamp, "%H"))
    ) %>%
    filter(
      action == "Update record",
      !is.na(old_value),
      old_value != "NA",
      old_value != new_value,
      weekday %in% c("Saturday", "Sunday") | hour < 8 | hour >= 18
    ) %>%
    select(
      timestamp,
      username,
      record,
      event,
      field,
      old_value,
      new_value,
      reason,
      weekday,
      hour
    ) %>%
    arrange(
      desc(timestamp),
      username,
      record,
      event,
      field
    )
}
