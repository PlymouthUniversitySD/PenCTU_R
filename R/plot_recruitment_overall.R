# Author: Matthew Bailey and Paigan Aspinall
# Date & version: 24AUG2023 V1.1.0
# R version: 4.2.2

library(dplyr) #1.1.0
library(ggplot2) #3.4.2
library(tidyr) #1.3.0

#' Generate an actual versus target recruitment plot.
#'
#' This function plots the actual recruitment of a study and the target recruitment against the length of the study.
#'
#' @param data A data frame to collect enrolment date data from.
#' @param recruitment_start A date which is the start date of recruitment, given in "2023-01-01" format.
#' @param recruitment_length_months A number which is the number of months that recruitment is open.
#' @param recruitment_target A number which is the target number of participants to be recruited.
#' @param enrollment_date_column A column which is where the enrolment date of participants is defined, given in "column_name" format.
#'
#' @return A ggplot of expected and actual recruitment.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples
#' Example usage:
#' recruitment_plot <- plot_recruitment_overall(dataset, "2023-11-30", 9, 400, "date")
#' 
#'
#' @export
#' 

plot_recruitment_overall <- function(data, recruitment_start, recruitment_length_months, recruitment_target, enrollment_date_column) {
  
  # Get today's date
  today_date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
  
  # Convert date values to date format
  data[[enrollment_date_column]] <- as.Date(data[[enrollment_date_column]])
  recruitment_start <- as.Date(recruitment_start)
  # Calculate the end date of recruitment
  recruitment_end <- recruitment_start + months(recruitment_length_months)
  
  # Prepare enrolment data
  enrollment_data <- data %>% 
    # Create a year-month column
    mutate(year_month = format(.data[[enrollment_date_column]], "%Y-%m")) %>% 
    # Remove NA values from date column
    filter(!is.na(.data[[enrollment_date_column]])) %>%
    # Count enrolments per year-month
    count(year_month, name="count") %>%
    # Arrange by date
    arrange(as.Date(paste0(year_month, "-01")))
  
  # Generate a sequence of year-months from recruitment start
  year_months <- seq(from = floor_date(recruitment_start, unit = "month"), 
                     to = floor_date(today_date, unit = "month"), by = "1 month")
  year_months <- format(year_months, "%Y-%m")
  # Create a dataframe with all year-months
  all_months_data <- data.frame(year_month = year_months)
  # Left join enrollment data with all months data
  enrollment_data <- all_months_data %>%
    left_join(enrollment_data, by = "year_month") %>%
    # Replace NA values with 0
    mutate(count = replace_na(count, 0))
  # Calculate a cumulative count of enrollments
  enrollment_data <- enrollment_data %>%
    mutate(cumulative_count = cumsum(count))
  
  # Convert year-month column into date format
  enrollment_data$year_month <- as.Date(paste0(enrollment_data$year_month, "-01"), format = "%Y-%m-%d")
  
  # Determine pretty y-axis breaks
  y_breaks <- pretty(c(0, recruitment_target))
  
  # Generate the recruitment plot
  recruitment_plot <- ggplot(enrollment_data, aes(x = year_month, y = cumulative_count)) +
    geom_line(aes(color = "Actual"), size = 1) +
    geom_point(aes(color = "Actual"), size = 3) +
    geom_segment(aes(x = year_month[1], y = recruitment_target/recruitment_length_months, xend = recruitment_end, yend = recruitment_target, color = "Target"), linetype = "dashed") +
    ylab("Recruitment") + xlab("Date") +
    geom_text(aes(label = cumulative_count), vjust = -0.7) +
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(y_breaks)), breaks = y_breaks) +
    scale_color_manual(name = "Key:", values = c("Actual" = "#F8766D", "Target" = "#00B0F6"), labels = c("Actual recruitment", "Target recruitment")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  recruitment_plot
}
