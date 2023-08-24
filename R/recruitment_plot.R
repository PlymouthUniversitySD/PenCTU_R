# Author: Matthew Bailey and Paigan Aspinall
# Date: 24AUG2023
# R version: 4.2.2

library(dplyr) #1.1.0
library(ggplot2) #3.4.2
library(tidyr) #1.3.0

#' Generate an actual versus target recruitment plot.
#'
#' This function plots the actual recruitment of a study and the target recruitment against the length of the study.
#'
#' @param data A data frame to collect enrollment date data from.
#' @param recruitment_start A date which is the start date of recruitment, given in "2023-01-01" format.
#' @param recruitment_length_months A number which is the number of months that recruitment is open.
#' @param recruitment_target A number which is the target number of participants to be recruited.
#' @param enrollment_date_column A column which is where the enrollment date of participants is defined, given in "column_name" format.
#'
#' @return A ggplot of expected and actual recruitment.
#'
#'
#' @importFrom dplyr "%>%"
#'
#' @export
#' 

recruitment_plot <- function(data, recruitment_start, recruitment_length_months, recruitment_target, enrollment_date_column) {
  
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  data[[enrollment_date_column]] <- as.Date(data[[enrollment_date_column]])
  recruitment_start <- as.Date(recruitment_start)
  
  enrollment_data <- data %>% 
    mutate(year_month = format(.data[[enrollment_date_column]], "%Y-%m")) %>% 
    filter(!is.na(.data[[enrollment_date_column]])) %>%
    count(year_month, name="count") %>%
    arrange(as.Date(paste0(year_month, "-01")))
  
  plot_data <- data.frame(year_month=seq(recruitment_start, by = "1 month", length.out = recruitment_length_months))  %>% 
    mutate(year_month = format(year_month, "%Y-%m")) %>%
    left_join(., enrollment_data, by = "year_month") %>%
    mutate(count=replace_na(count, 0)) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
    mutate(cumulative_count = ifelse(year_month>today_date,NA,cumulative_count))
           print(plot_data)
  
  recruitment_plot <- ggplot(plot_data, aes(x = year_month, y = cumulative_count)) +
    geom_line(aes(color = "Actual"), size = 1) +
    geom_point(aes(color = "Actual"), size = 3) +
    geom_segment(aes(x = year_month[1], y = recruitment_target/recruitment_length_months, xend = tail(year_month, n=1), yend = recruitment_target, color = "Target"), linetype = "dashed") +
    ylab("Recruitment") + xlab("Date") +
    geom_text(aes(label=cumulative_count), vjust=-0.7)+
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y")) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, (recruitment_target+10)), breaks=seq(0, 100, 10)) +
    scale_color_manual(name = "Key:", values = c("Actual" = "blue", "Target" = "red"), labels = c("Actual recruitment", "Target recruitment")) +
    theme_bw()
  
  recruitment_plot
  
}
