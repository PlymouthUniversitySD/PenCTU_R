# Author: Paigan Aspinall and Matthew Bailey
# Date: 31AUG2023
# R version: 4.2.2

library(dplyr) #1.1.0
library(ggplot2) #3.4.2
library(tidyr) #1.3.0

#' Generate a plot of recruitment at each site in a study.
#'
#' This function plots the recruitment at each site against the month and year; the sites can be defined or pulled from the data 
#' the dates can be up to today or up until the end of recruitment.
#'
#' @param recruitment_start A date which is the start date of recruitment, given in "2023-01-01" format.
#' @param enrollment_date_data The column in the dataset where the enrollment date for each participant is defined, given in 
#' format "dataset$enrollment_date".
#' @param site_data The column in the dataset where the participant's site is defined, given in format "dataset$site".
#' @param site_names A vector where the study sites can be defined as "c("Site 1", "Site2"), if left as NULL, these will be auto
#' populated from the site_data column.
#' @param recruitment_end A date which is the end date of recruitment, given in "2023-01-01" format. If left blank is auto 
#' populated as today.
#' 
#'
#' @return A ggplot of recruitment at each site.
#'
#'
#' @importFrom dplyr "%>%"
#'
#' @export
#' 

plot_recruitment_by_site <- function(recruitment_start, enrollment_date_data, site_data, site_names=NULL, recruitment_end=Sys.Date()){

  recruitment_start <- as.Date(recruitment_start)
  recruitment_end <- as.Date(recruitment_end)
  enrollment_date_data <- as.Date(enrollment_date_data)
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  plot_data <- data.frame()
  
  if(is.null(site_names)){ 
    site_names <- unique(na.omit(site_data))
  }
  
  recruitment_length_months <- ceiling(difftime( recruitment_end, recruitment_start, units = "days") / 30.44)
  
  enrollment_data <- tibble(enrollment_date_data = enrollment_date_data, site_data = site_data) %>%
    mutate(year_month = format(enrollment_date_data, "%Y-%m")) %>% 
    filter(!is.na(enrollment_date_data)) %>%
    count(site_data, year_month, name = "count") %>%
    arrange(site_data, as.Date(paste0(year_month, "-01")))
  
  for (site_name in site_names) {
      month_count_site <- enrollment_data %>%
      filter(site_data == site_name)
      recruits_site <- data.frame(year_month = seq(recruitment_start, by = "1 month", length.out = recruitment_length_months),
                                count = 0,
                                Site = site_name) %>% 
      mutate(year_month = format(year_month, "%Y-%m"))
      recruits_site$count[recruits_site$year_month %in% month_count_site$year_month] <- month_count_site$count
      plot_data <- rbind(plot_data, recruits_site)
  }
  
   plot_data <- plot_data %>%
    arrange(Site, year_month) %>%
    group_by(Site) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
    mutate(cumulative_count = ifelse(year_month>today_date,NA,cumulative_count))
  
  recruitment_by_site_plot<-ggplot(data = plot_data, aes(x = year_month, y = cumulative_count, group = Site)) +
    geom_line(aes(color = Site), size = 2) +
    geom_point(aes(color = Site, shape = Site), size = 3) +
    labs(x = "Month", y = "Number enrolled", title = "Participant enrollment by site") +
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"))+
    theme_bw()
}
