# Author: Paigan Aspinall and Matthew Bailey
# Date & version: 31AUG2023 V1.1.0
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
#' @param enrolment_date_data The column in the dataset where the enrolment date for each participant is defined, given in 
#' format "dataset$enrolment_date".
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
#' @example 
#' Example usage:
#' site_names <- c("Central and North West London", "Cheshire and Wirral Partnership", "Leicestershire Partnership", "Swansea Bay University Health Board", "Cornwall Partnership")
#' recruitment_by_site <- plot_recruitment_by_site("2023-11-30", dataset$recruitment_date, dataset$Site, site_names, recruitment_end="2024-04-01")
#'
#' @export
#' 
plot_recruitment_by_site <- function(recruitment_start, enrolment_date_data, site_data, site_names=NULL, recruitment_end=Sys.Date()){
  
  # Convert parameters to appropriate date formats
  recruitment_start <- as.Date(recruitment_start)
  recruitment_end <- as.Date(recruitment_end)
  enrolment_date_data <- as.Date(enrolment_date_data)
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Initialize an empty dataframe to store plot data
  plot_data <- data.frame()
  
  # If site_names is NULL, generate it from unique non-NA values in site_data
  if (is.null(site_names)) {
    site_names <- unique(na.omit(site_data))
  }
  
  # Calculate recruitment length in months
  recruitment_length_months <- ceiling(difftime(recruitment_end, recruitment_start, units = "days") / 30.44)
  
  # Prepare enrolment data for plotting
  enrolment_data <- tibble(enrolment_date_data = enrolment_date_data, site_data = site_data) %>%
    mutate(year_month = format(enrolment_date_data, "%Y-%m")) %>% 
    filter(!is.na(enrolment_date_data)) %>%
    count(site_data, year_month, name = "count") %>%
    arrange(site_data, as.Date(paste0(year_month, "-01")))
  
  # Iterate over each site to generate plot data
  for (site_name in site_names) {
    month_count_site <- enrolment_data %>%
      filter(site_data == site_name)
    recruits_site <- data.frame(year_month = seq(as.Date(format(recruitment_start, "%Y-%m-01")), 
                                                 by = "1 month", 
                                                 length.out = recruitment_length_months),
                                count = 0,
                                Site = site_name)%>% 
      mutate(year_month = format(year_month, "%Y-%m"))
    recruits_site$count[recruits_site$year_month %in% month_count_site$year_month] <- month_count_site$count
    recruits_site$cumulative_count <- cumsum(recruits_site$count)
    plot_data <- rbind(plot_data, recruits_site)
  }
  
  # Define custom colors for sites
  custom_colors <- c("#F8766D", "#00BFC4", "#CD9600", "#C77CFF","#7CAE00","#FB61D7","#619CFF","#00BA38","#A3A500")
  
  # Prepare plot data for plotting
  plot_data <- plot_data %>%
    arrange(Site, year_month) %>%
    group_by(Site) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
    mutate(cumulative_count = ifelse(year_month > as.Date(today_date), NA, cumulative_count))
  
  # Generate recruitment by site plot
  recruitment_by_site_plot <- ggplot(data = plot_data, aes(x = year_month, y = cumulative_count, group = Site)) +
    geom_line(aes(color = Site), size = 2) +
    geom_point(aes(color = Site, shape = Site), size = 3) +
    labs(x = "Month", y = "Number recruited", title = "Patients recruited by site") +
    scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"))+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust x-axis labels angle
  
  # Return recruitment by site plot
  return(recruitment_by_site_plot)
}