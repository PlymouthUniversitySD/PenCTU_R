# Author: Paigan Aspinall 
# Date & version: 28NOV2024 V1.0.0
# R version: 4.4.2

library(dplyr) #1.1.0
library(ggplot2) #3.4.2
library(tidyr) #1.3.0

#' Generate a plot of recruitment at each site in a study. Updated version of plot_recruitment_by_site which allows sites to have different start dates.
#'
#' This function plots the recruitment at each site against the month and year; the sites can be defined or pulled from the data 
#' the dates can be up to today or up until the end of recruitment.
#'
#' @param recruitment_start A date which is the start date of recruitment, given in "2023-01-01" format.
#' @param site_opening_dates A vector where the opening date of each site is defined (e.g. site_opening_dates <- c("Plymouth"="2024-09-03", "Manchester"="2025-06-01"))
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
#' @examples
#' Example usage:
#' site_names <- c("Plymouth","Manchester") 
#' site_opening_dates <- c("Plymouth"="2024-09-03", "Manchester"="2025-06-01")
#' recruitment_by_site <- plot_recruitment_by_site("2024-09-03", site_opening_dates, dataset$recruitment_date, dataset$Site, site_names, recruitment_end="2024-04-01")
#'
#' @export
#' 
plot_recruitment_by_site_2 <- function(recruitment_start, site_opening_dates, enrolment_date_data, site_data, site_names=NULL, recruitment_end=Sys.Date()){
  if(is.null(recruitment_start)) {
    stop("Start date of recruitment not provided!")
  }
  
  if(is.null(site_opening_dates)) {
    stop("Site opening dates not provided!")
  }
  
  if(is.null(enrolment_date_data)) {
    stop("Enrolment date data not provided!")
  }
  
  if(is.null(site_data)) {
    stop("Site data not provided!")
  }
  
  # Convert parameters to appropriate date formats
  recruitment_start <- as.Date(recruitment_start)
  recruitment_end <- as.Date(recruitment_end)
  enrolment_date_data <- as.Date(enrolment_date_data)
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  if(!grepl("^\\d{4}-\\d{2}-\\d{2}$", as.Date(recruitment_start, format="%Y-%m-%d"))) {
    stop("Recruitment start date was given in incorrect format")
  }
  
  if(!is.null(recruitment_end) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.Date(recruitment_end, format="%Y-%m-%d"))) {
    stop("Recruitment end date was given in incorrect format")
  }
  
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
    # Filter enrolment data for the current site
    month_count_site <- enrolment_data %>%
      filter(site_data == site_name)
    
    if(!(site_name %in% month_count_site$site_data)) {
      stop(paste0(site_name, " not found in site data!"))
    }
    
    # Ensure recruitment_start is a Date object and generate the date sequence
    date_sequence <- seq(as.Date(format(recruitment_start, "%Y-%m-01")), 
                         by = "1 month", 
                         length.out = recruitment_length_months)
    
    # Check for any NA values in the sequence
    if (any(is.na(date_sequence))) stop("Date sequence contains NA values")
    
    # Create recruits_site
    recruits_site <- data.frame(year_month = format(date_sequence, "%Y-%m"),
                                count = 0,
                                Site = site_name,
                                stringsAsFactors = FALSE) 
    
    # Match counts from month_count_site
    recruits_site$count[recruits_site$year_month %in% month_count_site$year_month] <- month_count_site$count
    recruits_site$cumulative_count <- cumsum(recruits_site$count)
    
    # Append to plot_data
    plot_data <- rbind(plot_data, recruits_site)
  }
  
  # Ensure year_month in plot_data is a Date object for comparison
  plot_data$year_month <- as.Date(paste0(plot_data$year_month, "-01"))
  
  # Convert site_opening_dates to Date format
  site_opening_dates <- as.Date(site_opening_dates)
  
  # Iterate over each site and adjust cumulative_count
  for (site_name in names(site_opening_dates)) {
    opening_date <- site_opening_dates[site_name]
    
    # Calculate the first day of the opening month
    opening_month_start <- as.Date(format(opening_date, "%Y-%m-01"))
    
    # Update cumulative_count to NA for rows before the opening month
    plot_data$cumulative_count[plot_data$Site == site_name & 
                                 plot_data$year_month < opening_month_start] <- NA
  }
  
  plot_data$year_month <- format(plot_data$year_month, "%Y-%m")
  
  
  # Define custom colors for sites
  custom_colors <- c("#F8766D", "#00BFC4", "#CD9600", "#C77CFF","#7CAE00","#FB61D7","#619CFF","#00BA38","#A3A500")
  
  plot_data$year_month <- as.Date(paste0(plot_data$year_month, "-01"))
  
  # Create the plot
  ggplot(plot_data, aes(
    x = year_month, 
    y = cumulative_count, 
    group = Site, 
    color = Site
  )) +
    geom_line() +  # Add lines to the plot for each site
    geom_point() + # Add points to emphasize data points
    labs(
      title = "Patients Recruited by Site", # Main title of the plot
      x = "Date",                          # X-axis label
      y = "Number Recruited",              # Y-axis label
      color = "Site"                       # Legend title
    ) +
    scale_color_manual(values = custom_colors) + # Apply custom color palette to the sites
    scale_x_date(
      date_labels = "%Y-%m",               # Format x-axis labels to show Year-Month
      date_breaks = "1 month"             # Set breaks to one-month intervals
    ) +
    scale_y_continuous(
      breaks = seq(0, max(plot_data$cumulative_count, na.rm = TRUE), by = 1), # Ensure only integers on y-axis
      expand = c(0, 0)                     # Remove extra space at the ends of y-axis
    ) +
    theme_minimal() +                      # Apply a clean and minimal theme
    theme(
      axis.text.x = element_text(
        angle = 45,                       # Rotate x-axis labels by 45 degrees
        hjust = 1                         # Adjust horizontal justification for readability
      )
    )
  
}
