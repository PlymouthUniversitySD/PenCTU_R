# Author: Paigan Aspinall
# Date: 12JAN2024
# R version: 4.2.2

#' Produce a histograph of time lag.
#'
#' This function calculate the difference between two specified date columns and plots a histogram of the differences.
#' 
#' @param dataset Dataset from which time lag is to calculated.
#' @param lag_type Specifies whether the time lag will be calculated in days or months.
#' @param start_column Column where first date is specified. 
#' @param start_event Name of event where first date is located. 
#' @param end_column Column where second date is specified.
#' @param end_event Name of event where second event is specified. 
#' @param plot_name Title of the plot.
#'
#' @return A histograph of difference between two dates.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example usage:
#' 
#' plot <- plot_time_lag(dataset, "months", "hoappointdat", "week_0_blinded_ass_arm_1", "hoappnextdat", "week_0_blinded_ass_arm_1", "Time until next scheduled appointment")
#'
#' @export
#'

plot_time_lag <- function(dataset, lag_type, start_column, start_event, end_column, end_event, plot_name) {

  #create a blank dataset with the required columns
  new_dataset <- data.frame(record_id = character(),
                            start_value = character(),
                            end_value = character(),
                            stringsAsFactors = FALSE)
  
  
  for (record_id in unique(dataset$record_id)) {
    #filter rows for the current record_id
    record_data <- dataset[dataset$record_id == record_id, ]
    
    #extract start_value and end_value based on conditions
    start_value <- record_data[[start_column]][record_data$redcap_event_name == start_event]
    end_value <- record_data[[end_column]][record_data$redcap_event_name == end_event]
    
    #apend to the new dataset if values are not NA
    if (!all(is.na(start_value)) && !all(is.na(end_value))) {
      new_dataset <- rbind(new_dataset, data.frame(record_id, start_value, end_value))
    }
  }
  
  if (lag_type == "days") {
    #calculate the date difference in days
    new_dataset$time_lag <- as.numeric(difftime(new_dataset$end_value, new_dataset$start_value, units = "days"))
    
    #calculate the required bin width based on Freedman-Diaconis rule
    fd_binwidth <- diff(quantile(new_dataset$time_lag, c(0.25, 0.75))) / (2 * IQR(new_dataset$time_lag)^(1/3))
    
    #use ggplot2 with the calculated bin width
    plot <- ggplot(new_dataset, aes(x = time_lag)) +
      geom_histogram(binwidth = fd_binwidth, fill = "#ffdd00", color = "black", alpha = 0.7) +
      labs(title = plot_name, x = "Days", y = "Frequency") +
      theme_minimal()
    
    return(plot)
    
  } else if (lag_type == "months") {
    #calculate the date difference in months
    new_dataset$time_lag <- as.numeric(difftime(new_dataset$end_value, new_dataset$start_value, units = "weeks") / 4.34812141)
    
    #calculate the required bin width based on Freedman-Diaconis rule
    fd_binwidth <- diff(quantile(new_dataset$time_lag, c(0.25, 0.75))) / (2 * IQR(new_dataset$time_lag)^(1/3))
    
    #use ggplot2 with the calculated bin width
    plot <- ggplot(new_dataset, aes(x = time_lag)) +
      geom_histogram(binwidth = fd_binwidth, fill = "#ffdd00", color = "black", alpha = 0.7) +
      labs(title = plot_name, x = "Months", y = "Frequency") +
      theme_minimal()
    return(plot)
  
  }
}
  
