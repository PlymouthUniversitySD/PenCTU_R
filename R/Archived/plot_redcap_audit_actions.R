# Author: Paigan Aspinall
# Date: 10JAN2024
# R version: 4.2.2

#' Create a line plot of record-related actions over time.
#'
#' This function uses audit log data to plot the number of actions taken over time.
#' 
#' @param audit_data The REDCap dataset containing the audit log.
#' @param date_breaks Specifies the increments used on the x-axis. Can be set as "day" or "month".
#' @param start_date The date from which the graph should commence, optional.
#' @param end_date The date at which the graph should end, optional.
#'
#' @return A plot of actions by day.
#'
#'
#' @importFrom dplyr "%>%"
#' 
#' @examples:
#' Example usage:
#' 
#' audit_plot <- plot_redcap_audit_actions(audit_data, date_breaks = "month", start_date= "2023-10-01", end_date=NULL )
#'
#' @export
#'

plot_redcap_audit_actions <- function(audit_data, date_breaks, start_date=NULL, end_date=NULL){

  #remove specific from action names, e.g. record ID, user emails
  audit_data$action <- gsub("[0-9]", "", audit_data$action)
  audit_data$action <- sub("Manage/Design.*", "Manage/Design", audit_data$action)
  audit_data$action <- sub("Enable external module.*", "Enable external module", audit_data$action)
  audit_data$action <- sub("Add user.*", "Add user", audit_data$action)
  audit_data$action <- sub("User assigned to role.*", "User assigned to role", audit_data$action)
  audit_data$action <- sub("-.*", "", audit_data$action)
  audit_data$action <- sub("Modify configuration for external module.*", "Modify configuration for external module", audit_data$action)
  audit_data$action <- sub("Could not generate record number due to missing required data: could not detect group_id Record .*", "Record number error", audit_data$action)
  audit_data$action <- sub("Create record.*", "Create record", audit_data$action)
  audit_data$action <- sub("Update record.*", "Update record", audit_data$action)
  audit_data$action <- sub("Update Response.*", "Update Response", audit_data$action)
  audit_data$action <- sub("Data export.*", "Data export", audit_data$action)
  
  #set update record actions where a reason is not given as data entry
  audit_data$action <- ifelse(audit_data$action == "Update record" & is.na(audit_data$reason), 
                           "Record data entry", 
                           audit_data$action)
  
  #extract date from time stamp column
  audit_data$Date <- as.Date(audit_data$timestamp)
  
  #if a start date is being used filter data to remove dates before start date
  if(!is.null(start_date)){
    start_date <- as.Date(start_date)
    audit_data <- subset(audit_data, Date > start_date)
  }
  
  #if an end date is being used filter data to remove dates after end date
  if(!is.null(end_date)){
    start_date <- as.Date(end_date)
    audit_data <- subset(audit_data, Date < end_date)
  }
  
  #rename action column
  names(audit_data)[names(audit_data) == "action"] <- "Action"
  
  #select only actions of interest
  audit_data <- subset(audit_data, Action == "Create record"|Action == "Delete record "|Action == "Lock/Unlock Record "|Action == "Randomize Record "
                       |Action == "Record data entry"|Action == "Update record")
  
  #rename actions to a consistent format
  new_values <- c("Create record", "Delete record", "Lock/unlock record", "Randomise record", "Record data entry", "Update record")
  old_values <- c("Create record", "Delete record ", "Lock/Unlock Record ", "Randomize Record ", "Record data entry", "Update record")
  audit_data$Action <- new_values[match(audit_data$Action, old_values)]
  
  #group actions for plot
   action_counts <- audit_data %>%
    group_by(Date, Action) %>%
    summarise(count = n())
  
   #set colour palette
  action_colors <- c("Create record" = "#09eb18", 
                     "Create response" = "#ffdd00", 
                     "Delete record" = "#113d14", 
                     "Lock/unlock record" = "#9513d1", 
                     "Randomise record" = "#f500ab", 
                     "Record data entry" = "#ff0400", 
                     "Update record" = "#2800ff", 
                     "Update response" = "#4a0606")
  
  #plot the data with month increments
  if(date_breaks == "month"){
    return(
      ggplot(action_counts, aes(x = Date, y = count, color = Action)) +
        geom_line(size=0.75) +
        scale_color_manual(values = action_colors) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(x = "Date", y = "Number of Actions", title = "Actions per Day") +
        theme_minimal()
    )
  }
  
  #plot the data with day increments
  if(date_breaks == "day"){
    return(
      ggplot(action_counts, aes(x = Date, y = count, color = Action)) +
        geom_line(size = 0.75) +
        scale_color_manual(values = action_colors) +
        scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
        labs(x = "Date", y = "Number of Actions", title = "Actions per Day") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
}
