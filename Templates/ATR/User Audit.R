#Title: REDCap User Audit
#Author: Paigan Aspinall
#Version & Date: V1.0.0 16JUN2026
#R version: 4.4.3

#Set the document author name
document_author <- "Paigan Aspinall"

#Import the user list
user_list <- read.csv("UserList.csv")

#Import libraries
library(dplyr)
library(lubridate)
library(openxlsx)
library(tidyr)
library(ggplot2)

#Set today's date
today <- Sys.Date()

#Clean date columns and filter suspended users
user_list_clean <- user_list %>%
  mutate(
    Last.Login = as.Date(Last.Login),
    Last.Activity = as.Date(Last.Activity)
  ) %>%
  filter(Time.of.suspension == "N/A")

#Columns to remove from all output reports
cols_to_remove <- c(
  "Administrator",
  "Time.of.suspension"
)

#1. Users not logged in during the last 3 months
not_logged_3_months <- user_list_clean %>%
  filter(is.na(Last.Login) | Last.Login < today %m-% months(3)) %>%
  select(-any_of(cols_to_remove))

#2. Logged in within last 3 months, but last activity more than 3 months ago
logged_3_months_activity_old <- user_list_clean %>%
  filter(
    Last.Login >= today %m-% months(3),
    is.na(Last.Activity) | Last.Activity < today %m-% months(3)
  ) %>%
  select(-any_of(cols_to_remove))

#Create study-level summary data
study_summary_base <- user_list_clean %>%
  mutate(
    Inactive = is.na(Last.Login) | Last.Login < today %m-% months(3),
    Institution.ID = ifelse(is.na(Institution.ID) | Institution.ID == "", "Not recorded", Institution.ID)
  ) %>%
  separate_rows(Institution.ID, sep = ";") %>%
  mutate(Institution.ID = trimws(Institution.ID))

#1. Total users by study, active and inactive, excluding suspended users
total_users_by_study <- study_summary_base %>%
  count(Institution.ID, name = "Number.of.users") %>%
  arrange(desc(Number.of.users))

#2. Inactive users by study
inactive_users_by_study <- study_summary_base %>%
  filter(Inactive) %>%
  count(Institution.ID, name = "Number.of.inactive.users") %>%
  arrange(desc(Number.of.inactive.users))

#Function to add summary plots
add_summary_plots <- function(wb) {
  
  addWorksheet(wb, "Summary")
  
  # Dynamic x-axis limits
  total_users_xlim <- c(
    0,
    max(total_users_by_study$Number.of.users, na.rm = TRUE) * 1.15
  )
  
  inactive_users_xlim <- c(
    0,
    max(inactive_users_by_study$Number.of.inactive.users, na.rm = TRUE) * 1.15
  )
  
  # Total users by study
  par(mar = c(5, 14, 4, 2))
  
  barplot(
    rev(total_users_by_study$Number.of.users),
    names.arg = rev(total_users_by_study$Institution.ID),
    horiz = TRUE,
    las = 1,
    xlim = total_users_xlim,
    main = "Total users by study",
    xlab = "Number of users"
  )
  
  insertPlot(
    wb,
    sheet = "Summary",
    startRow = 2,
    startCol = 1,
    width = 11,
    height = 7
  )
  
  # Inactive users by study
  par(mar = c(5, 14, 4, 2))
  
  barplot(
    rev(inactive_users_by_study$Number.of.inactive.users),
    names.arg = rev(inactive_users_by_study$Institution.ID),
    horiz = TRUE,
    las = 1,
    xlim = inactive_users_xlim,
    main = "Inactive users by study (>3 months)",
    xlab = "Number of users"
  )
  
  insertPlot(
    wb,
    sheet = "Summary",
    startRow = 2,
    startCol = 14,
    width = 11,
    height = 7
  )
}

#Create workbook
wb <- createWorkbook()

#Styles
title_style <- createStyle(
  fgFill = "#7EC8E3",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fontSize = 14,
  border = "TopBottomLeftRight"
)

label_style <- createStyle(
  textDecoration = "bold",
  border = "TopBottomLeftRight"
)

cell_style <- createStyle(
  border = "TopBottomLeftRight"
)

header_style <- createStyle(
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  border = "TopBottomLeftRight"
)

#Cover sheet
addWorksheet(wb, "Cover")

mergeCells(wb, "Cover", cols = 1:3, rows = 1)
writeData(wb, "Cover", "REDCap User Audit", startCol = 1, startRow = 1)
addStyle(wb, "Cover", title_style, rows = 1, cols = 1:3, gridExpand = TRUE)

mergeCells(wb, "Cover", cols = 1:3, rows = 2)
writeData(wb, "Cover", "Template version & date: V.1.0.0 16JUN2026", startCol = 1, startRow = 2)
addStyle(wb, "Cover", cell_style, rows = 2, cols = 1:3, gridExpand = TRUE)

writeData(wb, "Cover", "Document author:", startCol = 1, startRow = 4)
writeData(wb, "Cover", document_author, startCol = 2, startRow = 4)

writeData(wb, "Cover", "Date of audit:", startCol = 1, startRow = 5)
writeData(wb, "Cover", format(today, "%d%b%Y"), startCol = 2, startRow = 5)

addStyle(wb, "Cover", label_style, rows = 4:5, cols = 1, gridExpand = TRUE)
addStyle(wb, "Cover", cell_style, rows = 4:5, cols = 2:3, gridExpand = TRUE)

setColWidths(wb, "Cover", cols = 1, widths = 28)
setColWidths(wb, "Cover", cols = 2:3, widths = 30)

action_header_style <- createStyle(
  fgFill = "#FFFF00",
  textDecoration = "bold",
  border = "TopBottomLeftRight"
)

add_summary_plots(wb)

add_report_sheet <- function(wb, sheet_name, data) {
  
  # Add Action taken column
  data$Action.taken <- ""
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet_name,
    data,
    startRow = 1,
    startCol = 1,
    headerStyle = header_style
  )
  
  # Colour Action taken header yellow
  action_col <- ncol(data)
  
  addStyle(
    wb,
    sheet_name,
    action_header_style,
    rows = 1,
    cols = action_col,
    gridExpand = FALSE,
    stack = FALSE
  )
  
  freezePane(wb, sheet_name, firstRow = TRUE)
  
  setColWidths(
    wb,
    sheet_name,
    cols = 1:ncol(data),
    widths = "auto"
  )
}
add_report_sheet(wb, "No login 3 months", not_logged_3_months)
add_report_sheet(wb, "Login 3m Activity old", logged_3_months_activity_old)

#Set workbook name
doc_name <- paste0("REDCap_UserAudit_", today, ".xlsx")

#Save workbook
saveWorkbook(wb, doc_name, overwrite = TRUE)
