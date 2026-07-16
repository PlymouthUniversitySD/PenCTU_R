#Title: Data Completeness ET
#Author: Paigan Aspinall
#Version & Date: V1.0.0 14JUL2026
#R version: 4.4.3

#Define token
token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT

#Set study name
study_name <- "ET"

#Load libraries
library(PenCTU)
library(lubridate)
library(openxlsx)
library(dplyr)

#Import data
url <- "https://clinicaltrials.plymouth.ac.uk/api/"
formData <- list("token"= token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='true',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
dataset <- httr::content(response)

#Define sites
sites <- c("01 - Risley", "02 - Liverpool")

#Define completeness statuses
statuses <- c("Incomplete", "Partially complete", "Complete")

#Prepare Excel headers
header_row1 <- c("Form:", rep(sites, each = length(statuses)))
header_row2 <- c("Completeness:", rep(statuses, times = length(sites)))

#Create workbook
wb <- createWorkbook()

#-------------------------------------
  #BASELINE
#-------------------------------------

#Define the completeness fields

form_list <- c("timepoint_introduction_complete", 
               "demographics_complete", 
               "sentence_details_complete", 
               "medical_history_complete", 
               "canforr_service_user_complete", 
               "canforr_staff_complete", 
               "ascot_sct4_complete", 
               "eq5d5l_complete", 
               "icecapa_complete", 
               "reqol10_complete", 
               "resource_use_source_data_complete", 
               "resource_use_part_1_complete", 
               "resource_use_part_2_complete", 
               "personal_care_and_help_complete", 
               "falls_diary_complete", 
               "end_of_visit_complete"
               )

#Subset to isolate only the data from the timepoint
baseline_data <- subset(dataset, redcap_event_name =="Baseline")

#Isolate the completeness data
baseline_completeness <- select(baseline_data, 'redcap_data_access_group', form_list)

#Summarise the completeness data
baseline_data_completeness <- completeness_tracker_data_prep(baseline_completeness, form_list, sites, statuses)

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "Baseline", baseline_data_completeness, baseline_completeness, sites, statuses)


#-------------------------------------
#42-DAY FOLLOW-UP
#-------------------------------------

#Define the completeness fields

form_list <- c("timepoint_introduction_complete", 
               "sentence_details_complete", 
               "canforr_service_user_complete", 
               "canforr_staff_complete", 
               "ascot_sct4_complete", 
               "eq5d5l_complete", 
               "icecapa_complete", 
               "reqol10_complete", 
               "resource_use_source_data_complete", 
               "resource_use_part_1_complete", 
               "resource_use_part_2_complete", 
               "personal_care_and_help_complete", 
               "falls_diary_complete", 
               "end_of_visit_complete"
)

#Subset to isolate only the data from the timepoint
day_42_data <- subset(dataset, redcap_event_name =="42-day Follow-up")

#Isolate the completeness data
day_42_completeness <- select(day_42_data, 'redcap_data_access_group', form_list)

#Summarise the completeness data
day_42_data_completeness <- completeness_tracker_data_prep(day_42_completeness, form_list, sites, statuses)

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "42-day Follow-up", day_42_data_completeness, baseline_completeness, sites, statuses)

#-------------------------------------
#90-DAY FOLLOW-UP
#-------------------------------------

#Define the completeness fields

form_list <- c("timepoint_introduction_complete", 
               "sentence_details_complete", 
               "canforr_service_user_complete", 
               "canforr_staff_complete", 
               "ascot_sct4_complete", 
               "eq5d5l_complete", 
               "icecapa_complete", 
               "reqol10_complete", 
               "resource_use_source_data_complete", 
               "resource_use_part_1_complete", 
               "resource_use_part_2_complete", 
               "personal_care_and_help_complete", 
               "falls_diary_complete", 
               "end_of_visit_complete"
)

#Subset to isolate only the data from the timepoint
day_90_data <- subset(dataset, redcap_event_name =="90-day Follow-up")

#Isolate the completeness data
day_90_completeness <- select(day_90_data, 'redcap_data_access_group', form_list)

#Summarise the completeness data
day_90_data_completeness <- completeness_tracker_data_prep(day_90_completeness, form_list, sites, statuses)

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "90-day Follow-up", day_90_data_completeness, baseline_completeness, sites, statuses)

#---------------------------------------------------------

#Set the file name
today_date <- Sys.Date()
wb_file_name <- paste0(study_name, "_CompletenessSummary_", today_date, ".xlsx")

#Save and open
saveWorkbook(wb, wb_file_name, overwrite = TRUE)
