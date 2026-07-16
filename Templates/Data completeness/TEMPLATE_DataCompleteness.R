#Title: Data Completeness TEMPLATE
#Author: Paigan Aspinall
#Version & Date: V1.0.1 14JUL2026
#R version: 4.4.3

#Define token
token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT

#Set study name
study_name <- "STUDY NAME"

#Load libraries
library(PenCTU)
library(lubridate)
library(dplyr)
library(openxlsx)

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
sites <- c("001 - Avon View", "002 - Pendennis Residential Home",  "003 - Chestnut Lodge")

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

form_list <- c("timepoint_introduction_complete", "demographics_complete","medical_history_complete",  "antibiotic_use_complete")

#Subset to isolate only the data from the timepoint
baseline_data <- subset(dataset, redcap_event_name =="Baseline")

#Isolate the completeness data
baseline_completeness <- select(baseline_data, 'redcap_data_access_group', 'redcap_repeat_instance', form_list)

#Summarise the completeness data
baseline_data_completeness <- completeness_tracker_data_prep(baseline_completeness, form_list, sites, statuses, exclude_repeat_instances = TRUE) #may need to set last parameter to false if using a repeating event

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "Baseline", baseline_data_completeness, baseline_completeness, sites, statuses, exclude_repeat_instances = TRUE)

#---------------------------------------------------------
#Add the above for as many timepoints as necessary

#Set the file name
today_date <- Sys.Date()
wb_file_name <- paste0(study_name, "_CompletenessSummary_", today_date, ".xlsx")

#Save and open
saveWorkbook(wb, wb_file_name, overwrite = TRUE)