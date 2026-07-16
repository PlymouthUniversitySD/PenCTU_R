#Title: Data Completeness BOOST
#Author: Paigan Aspinall
#Version & Date: V1.0.1 16JUL2026
#R version: 4.4.3

#Define token
token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT

#Set study name
study_name <- "BOOST"

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
sites <- c("01 - Derriford Hospital, Plymouth", "02 - Hull Royal Infirmary",  "03 - Aintree University Hospital, Liverpool",
           "04 - Gateshead Health NHS Foundation Trust", "05 - St Georges Hospital, London", "06 - Glasgow Royal Infirmary",
           "07 - Royal Devon and Exeter", "08 - Royal Wolverhampton NHS Trust", "09 - Queens Medical Centre, Nottingham")

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
               "personal_details_complete", 
               "demographics_complete", 
               "medical_history_complete", 
               "physical_assessment_complete", 
               "concomitant_medications_complete", 
               "hmb_supplements_complete", 
               "liver_frailty_index_lfi_complete", 
               "animal_naming_test_ant_complete", 
               "short_form36_sf36_complete", 
               "warwick_edinburgh_mental_wellbeing_scale_wemwbs_complete", 
               "alcohol_7day_timeline_follow_back_complete", 
               "hour_diet_recall_complete", 
               "blood_tests_complete", 
               "model_for_end_stage_liver_disease_meld_score_complete", 
               "child_pugh_90day_complete", 
               "child_pugh_score_complete", 
               "deviations_complete", 
               "end_of_visit_complete"
)

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
