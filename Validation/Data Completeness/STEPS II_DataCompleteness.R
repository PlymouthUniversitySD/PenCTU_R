#Title: Data Completeness STEPS II
#Author: Paigan Aspinall
#Version & Date: V1.0.0 14JUL2026
#R version: 4.4.3

#Define token
token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT

#Set study name
study_name <- "STEPS II"

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
sites <- c("01 - Salisbury", 
           "02 - North Cumbria", 
           "03 - Birmingham", 
           "04 - Swansea", 
           "05 - Leeds", 
           "06 - Betsi Cadwaladr", 
           "07 - Derby & Burton", 
           "08 - Ipswich")

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
               "initial_assessment_questions_complete", 
               "baseline_exercise_diary_complete", 
               "medication_changes_complete", 
               "m_walking_test_10_mwt_complete", 
               "mds_updrs_part_1a_complete", 
               "mds_updrs_part_1b_complete", 
               "mds_updrs_part_2_complete", 
               "dynamometry_walking_assessment_complete", 
               "new_freezing_of_gait_questionnaire_nfog_complete", 
               "concern_of_falling_questionnaire_fesi_complete", 
               "mds_updrs_part_3_complete", 
               "imu_data_complete", 
               "anticipatory_postural_adjustments_apa_complete", 
               "parkinsons_disease_quality_of_life_questionnaire_p_complete", 
               "minibestest_complete", 
               "eq5d5l_complete", 
               "deviations_complete", 
               "end_of_appointment_complete"
               
)

#Subset to isolate only the data from the timepoint
baseline_data <- subset(dataset, redcap_event_name =="Week 0: blinded assessment")

#Isolate the completeness data
baseline_completeness <- select(baseline_data, 'redcap_data_access_group', 'redcap_repeat_instance', form_list)

#Summarise the completeness data
baseline_data_completeness <- completeness_tracker_data_prep(baseline_completeness, form_list, sites, statuses, exclude_repeat_instances = TRUE)

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "Week 0", baseline_data_completeness, baseline_completeness, sites, statuses, exclude_repeat_instances=TRUE)

#---------------------------------------------------------
#Add the above for as many timepoints as necessary

#Set the file name
today_date <- Sys.Date()
wb_file_name <- paste0(study_name, "_CompletenessSummary_", today_date, ".xlsx")

#Save and open
saveWorkbook(wb, wb_file_name, overwrite = TRUE)
