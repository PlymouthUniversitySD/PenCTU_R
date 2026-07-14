#Title: Data Completeness TEMPLATE
#Author: Paigan Aspinall
#Version & Date: V1.0.0 13JUL2026
#R version: 4.4.3

#Define token
token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT

#Set study name
study_name <- "STUDY NAME"

#Load libraries
library(PenCTU)
library(lubridate)
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
sites <- c("001 - Avon View", 
           "002 - Pendennis Residential Home", 
           "003 - Chestnut Lodge", 
           "004 - Three Corners Nursing Home", 
           "006 - Hill House Nursing Home", 
           "007 - Primley Court Nursing Home", 
           "009 - Camelot House and Lodge", 
           "011 - Mount Olivet nursing home", 
           "013 - West Eaton Nursing Home", 
           "014 - Heron House Residential Home", 
           "016 - Trafalgar Care Home", 
           "017 - summerdyne", 
           "018 - Burwood nursing home", 
           "022 - Mulberry House")

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
               "medical_history_complete", 
               "antibiotic_use_complete", 
               "functional_assessment_staging_tool_fast_complete", 
               "clinical_frailty_scale_cfs_complete", 
               "uk_eng_eq5d5l_redcap_proxy1_complete", 
               "demqolch_complete", 
               "mini_nutritional_assessment_mna_complete", 
               "weight_complete", 
               "modified_barthel_index_mbi_complete", 
               "food_diary_complete", 
               "end_of_visit_complete", 
               "deviations_complete"
)

#Subset to isolate only the data from the timepoint
baseline_data <- subset(dataset, redcap_event_name =="baseline_arm_1")

#Isolate the completeness data
baseline_completeness <- select(baseline_data, 'redcap_data_access_group', form_list)

#Summarise the completeness data
baseline_data_completeness <- completeness_tracker_data_prep(baseline_completeness, form_list, sites, statuses)

#Add the completeness data to the Excel sheet
write_excel_completeness(wb, "Baseline", baseline_data_completeness, baseline_completeness, sites, statuses)

#---------------------------------------------------------
#Add the above for as many timepoints as necessary

#Set the file name
today_date <- Sys.Date()
wb_file_name <- paste0(study_name, "_CompletenessSummary_", today_date, ".xlsx")

#Save and open
saveWorkbook(wb, wb_file_name, overwrite = TRUE)