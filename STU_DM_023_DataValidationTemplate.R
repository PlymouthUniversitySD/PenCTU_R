#Script Title: <STUDY NAME> Validation Script #\substitute <STUDY NAME> for the name of the study
#Author: <AUTHOR NAME> #\substitute <AUTHOR NAME> for your name
#Date/Verion: DDMMMYYYY VX.X #\add the date the script was published and the version number
#R Version X.X.X #\add the instance of R you wrote the script using (displayed in first line of console)

#\HOW TO USE THIS TEMPLATE:
#\Lines that start with '#' are script annotations and should remain in the script. Lines that start with '#\' are template instructions and 
#\should be deleted.
#\Find and replace all instances for 'STU' with the letter code for your study, found as a prefix on all study documentation (e.g. for FRECycl-D this is FRE)
#\Find and replace all instance of 'STUDYNAME' with the name of your study
#\You will need to complete a copy of STU_DM_023_date_range_checks_dynamic, STU_DM_023_date_range_checks_standard,
#\STU_DM_023_date_range_validation_repeating, #\STU_DM_023_numeric_range_validation with all of the date and range checks required in your study.
#\STU in the file name should be replaced with the study prefix.

#Define API code
token <- "your_token" #NEVER SAVE YOUR API CODE IN THE SCRIPT

#Define the URL
url <- "https://clinicaltrials.plymouth.ac.uk/api/"

#Load the raw dataset
#import dataset via REDCap Api
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='true',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
dataset <- httr::content(response)

#Load libraries
library(PenCTU)
library(dplyr)
library(tidyr)

#Load date validation CSVs
STU_DM_023_date_range_checks_standard <- read.csv('STU_DM_023_date_range_checks_standard.csv')
STU_DM_023_date_range_checks_dynamic <- read.csv('STU_DM_023_date_range_checks_dynamic.csv')
STU_DM_023_date_range_checks_repeating <- read.csv('STU_DM_023_date_range_checks_repeating.csv')

#Run date validation functions
standard_date_validation <- date_range_validation_standard(dataset, STU_DM_023_date_range_checks_standard)
dynamic_date_validation <- date_range_validation_dynamic(dataset, STU_DM_023_date_range_checks_dynamic)
repeating_date_validation <- date_range_validation_repeating(dataset, STU_DM_023_date_range_checks_repeating)

#Load numeric validation CSVs
STU_DM_023_numeric_range_checks <- read.csv('STU_DM_023_numeric_range_checks.csv')

#Run numeric validation function
numeric_validation <- numeric_range_validation(dataset, STU_DM_023_numeric_range_checks)

#Format column names
colnames(dynamic_date_validation)[colnames(dynamic_date_validation) == 'error'] <- 'error_message'
dynamic_date_validation <- select(dynamic_date_validation, record_id, field_name, event_name, error_message)
colnames(standard_date_validation)[colnames(standard_date_validation) == 'error'] <- 'error_message'
standard_date_validation <- select(standard_date_validation, record_id, field_name, event_name, error_message)

#set today's date
today <- Sys.Date()

#Set file names
standard_dataset_name <- paste0(today, "_STUDYNAME_StandardDateValidationOutput.csv")
dynamic_dataset_name <- paste0(today, "_STUDYNAME_DynamicDateValidationOutput.csv")
repeating_dataset_name <- paste0(today, "_STUDYNAME_RepeatingDateValidationOutput.csv")
numeric_dataset_name <- paste0(today, "_STUDYNAME_NumericValidationOutput.csv")

#Save the output CSVs
write.csv(standard_date_validation, file = standard_dataset_name, row.names = FALSE)
write.csv(dynamic_date_validation, file = dynamic_dataset_name, row.names = FALSE)
write.csv(repeating_date_validation, file = repeating_dataset_name, row.names = FALSE)
write.csv(numeric_validation, file = numeric_dataset_name, row.names = FALSE)