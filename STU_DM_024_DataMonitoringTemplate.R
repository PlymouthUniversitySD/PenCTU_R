#Script Title: <STUDY NAME> Data Monitoring #\substitute <STUDY NAME> for the name of the study
#Author: <AUTHOR NAME> #\substitute <AUTHOR NAME> for your name
#Date/Verion: DDMMMYYYY VX.X #\add the date the script was published and the version number
#R Version X.X.X #\add the instance of R you wrote the script using (displayed in first line of console)

#\HOW TO USE THIS TEMPLATE:
#\Lines that start with '#' are script annotations and should remain in the script. Lines that start with '#\' are template instructions and 
#\should be deleted.

#Define API code
token <- "your_token" #NEVER SAVE YOUR API CODE IN THE SCRIPT

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

#Define url
#url <- "https://clinicaltrials.plymouth.ac.uk/api/" #\use this URL when working in test (remove hash at start of line to use this URL)
#url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/" #\use this url when working in live (remove hash at start of line to use this URL)

#Import dataset via REDCap API
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

#remove repeat instances
#\it may be necessary to remove repeat instances otherwise your critical data items will be noted as misisng for all repeats.
dataset <- subset(dataset, is.na(redcap_repeat_instance))

#Set today's date
today <- as.Date(format(Sys.Date(),"%Y-%m-%d"))

###IDENTIFY MISSING CRITICAL DATA ITEMS###

#Load critical data items file
critical_data_items <- read.csv("STU_DM_024_critical_data_items.csv") #\replace STU with the study acronym

#create blank list
results_list <- list()

#identify all missing critical data items in the dataset
for (i in 1:nrow(critical_data_items)) {
  
  timepoint <- critical_data_items$timepoint[i]
  field <- critical_data_items$field[i]
  
  filtered_data <- dataset %>% filter(redcap_event_name == timepoint)
  
  blank_or_special_records <- filtered_data %>%
    filter(is.na(!!sym(field)) | 
             !!sym(field) == "" | 
             !!sym(field) %in% c("NI", "UNK", "NA", "PNS")) %>%
    pull(record_id) 
  
  if (length(blank_or_special_records) > 0) {
    results_list[[i]] <- data.frame(
      timepoint = timepoint,
      field = field,
      record_id = blank_or_special_records
    )
  }
}

#bind results into a single table
missing_critical_data_items <- bind_rows(results_list)

#write results file
filename <- paste0(today, "_<STUDY NAME>_missing_critical_data_items.csv") #\substitute <STUDY NAME> for the name of the study
write.csv(missing_critical_data_items, filename)

###IDENTIFY OVERDUE TIMEPOINTS###

#Load event data file
scheduled_events <- read.csv("STU_DM_024_scheduled_events.csv") #\replace STU with the study acronym

#create blank list
results_list <- list()
today_date <- Sys.Date()

#identify all overdue timepoints in the dataset
for (i in 1:nrow(scheduled_events)) {

  timepoint_name <- scheduled_events$timepoint_name[i]
  date_fieldname <- scheduled_events$date_fieldname[i]
  previous_timepoint_name <- scheduled_events$previous_timepoint_name[i]
  previous_date_fieldname <- scheduled_events$previous_date_fieldname[i]
  offset_days <- scheduled_events$offset[i]

  previous_timepoint_data <- dataset %>% filter(redcap_event_name == previous_timepoint_name)
  
  previous_timepoint_data <- previous_timepoint_data %>%
    mutate(calculated_date = !!sym(previous_date_fieldname) + days(offset_days)) %>%
    filter(calculated_date < today_date)
  
  record_ids_before_today <- previous_timepoint_data %>% pull(record_id)
  
  missing_record_ids <- dataset %>%
    filter(record_id %in% record_ids_before_today & redcap_event_name == timepoint_name) %>%
    pull(record_id)
  
  missing_record_ids <- setdiff(record_ids_before_today, missing_record_ids)
  if (length(missing_record_ids) > 0) {
    results_list[[i]] <- data.frame(
      timepoint_name = timepoint_name,
      record_id = missing_record_ids
    )
  }
}

#bind results into a single table
overdue_timepoints <- bind_rows(results_list)

#write results file
filename <- paste0(today, "_<STUDY NAME>_overdue_timepoints.csv")  #\substitute <STUDY NAME> for the name of the study
write.csv(overdue_timepoints, filename)

###FREE TEXT FIELDS###

#Load free text fields file
free_text_fields <- read.csv("STU_DM_024_Free_text_fields.csv") #\replace STU with the study acronym

results_list <- list()

for (i in 1:nrow(free_text_fields)) {
  
  timepoint <- free_text_fields$timepoint[i]
  field <- free_text_fields$field[i]
  
  filtered_data <- dataset %>%
    filter(redcap_event_name == timepoint) %>%
    select(record_id, redcap_event_name, !!sym(field))
  
  colnames(filtered_data)[3] <- "field_value"
  
  filtered_data <- filtered_data %>%
    mutate(field_value = as.character(field_value),
           field_name = field)
  filtered_data <- filtered_data %>%
    select(record_id, redcap_event_name, field_name, field_value)
  results_list[[i]] <- filtered_data
}

# Combine all data frames in the results list
free_text_data <- bind_rows(results_list)
filename <- paste0(today, "_<STUDY NAME>_free_text_data.csv") #\substitute <STUDY NAME> for the name of the study
write.csv(free_text_data, filename)