#Script Title: <STUDY NAME> Audit Trail Monitoring #\replace <STUDY NAME> with your study name.
#Author: <AUTHOR NAME> #\substitute <AUTHOR NAME> for your name
#Date/Verion: DDMMMYYYY VX.X #\add the date the script was published and the version number
#R Version X.X.X #\add the instance of R you wrote the script using (displayed in first line of console)

#\HOW TO USE THIS TEMPLATE:
#\Lines that start with '#' are script annotations and should remain in the script. Lines that start with '#\' are template instructions and 
#\should be deleted.

#Define the API tokens - NEVER SAVE YOUR API CODE IN THE SCRIPT 
token <- 'your_token' #\replace 'your_token' with your API token for the study. 
admin_token <- 'your_admin_token' #\replace 'your_token' with your API token for the admin module.

#Load the audit trail from API
url <- "https://clinicaltrials.plymouth.ac.uk/api/"
formData <- list("token"=token,
                 content='log',
                 logtype='',
                 user='',
                 record='',
                 beginTime='2023-12-11 10:57',
                 endTime='',
                 format='csv',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
audit <- httr::content(response)

#Load the Admin Module data for the study
formData <- list("token"=admin_token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'records[0]'='<RECORD_NAME>', #\replace <RECORD_NAME> with the record name of your study in the admin module (e.g. STEPS_II)
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='true',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
admin <- httr::content(response)

#Load packages
library(dplyr)
library(officer)
library(tidyr)
library(flextable)
library(lubridate)
library(stringr)

#Set todays date
today <- Sys.Date()

#Calculate the date 1 month ago
last_month <- today %m-% months(1)

#Remove time from date/time field
audit$timestamp <-substr(audit$timestamp, 1,10)

#Define actions of interest
audit$action <- sub("Update record.*", "Update record", audit$action)

#Define PenCTU users
excluded_users <- c("ryan.ashford@plymouth.ac.uk", "paigan.aspinall@plymouth.ac.uk", "mark.warner@plymouth.ac.uk", 
                    "hanna.abraham@plymouth.ac.uk", "reece.tarrant@plymouth.ac.uk", "daniel.maddock@plymouth.ac.uk", 
                    "helen.hambly@plymouth.ac.uk", "brian.wainman@plymouth.ac.uk") #\update if new staff join the team

##CREATE DOCUMENT
audit_review <- read_docx()
title <- paste0("<STUDY NAME> Audit Review Report - ", today) #\replace <STUDY NAME> with your study name.
audit_review <- audit_review %>%
  body_add_par(title, style="Normal")

#Identification of mass data changes
audit_review <- audit_review %>%
  body_add_par("Mass data changes by users in the last 30 days", style = "heading 1")

audit_mass <-audit %>%
  filter(!(username %in% excluded_users))

mass <- audit_mass %>% 
  filter(action == "Update record" & reason != ""& as.Date(timestamp) >= last_month) %>% 
  select(timestamp, username)

mass_count <- mass %>%
  group_by(username) %>%
  summarise(count=n(), .groups = 'drop') %>%
  arrange(desc(count))

mass_table <-flextable(mass_count)

mass_table <- set_table_properties(mass_table, width = 0.8, layout = "autofit")

audit_review <- audit_review %>% 
  body_add_flextable(mass_table)

#Identification of unauthorised exports

audit_review <- audit_review %>%
  body_add_par("Identification of unauthorised exports", style = "heading 1")

exports <- audit %>%
  filter(action == "Data export"|action == "Data export (API)"|action == "Manage/Design" & details == "Export Logging (API)"
         |action == "Manage/Design" & str_starts(details, "Export"))

exportlog <- admin %>%
  filter(redcap_repeat_instrument == "24_Export Log")%>%
  select(redcap_repeat_instance, date_of_export)

names(exports)[names(exports) == "timestamp"]<- "date"
names(exportlog)[names(exportlog) == "date_of_export"]<- "date"

exports$date <- as.Date(exports$date)

unauthorised_exports <- exports %>%
  anti_join(exportlog, by = "date")

unauthorised_exports <- select(unauthorised_exports, date,username,action)

export_table <-flextable(unauthorised_exports)

export_table <- set_table_properties(export_table, width = 0.8, layout = "autofit")

audit_review <- audit_review %>% 
  body_add_flextable(export_table)

#Identification of unauthorised data entry

audit_review <- audit_review %>%
  body_add_par("Identification of unauthorised data entry in the last 30 days", style = "heading 1")

#\update below if new users join the team
dataentry <- audit %>%
  filter(action == "Update record" & username == "ryan.ashford@plymouth.ac.uk"|action == "Update record" & username == "paigan.aspinall@plymouth.ac.uk"|
           action == "Update record" & username == "mark.warner@plymouth.ac.uk"|action == "Update record" & username == "hanna.abraham@plymouth.ac.uk"|
           action == "Update record" & username == "reece.tarrant@plymouth.ac.uk"|action == "Update record" & username == "daniel.maddock@plymouth.ac.uk"|
           action == "Update record" & username == "helen.hambly@plymouth.ac.uk"|action == "Update record" & username == "brian.wainman@plymouth.ac.uk")

dataentry <- dataentry %>%
  filter(!is.na(details) & timestamp >= last_month & timestamp < today & !grepl("complete", details, ignore.case = TRUE))%>%
  select(timestamp, username, details, record)

entrylog <- admin %>% 
  filter(redcap_repeat_instrument == "34_Data Change Log" & date_of_change >= last_month & date_of_change < today)%>%
  select(redcap_repeat_instance, date_of_change, name_of_person_making_chan, affected_record)

names(dataentry)[names(dataentry) == "timestamp"]<- "date"
names(entrylog)[names(entrylog) == "date_of_change"]<- "date"

dataentry$date <- as.Date(dataentry$date)

unauthorised_de <- dataentry %>%
  anti_join(entrylog, by = "date")


entry_table <-flextable(unauthorised_de)

entry_table <- set_table_properties(entry_table, width = 0.8, layout = "autofit")

audit_review <- audit_review %>% 
  body_add_flextable(entry_table)

file_name <- paste0(today, "<SYUDY NAME>_AuditReview.docx") #\replace <STUDY NAME> with your study name.
print(audit_review, target=file_name)
