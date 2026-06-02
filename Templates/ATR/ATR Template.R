#Title: Audit Trail Review Template
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUN2026
#R version: 4.4.3

#Define API token(s)
token_1 <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT
token_2 <- "YOUR TOKEN 2" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if only 1 project used
token_3 <- "YOUR TOKEN 3" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if fewer than 3 projects used
token_4 <- "YOUR TOKEN 4" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if fewer than 4 projects used

#Define study name
study_name <- "STUDY NAME" #Enter your study name here, this will display in the exported file name
document_id <- "DOCUMENT ID" #Enter the document ID of the ATR here

#Load critical data items list
critical_data_items <- read.csv("_CriticalDataItems.csv")

#Run function to import the processed ATR data
atr_data_1 <- get_audit_trail(token_1, study_name, write_file = FALSE)
atr_data_2 <- get_audit_trail(token_2, study_name, write_file = FALSE) #Delete if only 1 project used
atr_data_3 <- get_audit_trail(token_3, study_name, write_file = FALSE) #Delete if fewer than 3 projects used
atr_data_4 <- get_audit_trail(token_4, study_name, write_file = FALSE) #Delete if fewer than 4 projects used

#Bind all ATR data into a single file
atr_data <- rbind(atr_data_1, atr_data_2, atr_data_3, atr_data_4) #remove any atr files that are not required

#Write audit trail as csv
today <- Sys.Date()
audit_file_name <- paste0(study_name, "_AuditTrail_", today, ".csv")
write.csv(atr_data, audit_file_name)

#Load data dictionaries
url <- "https://clinicaltrials.plymouth.ac.uk/api/"
formData <- list("token"=token_1,
                 content='metadata',
                 format='csv',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
metadata_1 <- httr::content(response)
#Delete if only 1 REDCap project used:
formData <- list("token"=token_2,
                 content='metadata',
                 format='csv',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
metadata_2 <- httr::content(response)
#Delete if fewer than 3 REDCap projects used
formData <- list("token"=token_3,
                 content='metadata',
                 format='csv',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
metadata_3 <- httr::content(response)
#Delete if fewer than 4 REDCap projects used
formData <- list("token"=token_4,
                 content='metadata',
                 format='csv',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
metadata_4 <- httr::content(response)

#Bind all data dictionaries into a single file
metadata <- rbind(metadata_1, metadata_2, metadata_3, metadata_4) #remove any data dictionary files that are not required

#Run function to identify changes to critical data items
critical_data_changes <- identify_critical_data_changes(atr_data,  critical_data_items)

#Run function to identify changes per user
user_data_changes <- identify_user_data_changes(atr_data, metadata)

#Run function to identify changes per user per month
monthly_user_changes <- identify_monthly_user_data_changes(atr_data, metadata)

#Run function to identify unstable fields
unstable_fields <- identify_unstable_fields(atr_data, metadata)

#Run function to return time between changes
time_to_change <- time_between_changes(atr_data, metadata)

#Run function to identify out of hours changes
out_of_hours_changes <- identify_out_of_hours_changes(atr_data)

#Output file name
output_name <- paste0(study_name, '_ATROutput_', today, '.xlsx')

#Compile the final XML sheet
export_atr_workbook(
  critical_data_changes = critical_data_changes,
  user_data_changes = user_data_changes,
  monthly_user_changes = monthly_user_changes,
  unstable_fields = unstable_fields,
  time_to_change = time_to_change,
  out_of_hours_changes = out_of_hours_changes,
  document_id = document_id,
  study_name = study_name,
  file = output_name
)



