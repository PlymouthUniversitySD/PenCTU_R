#Script title: <STUDY NAME> Site Monitoring #\substitute <STUDY NAME> for the name of the study
#Author: <AUTHOR NAME> #\substitute <AUTHOR NAME> for your name
#Date/Verion: DDMMMYYYY VX.X #\add the date the script was published and the version number
#R Version X.X.X #\add the instance of R you wrote the script using (displayed in first line of console)

#\HOW TO USE THIS TEMPLATE:
#\Lines that start with '#' are script annotations and should remain in the script. Lines that start with '#\' are template instructions and 
#\should be deleted.

#Define API code - NEVER SAVE YOUR API CODE IN THE SCRIPT
token <- 'your_token' 
admin_token <- 'your_admin_module_token'

#Define url
#url <- "https://clinicaltrials.plymouth.ac.uk/api/" #\use this URL when working in test (remove hash at start of line to use this URL)
#url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/" #\use this url when working in live (remove hash at start of line to use this URL)


##LOAD QUERY DATA
#\substitute <STUDY NAME> for the name of the study. 
##Note this file must be downloaded from REDCap each time the script is run. Will need to rename file and then move to the <STUDY NAME>_R folder
queries <- read.csv("<STUDY NAME>_DataResolutionDashboard.csv") 

##IMPORT DATA
formData <- list("token"=token,
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
                 exportCheckboxLabel='true',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
dataset <- httr::content(response)

##IMPORT ADMIN MODULE DATA
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'records[0]'='<STUDY NAME>', #\substitute <STUDY NAME> for the name of the study
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='true',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

#Format site name
dataset$redcap_data_access_group <- substr(dataset$redcap_data_access_group, 6, nchar(dataset$redcap_data_access_group))

#LOAD PACKAGES
library(dplyr)
library(officer)
library(tidyr)
library(flextable)
library(lme4)
library(ggplot2)

##CREATE DOCUMENT and Insert a title
today <- format(Sys.Date(), "%Y-%m-%d")
title <- paste0("<STUDY NAME> site monitoring report - ", today) #\substitute <STUDY NAME> for the name of the study

site_monitoring <- read_docx()
site_monitoring <- site_monitoring %>%
  body_add_par(title, style="Normal")

#NUMBER OF PARTICIPANTS BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of enrolled participants by site", style = "heading 1")
#\replace <ENROLLMENT TIMEPOINT> (below) with the label of the timepoints where participants are considered 
#\enrolled (e.g. 'Randomisation', 'Baseline', etc.)
enrolled <- subset(dataset, redcap_event_name == '<ENROLLMENT TIMEPOINT>') 
enrolled_records <- subset(dataset, redcap_event_name == '<ENROLLMENT TIMEPOINT>t') 
#\replace <SITE 1>, <SITE 2> (below) with the site names, add more as required.
expected_sites <- c('<SITE 1>', '<SITE 2>') 
actual_counts <- table(enrolled$redcap_data_access_group)
actual_df <- data.frame(value = actual_counts)
actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
missing_columns <- setdiff(expected_sites, names(actual_wide))
for (column in missing_columns) {
  actual_wide[[column]] <- 0
}
actual_wide <- actual_wide[, sort(names(actual_wide))]
enrolled <- actual_wide
enrolled_table <- flextable(actual_wide)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(enrolled_table)

#NUMBER OF AES BY SITE #\remove is AEs not recorde in study down to '-----------------------------------'
site_monitoring <- site_monitoring %>%
  body_add_par("Number of AEs by site", style = "heading 1")
ae_data <- subset(dataset, redcap_event_name == 'SAE reporting') #\Confirm name of SAE timepoint is correct
if(nrow(ae_data)>0){
  actual_counts <- table(ae_data$redcap_data_access_group)
  actual_df <- data.frame(value = actual_counts)
  actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  ae_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  ae_table <- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(ae_table)


##AES BY ENROLLED PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
ae_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(ae_table)

#\-----------------------------------

#NUMBER OF SAES BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of SAEs by site", style = "heading 1")
#\Confirm name of SAE timepoint is correct. If AEs are not recorded in study, remove 'aeserious=='Yes''.
sae_data <- subset(dataset, redcap_event_name == 'SAE reporting' & aeseriousyn=='Yes') 
if(nrow(sae_data)>0){
  actual_counts <- table(sae_data$redcap_data_access_group)
  actual_df <- data.frame(value = actual_counts)
  actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  sae_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  sae_table <- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(sae_table)


##SAES BY ENROLLED PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
ae_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(ae_table)


#TIME BETWEEN SAE AWARENESS AND REPORT BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Average time between site awareness of SAE and report (days)", style = "heading 1")
if(nrow(sae_data)>0){
  #\confirm the field names below match those that are used in the SAE form (aecomplete1dat = date of SAE report;
  #\aeawaredat=date of site awareness)
  sae_data$datediff <- difftime(sae_data$aecomplete1dat, sae_data$aeawaredat, units = 'days')
  sae_data$datediff <- as.numeric(sae_data$datediff)
  avg_datediff <- sae_data %>%
    group_by(redcap_data_access_group) %>%
    summarise(avg_datediff = mean(datediff, na.rm = TRUE)) %>%
    spread(key = redcap_data_access_group, value = avg_datediff)
  missing_columns <- setdiff(expected_sites, names(avg_datediff))
  for (column in missing_columns) {
    avg_datediff[[column]] <- NaN
  }
  avg_datediff <- avg_datediff[, sort(names(avg_datediff))]
  avg_datediff <- avg_datediff[, !colnames(avg_datediff) %in% "<NA>"]
  sae_table <- flextable(avg_datediff)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  expected_counts <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  sae_table <- flextable(actual_wide)
}
site_monitoring <- site_monitoring %>% 
  body_add_flextable(sae_table)

#TIME TO PI OPINION COMPLETION BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Average time to completion of SAE PI opinion form (days)", style = "heading 1")
if(nrow(sae_data)>0){
  #\confirm the field names below match those that are used in the SAE form (aepicompletedat = 
  #\date of SAE PI Opinion form completion; aecomplete1dat=date of SAE report)
  sae_data$pidatediff <- difftime(sae_data$aepicompletedat, sae_data$aecomplete1dat, units = 'days')
  sae_data$pidatediff <- as.numeric(sae_data$pidatediff)
  avg_datediff <- sae_data %>%
    group_by(redcap_data_access_group) %>%
    summarise(avg_datediff = mean(pidatediff , na.rm = TRUE)) %>%
    spread(key = redcap_data_access_group, value = avg_datediff)
  missing_columns <- setdiff(expected_sites, names(avg_datediff))
  for (column in missing_columns) {
    avg_datediff[[column]] <- NaN
  }
  avg_datediff <- avg_datediff[, sort(names(avg_datediff))]
  avg_datediff <- avg_datediff[, !colnames(avg_datediff) %in% "<NA>"]
  sae_table <- flextable(avg_datediff)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  expected_counts <- expected_counts * 0
  actual_wide <- data.frame(expected_counts)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  sae_table <- flextable(actual_wide)
}
site_monitoring <- site_monitoring %>% 
  body_add_flextable(sae_table)

#NUMBER OF QUERIES BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of queries by site", style = "heading 1")
if(nrow(queries)>0){
  queries$Data.Access.Group <- substr(queries$Data.Access.Group, 6, nchar(queries$Data.Access.Group))
  actual_counts <- table(queries$Data.Access.Group)
  actual_df <- data.frame(value = actual_counts)
  actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  query <- actual_wide
  query_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  query_table<- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

query_numbers <- actual_wide

##QUERIES BY ENROLLED PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
query_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

#AVERAGE TIME TO QUERY CLOSE
site_monitoring <- site_monitoring %>%
  body_add_par("Average time to query close by site (days)", style = "heading 1")
queries$Days.Open <- as.numeric(as.character(queries$Days.Open))
avg_queryopen <- queries %>%
  group_by(Data.Access.Group) %>%
  summarise(avg_datediff = mean(Days.Open , na.rm = TRUE)) %>%
  spread(key = Data.Access.Group, value = avg_datediff)
missing_columns <- setdiff(expected_sites, names(avg_queryopen))
for (column in missing_columns) {
  avg_queryopen[[column]] <- NaN
}
avg_queryopen <- avg_queryopen[, sort(names(avg_queryopen))]
query_resolution <- flextable(avg_queryopen)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_resolution)

#AVERAGE NUMBER OF ERRORS BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Average number of errors by site", style = "heading 1")
errors <- subset(result, redcap_repeat_instrument =='32_Error Log')
if(nrow(errors)>0){
  errors <- merge(errors, enrolled_records, by.x = 'record_id1', by.y = 'record_id')
  actual_counts <- table(errors$redcap_data_access_group.y)
  actual_df <- data.frame(value = actual_counts)
  actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  errors <- actual_wide
  error_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  error_table <- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(error_table)

##ERRORS BY QUERY
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of number of number of queries", style = "heading 2")
proportions <- errors

for (column_name in colnames(query_numbers)) {
  if (column_name %in% colnames(errors)) {
    proportions[[column_name]] <- errors[[column_name]] / query_numbers[[column_name]]
  }
}

proportions <- proportions[, sort(names(proportions))]
errors <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(errors)

#DEVIATIONS PER SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of deviations per site", style = "heading 1")
#\confirm the name of the deviations form (below) is correct.
deviations <- subset(dataset, redcap_repeat_instrument == 'Deviations')
actual_counts <- table(deviations$redcap_data_access_group)
actual_df <- data.frame(value = actual_counts)
actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
missing_columns <- setdiff(expected_sites, names(actual_wide))
for (column in missing_columns) {
  actual_wide[[column]] <- 0
}
actual_wide <- actual_wide[, sort(names(actual_wide))]
deviations <- actual_wide
deviations_table <- flextable(actual_wide)
site_monitoring <- site_monitoring %>%
  body_add_flextable(deviations_table)

##DEVIATIONS BY TIMEPOINT
site_monitoring <- site_monitoring %>%
  body_add_par("Number of deviations per timepoint by site", style = "heading 2")

#\below define the timepoints where deviations with not be raised (e.g. withdrawal). Do this
#\by replacing <EVENT 1>, <EVENT 2> etc with the event label (e.g. Withdrawal, Registration, etc)
#\Copy and paste the <EVENT...> line to add more event
timepoints <- subset(dataset, redcap_event_name != '<EVENT 1>' & 
                       redcap_event_name != '<EVENT 2>' & 
                       redcap_event_name != '<EVENT...>' & #\add more of this line as required
                       redcap_event_name != '<EVENT 3>')
actual_counts <- table(timepoints$redcap_data_access_group)
actual_df <- data.frame(value = actual_counts)
actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
missing_columns <- setdiff(expected_sites, names(actual_wide))
for (column in missing_columns) {
  actual_wide[[column]] <- 0
}
actual_wide <- actual_wide[, sort(names(actual_wide))]
timepoints <- actual_wide

deviations[] <- lapply(deviations, function(x) as.numeric(as.character(x)))
timepoints[] <- lapply(timepoints, function(x) as.numeric(as.character(x)))

proportions <- deviations / timepoints
proportions[] <- lapply(proportions, function(x) {
  x[is.na(x) | is.infinite(x)] <- 0
  return(x)
})
proportions_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(proportions_table)

#\the below measures the average time between two time points (between here and '-----------------------')
#\you can copy and paste this section as many times as required. Useful for looking at, for example
#\the average time between registration and screening or baseline and randomisation

#\Below, until ----------------------- replace <EVENT 1> with the earlier timepoint being measured
#\(e.g. Registration) and <EVENT 2> with the later timepoint being measured (e.g. Screening)
#AVERAGE TIME BETWEEN <EVENT 1> AND <EVENT 2>
site_monitoring <- site_monitoring %>%
  body_add_par("Average time between <EVENT 1> and <EVENT 2> (days)", style = "heading 1")
#\do not change the names of 'event_1' or 'event_2'
event_1 <- subset(dataset, redcap_event_name == '<EVENT 1>')
event_2 <- subset(dataset, redcap_event_name == '<EVENT 2>')
event_2 <- subset(event_2, is.na(redcap_repeat_instrument))

#\replace <EVENT 1 DATE FIELD> with the field where the date of event 1 is entered (e.g. pdregisterdat)
#\replace <EVENT 2 DATE FIELD> with the field where the date of event 2 is entered (e.g. hoappointdat)
#\note it does not matter if the field names are the same for both events
event_1$date_1 <- event_1$<EVENT 1 DATE FIELD>
event_1 <- select(event_1, record_id, redcap_data_access_group, date_1)
event_2$date_2 <- event_2$<EVENT 2 DATE FIELD>
event_2 <- select(event_2, record_id, date_2)
screening <- merge(event_1, event_2, by='record_id', all.x=TRUE)
screening <- subset(screening, !is.na(date_1))
screening$diffdat <- as.numeric(difftime(screening$date_2, screening$date_1, units = 'days'))
avg_days <- screening %>%
  group_by(redcap_data_access_group) %>%
  summarise(avg_datediff = mean(diffdat , na.rm = TRUE)) %>%
  spread(key = redcap_data_access_group, value = avg_datediff)
missing_columns <- setdiff(expected_sites, names(avg_days))
for (column in missing_columns) {
  avg_days[[column]] <- NaN
}
avg_days <- avg_days[, sort(names(avg_days))]
avg_days <- avg_days[, !colnames(avg_days) %in% "<NA>"]
screening_table <- flextable(avg_days)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(screening_table)

#\-----------------------

#NUMBER OF EVENTS THAT DID NOT TAKE PLACE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of missed timepoints by site", style = "heading 1")

#\confirm that the field name below is correct for the Yes-No question 'Did the timepoint take place?'
event_occur <- subset(dataset, hooccuryn=='No')
if(nrow(event_occur)>0){
  actual_counts <- table(event_occur$redcap_data_access_group)
  actual_df <- data.frame(value = actual_counts)
  actual_wide <- pivot_wider(actual_df, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  event_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  event_table <- flextable(actual_wide)
}

events <- actual_wide

site_monitoring <- site_monitoring %>% 
  body_add_flextable(event_table)

#NUMBER OF EVENTS THAT DID NOT OCCUR PER ENROLLED PARTICIPANT BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of missed timepoints per participant by site", style = "heading 1")
for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(events)) {
    proportions[[column_name]] <- events[[column_name]] / enrolled[[column_name]]
  }
}

proportions <- proportions[, sort(names(proportions))]
events <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(events)

#AVERAGE TIME TO DATA ENTRY #\note you can only use this section if you have a field for each timepoint being checked
#\that is the date of the appointment and another that is the date of data entry
site_monitoring <- site_monitoring %>%
  body_add_par("Average difference in days between date of appointment and date of data entry by site (days)", style = "heading 1")
#\below you will need to add the name of the event (<EVENT NAME>) e.g. Baseline; the field where the
#\event date is entered (<EVENT DATE FIELD>) e.g. hoappointdat; and the field where the date of data
#\entry is entered (<DATA ENTRY DATE FIELD>) e.g. hoentrydat.
event_1 <- subset(dataset, redcap_event_name == '<EVENT NAME 1>')
event_1$difftime <- as.numeric(difftime(event_1$<DATA ENTRY DATE FIELD 1>, event_1$<EVENT DATE FIELD 1>, units='days'))
event_1 <- select(event_1, record_id, redcap_data_access_group, difftime)

#\repeat the below (until '-------------') as many times as requuired, replacing the ... in 'event_...'
#\with a number, increasing by one for each repeat added
event_... <- subset(dataset, redcap_event_name == '<EVENT NAME ...>')
event_...$difftime <- as.numeric(difftime(event_...$<DATA ENTRY DATE FIELD ...>, event_...$<EVENT DATE FIELD ...>, units='days'))
event_... <- select(event_..., record_id, redcap_data_access_group, difftime)
-------------

#\below add all of the file names generated above, e.g. event_1, event_2, event_3, event_4, event_5, event_6, event_7, event_8
average_date <- rbind(event_1, event_2, event_...)
avg_days <- average_date %>%
  group_by(redcap_data_access_group) %>%
  summarise(avg_datediff = mean(difftime , na.rm = TRUE)) %>%
  spread(key = redcap_data_access_group, value = avg_datediff)
missing_columns <- setdiff(expected_sites, names(avg_days))
for (column in missing_columns) {
  avg_days[[column]] <- NaN
}
avg_days <- avg_days[, sort(names(avg_days))]
avg_days <- avg_days[, !colnames(avg_days) %in% "<NA>"]
average_days_table <- flextable(avg_days)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(average_days_table)

#NUMBER OF STUDY WITHDRAWALS BY SITE 
site_monitoring <- site_monitoring %>%
  body_add_par("Number of study withdrawals by site", style = "heading 1")

#\you may need to edit the witype used below based on what exclusion reasons you have in your study
#\be sure to update the subtitle above also.

withdrawn <- subset(dataset, redcap_event_name == 'Withdrawal' & witype == 'Withdrawal from study') 
if(nrow(withdrawn)>0){
  actual_withdrawals <- table(withdrawn$redcap_data_access_group)
  actual_withdrawn <- data.frame(value = actual_withdrawals)
  actual_wide <- pivot_wider(actual_withdrawn, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table<- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(withdrawn_table)

##STUDY WITHDRAWALS BY PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
withdrawal_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

#NUMBER OF TREATMENT WITHDRAWALS BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of treatment withdrawals by site", style = "heading 1")

withdrawn <- subset(dataset, redcap_event_name == 'Withdrawal' & witype == 'Treatment withdrawal') 
if(nrow(withdrawn)>0){
  actual_withdrawals <- table(withdrawn$redcap_data_access_group)
  actual_withdrawn <- data.frame(value = actual_withdrawals)
  actual_wide <- pivot_wider(actual_withdrawn, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table<- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(withdrawn_table)

##TREATMENT WITHDRAWALS BY PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
withdrawal_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

#LOSS TO FOLLOW UPS (PRE TREATMENT) BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of loss to follow-ups (prior to treatment start) by site", style = "heading 1")

withdrawn <- subset(dataset, redcap_event_name == 'Withdrawal' & witype == 'Loss to follow-up - before treatment start') 
if(nrow(withdrawn)>0){
  actual_withdrawals <- table(withdrawn$redcap_data_access_group)
  actual_withdrawn <- data.frame(value = actual_withdrawals)
  actual_wide <- pivot_wider(actual_withdrawn, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table<- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(withdrawn_table)

##LOSS TO FOLLOW UPS (PRE TREATMENT) BY PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
withdrawal_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

#LOSS TO FOLLOW UPS (POST TREATMENT) BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of loss to follow-ups (after to treatment start) by site", style = "heading 1")

withdrawn <- subset(dataset, redcap_event_name == 'Withdrawal' & witype == 'Loss to follow-up - after treatment start') 
if(nrow(withdrawn)>0){
  actual_withdrawals <- table(withdrawn$redcap_data_access_group)
  actual_withdrawn <- data.frame(value = actual_withdrawals)
  actual_wide <- pivot_wider(actual_withdrawn, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(withdrawn_table)

##LOSS TO FOLLOW UPS (POST TREATMENT) BY PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
withdrawal_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

#PARTICIPANT DEATHS BY SITE
site_monitoring <- site_monitoring %>%
  body_add_par("Number of participant deaths by site", style = "heading 1")

withdrawn <- subset(dataset, redcap_event_name == 'Withdrawal' & witype == 'Death of participant') 
if(nrow(withdrawn)>0){
  actual_withdrawals <- table(withdrawn$redcap_data_access_group)
  actual_withdrawn <- data.frame(value = actual_withdrawals)
  actual_wide <- pivot_wider(actual_withdrawn, names_from = value.Var1, values_from = value.Freq, values_fill = 0)
  missing_columns <- setdiff(expected_sites, names(actual_wide))
  for (column in missing_columns) {
    actual_wide[[column]] <- 0
  }
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
  site_monitoring <- site_monitoring %>% 
    body_add_flextable(withdrawn_table)
} else {
  expected_counts <- table(factor(expected_sites, levels = expected_sites))
  actual_wide <- expected_counts * 0
  actual_wide <- data.frame(actual_wide)
  actual_wide <- pivot_wider(actual_wide, names_from = Var1, values_from = Freq, values_fill = 0)
  actual_wide <- actual_wide[, sort(names(actual_wide))]
  withdrawn_table <- flextable(actual_wide)
}

site_monitoring <- site_monitoring %>% 
  body_add_flextable(withdrawn_table)

##PARTICIPANT DEATHS BY PARTICIPANT
site_monitoring <- site_monitoring %>%
  body_add_par("As a proportion of enrolled participants", style = "heading 2")
proportions <- actual_wide

for (column_name in colnames(enrolled)) {
  if (column_name %in% colnames(actual_wide)) {
    proportions[[column_name]] <- actual_wide[[column_name]] / enrolled[[column_name]]
  }
}
withdrawal_table <- flextable(proportions)
site_monitoring <- site_monitoring %>% 
  body_add_flextable(query_table)

site_monitoring <- site_monitoring %>%
  body_end_section_landscape()

file_name <- paste0(today, "_<STUDY NAME>_SiteMonitoring.docx") #\substitute <STUDY NAME> for the name of the study

#Create the word doc
print(site_monitoring, target=file_name)