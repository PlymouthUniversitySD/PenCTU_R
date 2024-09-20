#Script Title: <STUDY NAME> Weekly Report #\substitute <STUDY NAME> for the name of the study
#Author: <AUTHOR NAME> #\substitute <AUTHOR NAME> for your name
#Date/Verion: DDMMMYYYY VX.X #\add the date the script was published and the version number
#R Version X.X.X #\add the instance of R you wrote the script using (displayed in first line of console)

#\HOW TO USE THIS TEMPLATE:
#\Lines that start with '#' are script annotations and should remain in the script. Lines that start with '#\' are template instructions and 
#\should be deleted.

#Define API code
token <- "your_token" #NEVER SAVE YOUR API CODE IN THE SCRIPT

#Load packages
library(officer)
library(tidyr)
library(ggplot2)
library(usethis)
library(devtools)
library(ggconsort)
library(dplyr)
library(gtsummary)
library(flextable)
library(crosstable)
library(PenCTU)
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
#Raw dataset
raw_dataset <- httr::content(response)

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
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
#Labelled dataset
labelled_dataset <- httr::content(response)

#Import data dictionary via REDCap API
formData <- list("token"=token,
                 content='metadata',
                 format='csv',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
data_dictionary <- httr::content(response)

#Set today's date
today <- as.Date(format(Sys.Date(),"%Y-%m-%d"))

#Create weekly report document
weekly_report <- read_docx()

#Insert a title
title <- paste0("Weekly Report <STUDY NAME> - ", today) #\substitute <STUDY NAME> for the name of the study
weekly_report <- weekly_report %>%
  body_add_par(title,style = "Normal")

##EXPECTED VS ACTUAL RECRUITMENT##

#Insert subheadings
weekly_report <- weekly_report %>%
  body_add_par("Recruitment", style = "heading 1")
weekly_report <- weekly_report %>%
  body_add_par("Expected Vs Actual Recruitment", style = "heading 2")

#Format site name
raw_dataset$Site <- gsub("^\\d+__", "", raw_dataset$redcap_data_access_group)
raw_dataset$Site <- gsub("(^|_)([a-z])", "\\1\\U\\2", raw_dataset$Site, perl = TRUE)
labelled_dataset$Site <- gsub("^\\d{2}\\s-\\s", "", labelled_dataset$redcap_data_access_group)

#Add allocation data #\delete this section if there is no randomisation in the study
#\below substitute pdallocation for the name of the randomisation allocaton field. substitiute randomisation_arm_1 for the name of the randomisation event
raw_dataset <- allocation_data_cleaning(raw_dataset, data_dictionary, "pdallocation", "randomisation_arm_1") 
allocation_data <- subset(raw_dataset, redcap_event_name == 'randomisation_arm_1') #\substitiute randomisation_arm_1 for the name of the randomisation event
allocation_data <- select(allocation_data, record_id, Allocation)
labelled_dataset <- merge(labelled_dataset, allocation_data, by = 'record_id', all.x=TRUE)

#filter dataset for recruitment events only
enrolled <- subset(raw_dataset, redcap_event_name=="randomisation_arm_1") #\substitiute randomisation_arm_1 for the name of the enrollment event

#plot Expected Vs Actual Recruitment
#\below substitute 2024-08-01 for the start date of recruitment (in YYYY-MM-DD format). substitute 18 for the number of months recruitment is open for.
#\substitute 84 for the recruitment target. substitute pdrandomisationdat for the date of enrollment field
recruitment_plot <- plot_recruitment_overall(enrolled, "2024-08-01", 18, 84, "pdrandomisationdat") 

#insert recruitmemt plot
weekly_report <- weekly_report %>%
  body_add_gg(recruitment_plot, width = 7, height = 7)

#Add informative note 
#\use this note to explain which participants are classed as enrolled. Delete section if not required.
weekly_report <- weekly_report %>%
  body_add_par("Note: Participants who have been randomised are classed as recruits.", style="Normal")

##RECRUITMENT BY SITE##

#insert subheadings
weekly_report <- weekly_report %>%
  body_add_par("Recruitment by site", style = "heading 2")

#Set site names
#\below substitute <SITE 1/2> for your site names without numbering before them (e.g. 01 - Plymouth in REDCap would be 'Plymouth').
#\add additional sites in quotation marks and seperated by a comma as required
site_names <- c("<SITE 1>","<SITE 2>") #\substitute <SITE 1/2> for your site names without numbering before them (e.g. 01 - Plymouth in REDCap would be 'Plymouth')

#plot recruitment by site
#\below substitute pdrandomisationdat for the date of enrollment field
recruitment_by_site_plot <- plot_recruitment_by_site("2024-08-01", enrolled$pdrandomisationdat,enrolled$Site,site_names,recruitment_end = today) 

weekly_report <- weekly_report %>%
  body_add_gg(recruitment_by_site_plot, width = 7, height = 7)

#Add informative note
#\use this note to explain which participants are classed as enrolled. Delete section if not required.
weekly_report <- weekly_report %>%
  body_add_par("Note: Participants who have been randomised are classed as recruits.", style="Normal")

###PARTICIPANT FLOW DIAGRAM###
#insert subheadings
weekly_report <- weekly_report %>%
  body_add_par("Participant flow diagram", style = "heading 1")

#\insert the appropriate participant flow template script (based on the template that best suits your study)

##DEMOGRAPHICS##

#Convert to landscape
weekly_report <- weekly_report %>%
  body_end_section_portrait()

#Insert subheadings
weekly_report <- weekly_report %>%
  body_add_par("Key demographics", style = "heading 1")

#\delete the next two lines if study does not have any randomisation
weekly_report <- weekly_report %>%
  body_add_par("By site", style = "heading 2")

#Filter labelled_dataset for demographics data only
demographics_data <- subset(labelled_dataset, redcap_event_name=="Baseline") #\substitute Baseline for the event label of the event containing demographics data

#Define demographics dataset
#\substitute dmage, dmsexatbirth, dmethnic for the names of the demographics fields, add more as required seperated by a comma
demographics_data <- select(demographics_data,Site,dmage,dmsexatbirth,dmethnic)

#Generate demographics by site table
demographics_table <- demographics_data %>%
  tbl_summary(by=Site,
              #\below substitute the demographics field names for your own field names and add the field name in quotations after the ~ symbol
              label=list(dmage~"Age",dmsexatbirth~"Sex at birth",dmethnic~"Ethnicity"), 
              #\below substitute the demographics field names for your own field names and add whether they are continuous or categorical variables after the ~symbol
              type=list(dmage~"continuous",dmsexatbirth~"categorical",dmethnic~"categorical"),
              statistic=list(all_categorical()~"{n} / {N} ({p}%)",
                             all_continuous()~"{mean} ({sd})"))%>%
  italicize_levels()%>%
  bold_labels()%>%
  modify_caption("Participant Demographics by Site")%>%
  as_flex_table()

#Insert demographics table
weekly_report <- weekly_report %>% 
  body_add_flextable(demographics_table)

#Add informative note
weekly_report <- weekly_report %>%
  body_add_par("Note: Unknown represents instances where data is incomplete for the given field.", style="Normal")

#Add break
weekly_report <- weekly_report %>% 
  body_add_break()

#\Delete from here to '#\---------------------------------------------' if no randomisation occurs in study
weekly_report <- weekly_report %>%
  body_add_par("By allocation", style = "heading 2")

#Filter labelled_dataset for demographics data only
baseline <- subset(labelled_dataset, redcap_event_name=="Baseline") #\substitute Baseline for the event label of the event containing demographics data

#Define demographics dataset
#\substitute dmage, dmsexatbirth, dmethnic for the names of the demographics fields, add more as required seperated by a comma
demographics_data <- select(baseline,Allocation,dmage,dmsexatbirth,dmethnic)

#Generate demographics by allocation table
demographics_table <- demographics_data %>%
  tbl_summary(by=Allocation,
              #\below substitute the demographics field names for your own field names and add the field name in quotations after the ~ symbol
              label=list(dmage~"Age",dmsexatbirth~"Sex at birth",dmethnic~"Ethnicity"),
              #\below substitute the demographics field names for your own field names and add whether they are continuous or categorical variables after the ~symbol
              type=list(dmage~"continuous",dmsexatbirth~"categorical",dmethnic~"categorical"),
              statistic=list(all_categorical()~"{n} / {N} ({p}%)",
                             all_continuous()~"{mean} ({sd})"))%>%
  italicize_levels()%>%
  bold_labels()%>%
  modify_caption("Particpant Demographics by Allocation")%>%
  as_flex_table()

#Insert demographics table
weekly_report <- weekly_report %>% 
  body_add_flextable(demographics_table)

#Add informative note
weekly_report <- weekly_report %>%
  body_add_par("Note: Unknown represents instances where data is incomplete for the given field.", style="Normal")

#Add break
weekly_report <- weekly_report %>% 
  body_add_break()

#\---------------------------------------------

###DATA COMPLETENESS###

#Convert to portrait
weekly_report <- weekly_report %>%
  body_end_section_landscape()

weekly_report <- weekly_report %>%
  body_add_par("Timepoint completeness tables", style = "heading 1")

#\add the following section of code from here down to '#\---------------------------------------------' for each timepoint that you
#\want to report completeness for

#<TIMEPOINT 1> #\substitute <TIMEPOINT 1> with the name of the timepoint where completeness is being reported
if ('day_30_arm_1' %in% raw_dataset$redcap_event_name) { #\substitute day_30_arm_1 with the name of the event
  
  #\below substitute <timepoint> for the name of the timepoint (do not include any spaces)
  #\belowsubstitute day_30_arm_1 with the name of the event
  completeness_table_<timepoint> <- table_crf_completeness(raw_dataset, "day_30_arm_1", "Site", token, test=TRUE) #\remove ', test=TRUE' when using the live REDCap instance
  
  #Insert table
  weekly_report <- weekly_report %>% 
    body_add_flextable(completeness_table_<timepoint>) #\below substitute <timepoint> for the name of the timepoint (do not include any spaces)
  
  #Add break
  weekly_report <- weekly_report %>% 
    body_add_break()
}
#\---------------------------------------------

###WITHDRAWALS###

#Add header
weekly_report <- weekly_report %>%
  body_add_par("Withdrawals", style = "heading 1")

if ('Withdrawal' %in% labelled_dataset$redcap_event_name) { #\replace Withdrawal reporting with the label of the withdrawal event
  
  #Subset withdrawal data 
  withdrawal_data <- subset(labelled_dataset, redcap_event_name == 'Withdrawal') #\replace Withdrawal reporting with the label of the withdrawal event
  withdrawal_data <- select(withdrawal_data, record_id, Site, witype, widecision, wiwithdrawaldat, 
                            wilastcontact, wideathdat, wireason, wireasonft)
  
  #Coalesce withdrawal date columns
  withdrawal_date_columns <- c("wiwithdrawaldat", "wilastcontact", "wideathdat")
  withdrawal_data <- unite(withdrawal_data, 
                           col = "withdrawal_date",
                           withdrawal_date_columns, 
                           sep = "", 
                           na.rm = TRUE)
  
  #Replace 'Other' in withdrawal reason column with free text reason
  withdrawal_data <- withdrawal_data %>%
    mutate(wireason = ifelse(wireason == "Other", NA, wireason))
  withdrawal_reason_columns <- c("wireason", "wireasonft")
  withdrawal_data <- unite(withdrawal_data, 
                           col = "withdrawal_reason",
                           withdrawal_reason_columns, 
                           sep = "", 
                           na.rm = TRUE)
  
  #Rename columns
  withdrawal_data <- withdrawal_data %>%
    rename('Participant ID' = record_id)
  withdrawal_data <- withdrawal_data %>%
    rename('Withdrawal date' = withdrawal_date)
  withdrawal_data <- withdrawal_data %>%
    rename('Person making decision to withdrawal participant' = widecision)
  withdrawal_data <- withdrawal_data %>%
    rename('Withdrawal reason' = withdrawal_reason)
  withdrawal_data <- withdrawal_data %>%
    rename('Withdrawal type' = witype)
  
  #Create withdrawal table
  withdrawal_table <- flextable(withdrawal_data)
  
  #Insert withdrawal table
  weekly_report <- weekly_report %>% 
    body_add_flextable(withdrawal_table)
  
} else {
  
  #If there are no withdrawal rows print the following
  weekly_report <- weekly_report %>%
    body_add_par("No withdrawals reported to date.", style="Normal")
}

#Add break
weekly_report <- weekly_report %>% 
  body_add_break()

###SAEs###

#Add header
weekly_report <- weekly_report %>%
  body_add_par("SAEs", style = "heading 1")

if ('SAE reporting' %in% labelled_dataset$redcap_event_name) { #\replace SAE reporting with the label of the SAE reporting event
  
  #select withdrawal data
  sae_data <- subset(labelled_dataset, redcap_event_name =='SAE reporting') #\replace SAE reporting with the label of the SAE reporting event
  sae_data <- subset(labelled_dataset, aeseriousyn =='Yes')
  sae_data <- select(sae_data, record_id, Site, aetitle, aeonsetdat, aeseverity, aeoutcome1, aeresolve1dat, aeoutcome2, aerecover2dat, aecitrialyn, aeciexpect)
  
  #Format and coalesce outcome columns
  sae_data$aeoutcome1[sae_data$aeoutcome1 != "Recovered" & sae_data$aeoutcome1 != "Recovered with sequalae" & sae_data$aeoutcome1 != "Died"] <- ""
  sae_outcome_columns <- c("aeoutcome1", "aeoutcome2")
  sae_data <- unite(sae_data, 
                    col = "sae_outcome",
                    sae_outcome_columns, 
                    sep = "", 
                    
                    na.rm = TRUE)
  #Coalesce outcome date columns
  sae_date_columns <- c("aeresolve1dat", "aerecover2dat")
  sae_data <- unite(sae_data, 
                    col = "sae_outcome_date",
                    sae_date_columns, 
                    sep = "", 
                    na.rm = TRUE)
  
  #Reorder columns
  sae_data <- select(sae_data, record_id, Site, aetitle, aeonsetdat, sae_outcome, sae_outcome_date, aeseverity, aecitrialyn, aeciexpect)
  
  #Rename columns
  sae_data <- sae_data %>%
    rename('Participant ID' = record_id)
  sae_data <- sae_data %>%
    rename('SAE diagnosis' = aetitle)
  sae_data <- sae_data %>%
    rename('Onset date' = aeonsetdat)
  sae_data <- sae_data %>%
    rename('Outcome' = sae_outcome)
  sae_data <- sae_data %>%
    rename('Outcome date' = sae_outcome_date)
  sae_data <- sae_data %>%
  sae_data <- sae_data %>%
    rename('Related to trial?' = aecitrialyn) 
  sae_data <- sae_data %>%
    rename('Expectedness' = aeciexpect) 
  
  #Create withdrawal table
  sae_table <- flextable(sae_data)
  
  #Insert SAE table
  weekly_report <- weekly_report %>% 
    body_add_flextable(sae_table)
  
} else {
  
  #If there are no withdrawal rows print the following
  weekly_report <- weekly_report %>%
    body_add_par("No SAEs reported to date.", style="Normal")
}

#define filename
filename <- paste0(today, "_<STUDY NAME>_WeeklyReport.docx") #\replace <STUDY NAME> with name of study

#Create word doc
print(weekly_report,target = filename)
