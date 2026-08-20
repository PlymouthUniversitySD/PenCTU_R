#Title: CtQ Monitoring TEMPLATE
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUN2026
#R version: 4.4.3

#Define API token(s)
data_capture_token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT
safety_token <- "" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if project not required
site_token <- "YOUR TOKEN 3" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if project not required
screening_token <- "YOUR TOKEN 4" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT - delete if project not required

#Define study name
study_name <- "STUDY NAME" #Enter your study name here, this will display in the exported file name
document_id <- "DOCUMENT ID" #Enter the document ID of the ATR here

#Load metadata
qtl_metadata <- read.csv("STUDYNAME_qtl_metadata.csv")
kri_metadata <- read.csv("STUDYNAME_kri_metadata.csv")

library(PenCTU)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

#Import data capture data set - dataset names should correspond with the export_name field in your metadata
url <- "https://clinicaltrials.plymouth.ac.uk/api/"
formData <- list("token"=data_capture_token,
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
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
data_capture <- httr::content(response)

#Import safety data dataset - delete if dataset not required
formData <- list("token"=safety_token,
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
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
safety_data <- httr::content(response)

#Import site data dataset - delete if dataset not required
formData <- list("token"=site_token,
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
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
site_data <- httr::content(response)

#Import screening data dataset - delete if dataset not required
formData <- list("token"=screening_token,
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
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
screening_data <- httr::content(response)

#Run the QTL monitoring script
today <- Sys.Date()
qtl_title <- paste0(document_id, "_", study_name, "_QTLReport_", today, ".xlsx")
qtl_results <- run_qtl_monitoring(qtl_metadata, qtl_title)

#Run the KRI monitoring script
kri_title <- paste0(document_id, "_", study_name, "_KRIReport_", today, ".xlsx")
kri_results <- produce_kri_report(kri_metadata, kri_title)


