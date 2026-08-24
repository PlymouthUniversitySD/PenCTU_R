#Title: NC Log builder - TEMPLATE
#Author: Paigan Aspinall
#Version & Date: V1.0.0 24AUG2026
#R version: 4.4.3

#Load libraries
library(dplyr)
library(httr)
library(jsonlite)
library(PenCTU)
library(tidyr)

#Set token(s) - format to suit your study
family_token <- "" #NEVER SAVE YOUR API TOKEN INTO THIS DOCUMENT
participant_token <- "" #NEVER SAFE YOUR API TOKEN IN THE SCRIPT

#Set your initials and job role
initials <- "" #ENTER YOUR INITIALS HERE
job_role <- "" #ENTER YOUR JOB TITLE HERE

#Load NC metadata
nc_metadata <- read.csv("STUDYNAME_NCPresets.csv")

#Set URl
url <- "https://clinicaltrials.plymouth.ac.uk/api/"

#Load dataset(s) - format to suit your study
formData <- list("token"= family_token,
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
families <- httr::content(response)

formData <- list("token"= participant_token,
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
participants <- httr::content(response)

#token look up - format to suit your study
datasets <- list(
  families     = families,
  participants = participants
)

tokens <- list(
  families     = family_token,
  participants = participant_token
)

#Run the NC log function
nc_log <- build_nc_log(nc_metadata, initials, job_role, datasets, tokens, url)

write.csv(nc_log, "NC_LOG.csv")
