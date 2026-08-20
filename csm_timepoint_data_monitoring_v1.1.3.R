#Title: Timepoint Data Monitoring
#Author: Eduard Mazuru
#Version & Date: V1.1.3 26JUN2026
#R version: 4.4.3

#Import the study metadata file
metadata <- read.csv("BOOST_Timepoints.csv")

#Set the study name
study_name <- "BOOST"

#Define API token
token <- "053F3C28FF8795D624263A1CD6456FFE" #NEVER SAVE YOUR API TOKEN IN THE SCRIPT

#load libraries
library(dplyr)
library(tidyr)
library(tidyselect)
library(tibble)
library(openxlsx)
library(ggplot2)

#Import dataset
url <- "https://clinicaltrials.plymouth.ac.uk/api/"
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
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
data <- httr::content(response)

#Functions

#Run the "Missing, Overdue, and Out-Of-Window Timepoints" function.
missing_overdue_and_oow_timepoints(
  data = data,
  metadata = metadata
)
