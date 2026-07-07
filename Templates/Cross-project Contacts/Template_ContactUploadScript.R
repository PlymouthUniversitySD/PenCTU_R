#Title: Automated Script for Study Contact Upload - TEMPLATE 
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUL2026
#R version: 4.4.3

#Load libraries
library(dplyr)
library(httr)
library(jsonlite)

#Import the mapping csv defining the fields to be uploaded
mapping <- read.csv("XXX_ContactUpload.csv")

#Define your study name
study_name <- "STUDY NAME"

#Define the REDCap URL
url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
#url <- "https://clinicaltrials.plymouth.ac.uk/api/"

#Run the mapping function
results <- cross_project_contact_upload(mapping, url)

#Save the report out
today <- Sys.Date()
file_name <- paste0(study_name, "_ContactUploadReport_", today, ".csv")
write.csv(results, file_name)
