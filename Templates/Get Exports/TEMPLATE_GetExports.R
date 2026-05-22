#Title: Get REFRESH Exports
#Author: Paigan Aspinall
#Version & Date: V1.0.0 27APR2026
#R version: 4.4.3

token <- "YOUR TOKEN" #NEVER SAVE YOUR API TOKEN IN THE R SCRIPT
environment <- "live / test" #Delete as appropriate to get exports from the live or test environment
study_name <- "STUDY NAME" #Enter your study name here, this will display in the exported file name
export_metadata <- read.csv("export_metadata_file_name.csv") #Enter the name of your file where the metadata about exports is stored

#Run the function

get_redcap_exports(export_metadata, token, environment, study_name)
