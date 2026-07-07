#Title: AUtomated Script for Study Contact Upload
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUL2026
#R version: 4.4.3

#Load libraries
library(dplyr)
library(httr)
library(jsonlite)

#iImport the mapping csv defining the fields to be uploaded
mapping <- read.csv("BOOST_ContactUpload.csv")

#Define the REDCap URL
url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
#url <- "https://clinicaltrials.plymouth.ac.uk/api/"

#Run the mapping function
results <- cross_project_contact_upload(mapping, url)
