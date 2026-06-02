#Title: TEMPLATE Central Statistical Monitoring
#Author: Paigan Aspinall
#Version & Date: V1.0.0 02JUN2026
#R version: 4.4.3

#Import the study metadata file
metadata <- read.csv("STUDYNAME_CriticalDataItems.csv") =Insert your critical data item file name here

#Set the study name
study_name <- "STUDY NAME" #Insert your study name here

document_id <- 'XX###' #Insert the output document ID here

#Define API token
token <- "" #NEVER SAFE YOUR API TOKEN IN THE SCRIPT

#load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(car)
library(kSamples)
library(purrr)
library(PenCTU)

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

#Set today's date
today <- Sys.Date()

#Produce an overall summary
overall_summary <- overall_distribution_summary(
  data = data,
  metadata = metadata,
  variable_type_column = "variable_type",
  required_column = "required_yn",
  field_name_column = "field_name"
)

#Produce a site breakdown summary
site_summary <- site_distribution_summary(
  data = data,
  metadata = metadata,
  variable_type_column = "variable_type",
  required_column = "required_yn",
  field_name_column = "field_name",
  site_column = "redcap_data_access_group"
)

#Run the Z-score function
z_score_outputs <- z_score_analysis(
  data = data,
  metadata = metadata
)

#Pull out the Z-score output
z_scores <- z_score_outputs$z_scores
z_flags <- z_score_outputs$z_flags

#Run the mahalanobis function
mahalanobis_outputs <- mahalanobis_analysis(
  data = data,
  metadata = metadata
)

#Pull out the mahalanobis output
mahalanobis_results <- mahalanobis_outputs$mahalanobis_results

#Run the Brown-Forsythe function
variance_outputs <- variance_analysis(
  data = data,
  metadata = metadata
)

#Pull out the Brown-Forsythe output
variance_results <- variance_outputs$variance_results

#Run the Anderson-Darling function
ad_outputs <- ad_distribution_analysis(
  data = data,
  metadata = metadata
)

#Pull out the Anderson-Darling output
ad_results <- ad_outputs$ad_results

#Run the k-means function
cluster_outputs <- clustering_analysis(
  data = data,
  metadata = metadata
)

#Pull out the k-means output
cluster_results <- cluster_outputs$cluster_results
site_cluster_summary <- cluster_outputs$site_cluster_summary
cluster_flags <- cluster_outputs$cluster_flags

#Run the missing data function
missing_results <- missing_data_analysis(data, metadata)

#Run duplicate detection function
similarity_results <- similarity_detection(
  data = data,
  metadata = metadata
)

#Run the digit preference function
decimal_results <- decimal_preference_analysis(
  data = data,
  metadata = metadata
)

#Define Xlsx file name
file_name <- paste0(study_name, "_CSMOutput_", today, ".xlsx")

export_csm_workbook(
  document_id = document_id,
  study_name = study_name,
  overall_distribution_summary = overall_summary,
  site_distribution_summary = site_summary,
  z_scores = z_scores,
  mahalanobis_results = mahalanobis_results,
  variance_results = variance_results,
  ad_results = ad_results,
  site_cluster_summary = site_cluster_summary,
  missing_data_results = missing_results,
  similarity_results = similarity_results,
  decimal_preference_results = decimal_results,
  file = file_name
)
