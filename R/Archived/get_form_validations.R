# Author: Paigan Aspinall
# Date: 20FEB2025
# R version: 4.4.2
#' Pulls all of the form validation checks that need to be performe dout of the data dictionary.
#'
#' This function retrieves data from REDCap data dictionary using the API and identifies all of the form validation checks required for the study.
#'
#' @param api_token The API token for accessing REDCap.
#' @param test Logical, indicating whether to use the test environment (default is FALSE).
#'
#' @return A csv containing all of the form validation checks.
#'
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples
#' # Example usage:
#' get_form_validations(your_token, test=TRUE)
#'
#' @export
#' 
#' 

get_form_validations <- function(api_token, test = FALSE){
  
  ## Set URL based on environment
  if(test){
    url <- "https://clinicaltrials-pre.plymouth.ac.uk/api/"
  } else{
    url <- "https://clinicaltrials.plymouth.ac.uk/api/"
  }
  
formData <- list("token"=token,
                 content='metadata',
                 format='csv',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
data_dictionary <- httr::content(response)

#Date
text_validation_date_dmy <- subset(data_dictionary, text_validation_type_or_show_slider_number=="date_dmy")
text_validation_date_dmy$validation_check_type <- "Textbox with validation: Date (D-M-Y)"
text_validation_date_dmy <- select(text_validation_date_dmy, field_name, form_name, validation_check_type)

#Datetime
text_validation_datetime_dmy <- subset(data_dictionary, text_validation_type_or_show_slider_number=="datetime_dmy")
text_validation_datetime_dmy$validation_check_type <- "Textbox with validation: Datetime (D-M-Y HH:MM)"
text_validation_datetime_dmy  <- select(text_validation_datetime_dmy , field_name, form_name, validation_check_type)

#Datetime with seconds
text_validation_datetime_sec_dmy <- subset(data_dictionary, text_validation_type_or_show_slider_number=="datetime_seconds_dmy")
text_validation_datetime_sec_dmy$validation_check_type <- "Textbox with validation: Datetime (D-M-Y HH:MM:SS)"
text_validation_datetime_sec_dmy  <- select(text_validation_datetime_sec_dmy, field_name, form_name, validation_check_type)

#Email
text_validation_email <- subset(data_dictionary, text_validation_type_or_show_slider_number=="email")
text_validation_email$validation_check_type <- "Textbox with validation: Email"
text_validation_email  <- select(text_validation_email, field_name, form_name, validation_check_type)

#Integer
text_validation_integer <- subset(data_dictionary, text_validation_type_or_show_slider_number=="integer")
text_validation_integer$validation_check_type <- "Textbox with validation: Integer"
text_validation_integer  <- select(text_validation_integer, field_name, form_name, validation_check_type)

#Number
text_validation_number <- subset(data_dictionary, text_validation_type_or_show_slider_number=="number")
text_validation_number$validation_check_type <- "Textbox with validation: Number"
text_validation_number  <- select(text_validation_number, field_name, form_name, validation_check_type)

#Number 1dp
text_validation_number_1dp <- subset(data_dictionary, text_validation_type_or_show_slider_number=="number_1dp")
text_validation_number_1dp$validation_check_type <- "Textbox with validation: Number (1 d.p.)"
text_validation_number_1dp  <- select(text_validation_number_1dp, field_name, form_name, validation_check_type)

#Number 2dp
text_validation_number_2dp <- subset(data_dictionary, text_validation_type_or_show_slider_number=="number_2dp")
text_validation_number_2dp$validation_check_type <- "Textbox with validation: Number (2 d.p.)"
text_validation_number_2dp  <- select(text_validation_number_2dp, field_name, form_name, validation_check_type)

#Number 3dp
text_validation_number_3dp <- subset(data_dictionary, text_validation_type_or_show_slider_number=="number_3dp")
text_validation_number_3dp$validation_check_type <- "Textbox with validation: Number (3 d.p.)"
text_validation_number_3dp  <- select(text_validation_number_3dp, field_name, form_name, validation_check_type)

#Phone (UK)
text_validation_phone <- subset(data_dictionary, text_validation_type_or_show_slider_number=="phone_uk")
text_validation_phone$validation_check_type <- "Textbox with validation: Phone (UK)"
text_validation_phone  <- select(text_validation_phone , field_name, form_name, validation_check_type)

#Postcode (UK)
text_validation_postcode <- subset(data_dictionary, text_validation_type_or_show_slider_number=="postalcode_uk")
text_validation_postcode$validation_check_type <- "Textbox with validation: Postcode (UK)"
text_validation_postcode  <- select(text_validation_postcode, field_name, form_name, validation_check_type)

#Time (HH:MM:SS)
text_validation_time_hhmmss<- subset(data_dictionary, text_validation_type_or_show_slider_number=="time_hh_mm_ss")
text_validation_time_hhmmss$validation_check_type <- "Textbox with validation: Time (HH:MM:SS)"
text_validation_time_hhmmss  <- select(text_validation_time_hhmmss, field_name, form_name, validation_check_type)

#Time (HH:MM)
text_validation_time_hhmm<- subset(data_dictionary, text_validation_type_or_show_slider_number=="time")
text_validation_time_hhmm$validation_check_type <- "Textbox with validation: Time (HH:MM)"
text_validation_time_hhmm  <- select(text_validation_time_hhmm, field_name, form_name, validation_check_type)

#Time (MM:SS)
text_validation_time_mmss <- subset(data_dictionary, text_validation_type_or_show_slider_number=="time_mm_ss")
text_validation_time_mmss$validation_check_type <- "Textbox with validation: Time (MM:SS)"
text_validation_time_mmss  <- select(text_validation_time_mmss, field_name, form_name, validation_check_type)

#SQL Query
sql_query <- subset(data_dictionary, field_type=="sql")
sql_query$validation_check_type <- paste0("SQL query (", sql_query$select_choices_or_calculations, ")")
sql_query  <- select(sql_query, field_name, form_name, validation_check_type)

#Calculated field
calculated_field <- subset(data_dictionary, field_type=="calc")
calculated_field$validation_check_type <- paste0("Calculated field (", calculated_field$select_choices_or_calculations, ")")
calculated_field  <- select(calculated_field, field_name, form_name, validation_check_type)

#Minimum value
minimum_value <- subset(data_dictionary, text_validation_min!='' & !is.na(text_validation_min))
minimum_value$validation_check_type <- paste0("Minimum value (", minimum_value$text_validation_min, ")")
minimum_value  <- select(minimum_value, field_name, form_name, validation_check_type)

#Maximum value
maximum_value <- subset(data_dictionary, text_validation_max!='' & !is.na(text_validation_max))
maximum_value$validation_check_type <- paste0("Minimum value (", maximum_value$text_validation_max, ")")
maximum_value  <- select(maximum_value, field_name, form_name, validation_check_type)

#Branching logic
branching_logic <- subset(data_dictionary, branching_logic!='' & !is.na(branching_logic))
branching_logic$validation_check_type <- paste0("Maximum value (", branching_logic$branching_logic, ")")
branching_logic  <- select(branching_logic, field_name, form_name, validation_check_type)

#Action Tags
action_tags <- data_dictionary %>% filter(str_detect(field_annotation, "@"))
action_tags <- action_tags %>%
  mutate(field_annotation = str_extract_all(field_annotation, "@\\S+")) %>%  
  unnest(field_annotation)
action_tags <- action_tags %>%
  mutate(field_annotation = str_replace(field_annotation, "[()=].*", ""))
action_tags$validation_check_type <- paste0("Action tag (", action_tags$field_annotation, ")")
action_tags  <- select(action_tags, field_name, form_name, validation_check_type)

#Embedding (field label)
embedding_name <- data_dictionary[grepl("\\{|\\}", data_dictionary$field_label), ]
embedding_name$validation_check_type <- paste0("Embedding in field label")
embedding_name  <- select(embedding_name, field_name, form_name, validation_check_type)

#Embedding (response options)
embedding_response <- data_dictionary[grepl("\\{|\\}", data_dictionary$select_choices_or_calculations), ]
embedding_response <- subset(embedding_response, field_type == 'radio'| field_type=='checkbox'| field_type=='dropdown')
embedding_response$validation_check_type <- paste0("Embedding in response options")
embedding_response  <- select(embedding_response, field_name, form_name, validation_check_type)

#Piping (field label)
piping_name <- data_dictionary[grepl("\\[|\\]", data_dictionary$field_label), ]
piping_name$validation_check_type <- paste0("Piping in field label")
piping_name  <- select(piping_name, field_name, form_name, validation_check_type)

#Piping (response options)
piping_response <- data_dictionary[grepl("\\[|\\]", data_dictionary$select_choices_or_calculations), ]
piping_response <- subset(piping_response, field_type == 'radio'| field_type=='checkbox'| field_type=='dropdown')
piping_response$validation_check_type <- paste0("Piping in response options")
piping_response  <- select(piping_response, field_name, form_name, validation_check_type)

#Combine validation checks
form_validation <- rbind(piping_response, piping_name, embedding_response, embedding_name, action_tags, 
                         branching_logic, maximum_value , minimum_value, calculated_field, sql_query,
                         text_validation_date_dmy, text_validation_datetime_dmy, text_validation_datetime_sec_dmy,
                         text_validation_email, text_validation_integer, text_validation_number, text_validation_number_1dp,
                         text_validation_number_2dp, text_validation_number_3dp, text_validation_phone , text_validation_postcode,
                         text_validation_time_hhmmss, text_validation_time_hhmm, text_validation_time_mmss)
form_validation <- form_validation[order(form_validation$form_name), ]

#Write as a csv
write.csv(form_validation, "FormValidation.csv")
}
