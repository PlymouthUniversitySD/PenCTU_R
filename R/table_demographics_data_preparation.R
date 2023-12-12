#' Prepare a dataset from which to plot a demographics summary table.
#'
#' This function calculates the age at registration (if required) and isolates the desired demographics data into a new dataset.
#'
#' @param dataset The dataset where the demographics data is being pulled from.
#' @param demographic_columns A vector of desired columns to be presented in the demographics table, separated by a comma.
#' @param demographics_event The name of the event in which demographics data is collected and where the demographic_columns are located.
#' @param dob_column The name of the date of birth column. NULL by default.
#' @param anchor_column The column location of the date from which age will be calculated (e.g. age at registration). NULL by default.
#' @param anchor_event The name of the event where the anchor_column is collected. NULL by default.

#'
#' @return A data frame containing the desired demographics data.
#'
#' @details This function calculated the age at the anchor_column date, if required, and then 
#' selects all fo the desired demographics columns into a concise dataset.
#'
#' @import dplyr
#' @import lubridate

#' @examples
#' # Example usage:
#' 
#' demographic_columns <- c("dmsexatbirth", "dmethnic", "dmemployment", "dmlivewith", "dmmarital", "dmindependence")
#'
#' demographics_table_data <- table_demographics_data_preparation(dataset, demographic_columns, "facetoface_screeni_arm_1",
#'                                                                    "dmyob", "pdregisterdat", "participant_identi_arm_1")
#' example downstream useage:
#' baseline_characteristics <-  demographics_table_data %>% 
#' tbl_summary(by = category,
#'             label = list(Age ~ "Age", dmsexatbirth ~ "Sex at birth", dmethnic ~ "Ethnicity",
#'                          dmemployment ~ "Employment status", dmlivewith ~ "Number of cohabitants",
#'                          dmmarital ~ "Marital status", dmindependence ~ "Independence level"),
#'             type = list(Age ~ "continuous", dmsexatbirth ~ "categorical", dmethnic ~ "categorical",
#'                         dmemployment ~ "categorical", dmlivewith ~ "categorical",
#'                         dmmarital ~ "categorical", dmindependence ~ "categorical"),
#'             statistic = list(all_categorical() ~ "{n} / {N} ({p}%)",
#'                              all_continuous() ~ "{mean} ({sd})")) %>%
#'   italicize_levels() %>%
#'   bold_labels() %>%
#'   modify_caption("**Participant Characteristics**") %>%
#'   as_flex_table()
#' }

#'
#' @export
#' 

table_demographics_data_preparation <- function(dataset, demographic_columns, demographics_event,
                                                dob_column = NULL, anchor_column = NULL, anchor_event = NULL) {
  
  #if age is being calculated from DOB/registration date
  if(!is.na(dob_column) & !is.na(anchor_column)){ 
    
    #convert dates into correct date format
    dataset[[anchor_column]] <- as.Date(as.character(dataset[[anchor_column]]), format = "%Y-%m-%d")
    dataset[[dob_column]] <- as.Date(as.character(dataset[[dob_column]]), format = "%Y-%m-%d")
    
    #create a dataset of only demographics events with record is and DOB
    demographics_event_data <- subset(dataset, redcap_event_name == demographics_event)
    demographics_event_data <- select(demographics_event_data, record_id, dob_column)
    
    #create a dataset of only registration event with record id and registration date
    anchor_event_data <- subset(dataset, redcap_event_name == anchor_event)
    anchor_event_data <- select(anchor_event_data, record_id, anchor_column)
    
    #join the datasets so the dob and registration date are located on the same row
    result <- merge(anchor_event_data, demographics_event_data, by = "record_id", all.x = TRUE)
    
    #calculate the age at registration in years
    result$Age <- as.numeric(floor((difftime((result[[anchor_column]]), (result[[dob_column]]), units = "days"))/365.25))
    
    #merge the Age column with the rest of the data
    result <- select(result, record_id, Age)
    merged_dataset <- merge(dataset, result, by = "record_id", all.x=TRUE)

    #define the demographics fields to be selected for the demographics dataset
    demographic_columns <- c("Age", demographic_columns)
  }
  
  #create a dataset of only the demographics event with only the desired demographics columns
  merged_dataset <- subset(merged_dataset, redcap_event_name == demographics_event)
  merged_dataset <- select(merged_dataset, category_column, demographic_columns)

  return(merged_dataset)
}
  


