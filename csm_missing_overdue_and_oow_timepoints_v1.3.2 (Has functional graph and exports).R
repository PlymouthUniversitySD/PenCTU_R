#Title: Central Statistical Monitoring - Missing, Overdue, and Out-Of-Window Timepoints Data Function
#Author: Eduard Mazuru
#Version & Date: V1.3.2 26JUN2026
#R version: 4.4.3
#' Exports '.xlsx' excel workbook with sheets corresponding to study-wide required event timepoints that are:
#' missing - and the days overdue, out-of-window, in-window, or excluded.
#'
#' This function lists timepoints for required events and the number of days overdue or out of window,
#' using expected field names, timepoint names and date window definitions from a metadata csv.
#' The function separates the list by category into different sheets on an excel workbook.
#' Function also exports .CSV files for out-of-window timepoints and missing timepoints.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#'
#' @return An excel workbook containing timepoints, the days out of window, days overdue, and orders by participant ID and event name.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr "filter"
#' @importFrom dplyr "select"
#' @importFrom dplyr "setdiff"
#' @importFrom dplyr "bind_rows"
#' @importFrom dplyr "pull"
#' @importFrom dplyr "relocate"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyselect "last_col"
#' @importFrom tidyselect "everything"
#' @importFrom openxlsx "createStyle"
#' @importFrom openxlsx "createWorkbook"
#' @importFrom openxlsx "addWorksheet"
#' @importFrom openxlsx "writeData"
#' @importFrom openxlsx "freezePane"
#' @importFrom openxlsx "setColWidths"
#' @importFrom openxlsx "conditionalFormatting"
#' @importFrom openxlsx "saveWorkbook"
#' @importFrom openxlsx "write.xlsx"
#'
#' @examples
#' missing_overdue_and_oow_timepoints(data, metadata)
#'
#' @export
#' study_CSM_Missing_Overdue_and_OOW_Timepoints_YYYY-MM-YY.xlsx
#'

#FUNCTION-------------------------------------------------------------------------------------------------------------------------------------

# Define the function "missing_overdue_and_oow_timepoints" using parameters "data" and "metadata"
missing_overdue_and_oow_timepoints <- function(data, metadata) {
  
  #REQUIRED EVENTS--------------------------------------------------------------SECTION 1-----------------------------------------------------
  
  #Defining required events (and matching baseline events) into new character vectors.
  events <- metadata$timepoint_name
  baselines <- metadata$baseline_name #These are somewhat redundant.
  #Defining a new vector with all events including baseline.
  events_include_bl <-unique(append(baselines, events))
  
  
  #PARTICIPANT IDS--------------------------------------------------------------SECTION 2-----------------------------------------------------
  
  #Defining character vector of unique participant IDs within the data set (to be compared against IDs which have completed events).
  record_ids <- unique(metadata$record_id_field_name)
  data_unique_IDs <- unique(
    dplyr::select(data, 
                  all_of(record_ids)
                  )
  )
  
  #Adds a column corresponding to the number of the participant, not equal to their participant ID.
  participant_count <- data_unique_IDs
  participant_count <- mutate(data_unique_IDs, count = row(data_unique_IDs))
  participant_count <- dplyr::select(participant_count, count)
  
  #Store data from metadata file into data frame sites_by_unique_IDs in which each participant's site/DAG (Data Access Group) will be stored.
  #First, filter out record ids and the redcap_data_access_group (site).
  sites_by_unique_IDs <- dplyr::select(data, 
                                       redcap_data_access_group,
                                       all_of(record_ids)
                                       )
  #Then filter duplicate rows, which should leave a 1 to 1 map of record IDs to site/DAG.
  sites_by_unique_IDs <- dplyr::distinct(sites_by_unique_IDs)
  
  #Additionally define list of sites/DAGs
  all_sites <- dplyr::select(sites_by_unique_IDs,
                             redcap_data_access_group
                             )
  #Filter to only include unique sites/DAGs
  all_sites <- dplyr::distinct(all_sites)
  
  #Sort list of sites/DAGs
  all_sites <- dplyr::arrange(all_sites,
                              redcap_data_access_group
                              )

  #REQUIRED FIELDS--------------------------------------------------------------SECTION 3-----------------------------------------------------
  
  #Defining character vectors to store field names pulled from the metadata file.
  field_timepoint_yn <- metadata$timepoint_occurred_field_name
  field_timepoint_date <- metadata$timepoint_date_field_name
  field_baseline_yn <- metadata$baseline_occurred_field_name
  field_baseline_date <- metadata$baseline_date_field_name
  
  #Grouping fields into new variables for ease of use.
  
  #Defining a variable to store all fields that store information on whether or not a timepoint occurred
  events_occurredyn_fields <- append(field_baseline_yn,field_timepoint_yn)
  
  #Defining a variable to store all required fields
  required_fields <- unique(c(record_ids,
                              "redcap_event_name",
                              "redcap_data_access_group",
                              field_baseline_yn, 
                              field_baseline_date, 
                              field_timepoint_yn, 
                              field_timepoint_date
                              )
                            )
  
  
  #DEFINING TEMPORARY DATA FRAMES FOR FUNCTION----------------------------------SECTION 4-----------------------------------------------------
  
  #Create temporary data frame to store and manipulate data.
  df <- data %>%
    #Filter to exclude redundant information.
    dplyr::filter(
      #Filter to include only required events listed in metadata.
      redcap_event_name %in% events_include_bl,
      #Filter to remove forms with repeated visit instances.
      is.na(redcap_repeat_instance),
      #Filter to exclude timepoints that have not occurred.
      #This line could have been used if there was only 1 field -> .data[[field_timepoint_yn]] == 1
      if_any(all_of(events_occurredyn_fields), ~ .x == 1)
    ) %>%
    #Select columns to only show required variables for function.
    dplyr::select(all_of(required_fields))
  
  
  #Define all_timepoints dataframe with an empty row to later be filtered out prior to function output/csv export.
  all_timepoints <-data.frame(matrix(nrow = 1, ncol = length(required_fields)))
  #Set the column names of "all_timepoints" to be the same as the required fields pulled from the REDCap database.
  colnames(all_timepoints) <- required_fields
  #Add a column for the days overdue, to be used later.
  all_timepoints <- add_column(all_timepoints, days_overdue = c(NA))
  #Add a column for the days out of window, to be used later.
  all_timepoints <- add_column(all_timepoints, days_out_of_window = c(NA))

  
  #FUNCTION EVALUATION----------------------------------------------------------SECTION 5-----------------------------------------------------
  
  #For Loop: By ID Type---------------------------------------------------------FOR-------1
  
  #For generalisation, evaluate data one form of ID at a time (although it is unlikely for a study to have more than one field for record_id).
  #This assumes only one ID at a time, though. This is not yet generalised to allow for studies in which participants have multiple IDs.
  #That may be required for studies that include a sub-study ID.
  for (xi in 1:ncol(data_unique_IDs)) { 
    
    
    #For Loop: By Unique ID-------------------------------------------------------FOR-------2----------ID-SPLIT----------
    
    #Evaluate data one participant ID at a time.
    for (yi in 1:nrow(data_unique_IDs)) {
      
      #Temporary data frame to store data by ID.
      dfID <- df %>%
        #Filter rows to only include current participant ID.
        dplyr::filter(
          if_any(all_of(record_ids), ~ .x == as.character(data_unique_IDs[yi,xi]))
          
        )
      
      #For Loop: By Timepoint / Event-----------------------------------------------FOR-------3----------EVENT-SPLIT----------
      
      #Evaluate each ID-filtered data frame one event at a time.
      for (j in 1:length(events)) {
        
        #Temporary data frame to store data by event (and ID).
        dfIDevent <- dfID %>%
          #Filter rows to only include current event.
          dplyr::filter(
            redcap_event_name %in% events[j]
          )
        
        #Temporary data frame to store baseline data by event (and ID).
        dfIDbaseline <- dfID %>%
          #Filter rows to only include corresponding baseline.
          dplyr::filter(
            redcap_event_name %in% baselines[j]
          )
        #Define "days_after_baseline" for graph plotting.
        days_after_baseline <- NA
        
        #If Statement: Baseline Event Recording---------------------------------------IF--------1
        
        #Use an 'if' statement to filter whether dfIDbaseline is empty, which should correspond to a baseline event not found recorded for this participant.
        if (nrow(dfIDbaseline) == 0) {
          
          #Define and store 'NA' inside variable "baseline_date" for baseline events that are not found in the data.
          baseline_date <- NA
          #Store a comment about the baseline event for output.
          additional_comment <- "No Baseline Event Recorded"
          
          
          #Else-If: Baseline Event Recording------------------------------------------ELSE------1
          
          #Else if the baseline is not empty, pulls "baseline_date" from dfIDbaseline and stores comments dependent on pulled value.
        } else {
          
          #Define a variable "baseline_date" and fill with the appointment date from the current ID and corresponding baseline event.
          #Appointment date is pulled from dfIDbaseline cell in row 1 and the column which corresponds to the same field title as listed in the metadata.
          baseline_date <- (dplyr::pull(dfIDbaseline[1,field_baseline_date[j]]))
          
          
          #If Statement: Baseline Event Date Comment------------------------------------IF--------2
          
          #Use 'if' statement to filter based on whether a baseline date is recorded for this participant.
          if (is.na(baseline_date)) {
            
            #Store a comment for output.
            additional_comment <- "Baseline Date Not Recorded - Unable To Determine If Overdue"
            
            
            #Else-If: Baseline Event Date Comment---------------------------------------ELSE------2
          } else {
            
            #Store a comment for output.
            additional_comment <- NA
          }
          #Close-If: Baseline Event Date Comment----------------------------------------CLOSE-IF--2
          
        } 
        #Close-If: Baseline Event Recording-------------------------------------------CLOSE-IF--1
        
        
        #If Statement: Timepoint / Event Recorded Check-------------------------------IF--------3----------EVENT-HAS-NOT-OCCURRED----------
        
        #Use 'if' statement to filter based on whether a row is empty, which should correspond to missing events.
        if (nrow(dfIDevent) == 0) {
          
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For previously separate function: Days out of window.
#            
            #Define "days_out_of_window" as NA since it could not be calculated for an event that has not occurred.
            days_out_of_window <- NA
            #Store comment regarding the reason "days_out_of_window" is "NA".
            out_of_window_comment <- "Unable To Define 'Days Out Of Window' - Event Not Recorded"
#            
#            
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
          
          #If Statement: Missing Timepoint / Event Days Overdue Recording---------------IF--------4
          
            #Use further 'if' statement to filter based on whether a date was provided for the baseline visit, 
            #as the amount of days overdue will not be calculable if no baseline date exists.
            if (is.na(baseline_date)) {
              
              #Define a temporary variable "temp_days_overdue" for calculation, but fill with 'NA' if no baseline visit date.
              temp_days_overdue <- NA
              
              
              #Else-If: Missing Timepoint / Event Days Overdue Recording----------------ELSE------4
              #If the baseline date was provided, fill variable "temp_days_overdue" with calculation of days overdue.
            } else {
              
              #Use today's date (Sys.Date()), the provided "baseline_date", and the metadata information about the expected
              #date window to calculate the number of days a visit is overdue, and fill "temp_days_overdue".
              temp_days_overdue <- Sys.Date() - (baseline_date + metadata$expected_days_after_baseline[j] + metadata$plus_days[j])
              
              
              #If Statement: Missing Timepoint / Event Days Overdue Check-----------------IF--------5
              
              #If statement checks whether the timepoint is already overdue and stores a corresponding comment dependent on result.
              if (temp_days_overdue > 0) {
                
                #Store comment based on days overdue
                additional_comment <- paste0("Event Overdue By ", temp_days_overdue ," Days As Of ",Sys.Date())
                
                #Else-If: Missing Timepoint / Event Days Overdue Check--------------------ELSE------5
              } else {
                
                #Store comment based on days overdue
                additional_comment <- paste0("Event Not Yet Overdue"," As Of ",Sys.Date())
              }
              #Close If: Missing Timepoint / Event Days Overdue Check---------------------CLOSE-IF--5
            }
          #Close-If: Missing Timepoint / Event Days Overdue Recording-------------------CLOSE-IF--4
          
          
          #For missing events, fill temporary data frame "dfIDevent" with a defined row based on the current unique participant ID.
          #and the current "redcap_event_name" being evaluated.
          
          #Start by defining the dimensions and column names.
          dfIDevent <-data.frame(matrix(nrow = 1, ncol = length(required_fields)))
          colnames(dfIDevent) <- required_fields
          
          
          #For Loop: Fill Missing Data--------------------------------------------------FOR-------4
          
          #This 'for' loop fills columns based on their positions. 
          #The first few columns (up to the number of columns in data_unique_IDs) are participant IDs, 
          #though this is likely to be just one column. 
          #The next column is where the "redcap_event_name" is stored. 
          #The rest of the columns are filled with 'NA' as this is a missing event and thus has no date.
          for (fi in 1:length(required_fields)) {
            
            #If Statement: ID Columns Check-----------------------------------------------IF--------6
            
            if (fi <= ncol(data_unique_IDs)) {
              
                        #If Statement: Current ID Recording---------------------------------IF--------7
              
                                  if (xi == fi) {
                                    #This fills the ID column(s) with the current ID.
                                    dfIDevent[1,fi] <- data_unique_IDs[yi,xi]
                                    
                                    
                                    #Else-If: Current ID Recording--------------------------ELSE------7
                                  } else {
                                    #This fills other ID columns with NA. 
                                    #(This can be changed if there exist any studies with multiple IDs per participant,
                                    #such as any with sub-study IDs).
                                    NA
                                  }
                                  #Close If: Current ID Recording---------------------------CLOSE-IF--7
                                  
                      #Else-If: ID Columns Check------------------------------------------ELSE------6
              
                                } else {
                                  
                                  #If Statement: REDCap Event Name Recording----------------IF--------8
                                  
                                  #This fills the "redcap_event_name" column with the expected event name pulled from the metadata.
                                  if (fi == (ncol(data_unique_IDs) + 1)) {
                                    dfIDevent[1,fi] <- as.character(events[j])
                                    
                                    #Else-If: REDCap Event Name Recording-------------------ELSE------8
                                  } else {
                                    #All other columns are filled with NA as this is a missing event and does not include the information.
                                    #Baseline information is later manually filled
                                    NA
                                  }
                                  #Close If: REDCap Event Name Recording--------------------CLOSE-IF--8
                                  
                                  NA
                                }  
                        #Close-If: ID Columns Check---------------------------------------CLOSE-IF--6
            
            }
            #Close For Loop: Fill Missing Data------------------------------------------CLOSE-FOR-4
          
          
          #Else-If: Timepoint / Event Recorded Check----------------------------------ELSE------3----------EVENT-HAS-OCCURRED----------
           
            #If the event is not missing, define a variable "temp_days_overdue" and fill with 'NA'.
          #A separate function could be made here to measure days out of window.
          } else {
            
            
            #Define and fill "temp_days_overdue" with 'NA' for events 
            temp_days_overdue <- NA
        
            #If Statement: Timepoint / Event Date Check---------------------------------IF--------9
            
            #If statement checks whether a date was recorded for the timepoint. Then stores a comment depending on result.
            if (is.na(dplyr::pull(dfIDevent[1,field_timepoint_date[j]]))) {
              
              #Store a comment for output.
              additional_comment <- "Event Recorded - No Date Recorded"
              
# # # # # # # # # # # ###########################################EXCLUSIVELY FOR OOW FUNCTION################################oow#oow#oow#
#            
#            #For previously separate function: Days out of window.
#             
            #Define "days_out_of_window" as NA since it could not be calculated without both a baseline date and a visit date.
            days_out_of_window <- NA
            #Store comment regarding the reason "days_out_of_window" is "NA".
            out_of_window_comment <- "Unable To Define 'Days Out Of Window' - No Event Date Recorded"
#           
#            
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#              
              
              #Else-If Statement: Timepoint / Event Date Check--------------------------ELSE------9
            } else {
              
              #Store a comment for output.
              additional_comment <- "Event Recorded"
             
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For separate function: Days out of window.
#            
            #If Statement: C
            #Checks whether the baseline date was recorded
            if (is.na(baseline_date)) {
              #Define "days_out_of_window" as NA since it could not be calculated without both a baseline date and a visit date.
              days_out_of_window <- NA
              #Store comment regarding the reason "days_out_of_window" is "NA".
              out_of_window_comment <- "Unable To Define 'Days Out Of Window' - No Baseline Date Recorded"
            } else {
              #Define upper and lower limits of the expected days of out window based on the date of the event, 
              #date of baseline, and the expected date range pulled from metadata.
              #Requires "expected_days_after_baseline", minus_days, and "plus_days" from metadata file.
              temp_expected_event_date_upper_limit <- baseline_date + metadata$expected_days_after_baseline[j] + metadata$plus_days[j]
              temp_expected_event_date_lower_limit <- baseline_date + metadata$expected_days_after_baseline[j] - metadata$minus_days[j]
              #Define temp_event_date by pulling from dfIDevent.
              temp_event_date <- (dplyr::pull(dfIDevent[1,field_timepoint_date[j]]))
              
              #Define "days_after_baseline" for graph plotting.
              days_after_baseline <- temp_event_date - baseline_date
              if (temp_event_date > temp_expected_event_date_upper_limit) {
                days_out_of_window <- temp_event_date - temp_expected_event_date_upper_limit
                out_of_window_comment <- paste0("Event Out Of Window (Late) By ", days_out_of_window ," Days")
              } else {
                if (temp_event_date < temp_expected_event_date_lower_limit) {
                  days_out_of_window <- Mod(temp_event_date - temp_expected_event_date_lower_limit)
                  out_of_window_comment <- paste0("Event Out Of Window (Early) By ", days_out_of_window ," Days")
                } else {
                  #Define "days_out_of_window" as NA since is within the expected date range.
                  days_out_of_window <- NA
                  #Store comment regarding the reason "days_out_of_window" is "NA".
                  out_of_window_comment <- "Event Is Within Window"
                }
              }
            }
#
#              
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
            
            }
            #Close-If Statement: Timepoint / Event Date Check---------------------------CLOSE-IF--9
            
          }
          #Close-If: Timepoint / Event Recorded Check---------------------------------CLOSE-IF--3
        
        
        
        
        #Adds a column corresponding to the date of the baseline visit linked to this event.
        dfIDevent <- add_column(dfIDevent, baseline_date = baseline_date)
        #Adds a column corresponding to number of days after the baseline each event was attended.
        dfIDevent <- add_column(dfIDevent, days_after_baseline = days_after_baseline)
        #Transform the column's values to be numeric for future functions.
        dfIDevent <- transform(dfIDevent, days_after_baseline = as.numeric(days_after_baseline))
        #Adds a column to "dfIDevent" corresponding to the number of days that the event is overdue.
        dfIDevent <- add_column(dfIDevent, days_overdue = temp_days_overdue)
        #Transform the column's values to be numeric for future functions.
        dfIDevent <- transform(dfIDevent, days_overdue = as.numeric(days_overdue))
        #Adds a column with an additional comment based on the event date, baseline date, and whether the visit(s) occurred.
        dfIDevent <- add_column(dfIDevent, additional_comment = additional_comment)
        #Adds a column corresponding to the number of days that the event was out of window.
        dfIDevent <- add_column(dfIDevent, days_out_of_window = days_out_of_window)
        #Transform the column's values to be numeric for future functions.
        dfIDevent <- transform(dfIDevent, days_out_of_window = as.numeric(days_out_of_window))
        #Transform the site/DAG column to fill blank values with site based on unique ID.
        dfIDevent <- transform(dfIDevent, redcap_data_access_group = sites_by_unique_IDs[[yi,1]])
        #Adds a column with an additional comment based on the event date, baseline date, and whether the visit(s) was in window.
        dfIDevent <- add_column(dfIDevent, out_of_window_comment = out_of_window_comment)
        #Adds a column corresponding to the participant count for plotting purposes.
        dfIDevent <- add_column(dfIDevent, participant_count = participant_count[[yi,1]])
        #Transform the column's values to be numeric for future functions.
        dfIDevent <- transform(dfIDevent, participant_count = as.numeric(participant_count))
        
       #Bind together the rows from the pre-defined variable "all_timepoints" and the temporary data frame "dfIDevent"
       #for each event and each participant.
       all_timepoints <- dplyr::bind_rows(all_timepoints,dfIDevent)
       
       
       
      }
      #Close For Loop: By Timepoint / Event-----------------------------------------CLOSE-FOR-3
      
    }
    #Close For Loop: By Unique ID-------------------------------------------------CLOSE-FOR-2
    
  }
  #Close For Loop: By ID Type---------------------------------------------------CLOSE-FOR-1
  
  #Relocate the site/DAG for clarity.
  all_timepoints <- dplyr::relocate(all_timepoints, redcap_data_access_group)
  
  #EXPORT PREPARATION-------------------------------------------------------------------------------------------------------------------------
  
  
  #Filter out the empty row from when "all_timepoints" data frame was originally defined. 
  #(There is probably a way to have done this without orginally defining the empty row.)
  #Here it is filtered by removing any rows in which all possible record ID fields are NA.
  all_timepoints <- dplyr::filter(all_timepoints,
                                  if_all(
                                        all_of(record_ids),
                                        ~ !is.na(.x)
                                        )
                                  )
  
  #Define "dated_timepoints" for use in plotting, filtering only events that have an event date and a baseline date.
  dated_timepoints <- dplyr::filter(all_timepoints,
                                    !is.na(days_after_baseline)
                                    )
  
  #Remove the "participant_count" column, which is only required for plotting.
  all_timepoints <- dplyr::select(all_timepoints,
                                  !participant_count
                                  )
  
  #Define data frames to separate timepoints into those out of window, and those missed.
  #THIS line is currently redundant. oow_timepoints <- all_timepoints
  #I think this is also redundant. missing_timepoints <- all_timepoints
  
  #Set file name.
  file_name_1 <- paste0(study_name, "_CSM_Missing_And_Overdue_Timepoints_", Sys.Date(), ".csv")
  #Set file name.
  file_name_2 <- paste0(study_name, "_CSM_Missing_NOT_Overdue_Timepoints_", Sys.Date(), ".csv")
  
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For separate function: Days out of window.
#   
  #Set file name.
  file_name_3 <- paste0(study_name, "_CSM_Out_Of_Window_Timepoints_", Sys.Date(), ".csv")
#
#              
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#  
  
  #Filter to leave only the required rows in "missing_timepoints" prior to csv export.
  missing_timepoints <- dplyr::filter(all_timepoints, 
                                      
                                      #Filter could be based on "field_timepoint_yn" if only seeking missing events, regardless
                                      #of whether the date is provided. 
                                      #Here, it is disabled with a hash.
                                      #if_all(all_of(field_timepoint_yn), ~ is.na(.x))
                                      
                                      #Filter could be based on "field_timepoint_date" if seeking to include events that did
                                      #occur but did not include a date.
                                      if_all(all_of(field_timepoint_date), ~ is.na(.x)),
                                      
                                      #Filter out any rows for events corresponding to records that have no recorded Baseline Visit.
                                      #This would likely be participants who were not successfully screened.
                                      additional_comment != "No Baseline Event Recorded",
                                      
                                      #Filter out any rows for events corresponding to records that are not yet overdue.
                                      additional_comment != "Event Not Yet Overdue",
                                      
                                      #Filter out any rows for events corresponding to records that are not yet overdue.
                                      additional_comment != "Event Recorded - No Date Recorded"
                                      )
    
    #Optionally select columns to exclude all timepoint date fields. 
    #Here, it is disabled with a hash.
    #missing_timepoints <- dplyr::select(missing_timepoints, !all_of(field_timepoint_date))
  
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For separate function: Days out of window.
#   
  #Filter to select the required rows in dataframe "oow_timepoints" prior to csv export.
  oow_timepoints <- dplyr::setdiff(all_timepoints, missing_timepoints)
  oow_timepoints <- dplyr::filter(oow_timepoints,
                                  
                                  #Filter out any rows for events corresponding to records that have no recorded Baseline Visit.
                                  #This would likely be participants who were not successfully screened.
                                  additional_comment != "No Baseline Event Recorded",
                                  
                                  #Filter out any rows for events for which the visit was in window.
                                  out_of_window_comment != "Event Is Within Window"
                                  
                                  )
  
#
#              
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
  
##x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#EXCLUDED TIMEPOINTS IF DESIRED#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#excluded##
#x#x#x#x#x#  #x# #x# #x# #x# #x# 
# This section can be disabled with #s if excluded timepoints are not desired in the output.
# Alternatively, the output alone can be excluded by removing the "excluded_timepoints" and "in_window_timepoints" parameters from the write.xlsx output.
  #Set file name.
  file_name_4 <- paste0(study_name, "_CSM_In_Window_Timepoints_", Sys.Date(), ".csv") 
  file_name_5 <- paste0(study_name, "_CSM_Excluded_Timepoints_", Sys.Date(), ".csv")
  
  #Filter timepoints that were attended in window, if desired.
  in_window_timepoints <- dplyr::filter(all_timepoints, 
                                        out_of_window_comment == "Event Is Within Window"
  )
  
  #Sort "missing not overdue window timepoints" by "days_overdue"
  in_window_timepoints <- dplyr::arrange(in_window_timepoints, redcap_data_access_group)
  
  
  #Filter to place the excluded rows (in case this is useful) dataframe "excluded_timepoints" for csv export.
  excluded_timepoints <- dplyr::setdiff(all_timepoints, missing_timepoints)
  excluded_timepoints <- dplyr::setdiff(excluded_timepoints, oow_timepoints)
  excluded_timepoints <- dplyr::setdiff(excluded_timepoints, in_window_timepoints)
  
  #Sort "excluded timepoints" by "days_overdue"
  excluded_timepoints <- dplyr::arrange(excluded_timepoints, redcap_data_access_group)
  
  #Write data as CSV.
  write.csv (in_window_timepoints, file_name_4, row.names = FALSE)
  
  #Write data as CSV.
  write.csv (excluded_timepoints, file_name_5, row.names = FALSE)
  
#x#x#x#x#x#  #x# #x# #x# #x# #x#
##x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#EXCLUDED TIMEPOINTS IF DESIRED#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#excluded##
  
  
  #Select only columns which are relevant to "missing timepoints".
  missing_timepoints <- dplyr::select(missing_timepoints, !days_out_of_window)
  missing_timepoints <- dplyr::select(missing_timepoints, !out_of_window_comment)
  
  #Sort "missing_timepoints" by "days overdue"
  missing_timepoints <- sort_by(missing_timepoints, ~ days_overdue, decreasing = TRUE)
  
  #Sort "missing timepoints" by "days_overdue"
  missing_timepoints <- dplyr::arrange(missing_timepoints, redcap_data_access_group, desc(days_overdue))
  
  
  #Separate "missing_timepoints" into two separate data frames, one for those that are missing AND overdue, and one for those missing but NOT overdue.
  missing_and_overdue_timepoints <- dplyr::filter(missing_timepoints,
                                                  #Filter based on days overdue to keep only rows which are missing AND overdue.
                                                  days_overdue > 0
  )
  missing_not_overdue_timepoints <- dplyr::filter(missing_timepoints,
                                                 #Filter based on days overdue to keep only rows which are missing but NOT overdue.
                                                 days_overdue <= 0
  )
  
  #Clarifies when the days overdue is comparing from, to keep the exports up to date.
  colnames(missing_and_overdue_timepoints)[colnames(missing_and_overdue_timepoints) == 'days_overdue'] <- paste0("days_overdue_as_of_",Sys.Date()) 
  colnames(missing_not_overdue_timepoints)[colnames(missing_not_overdue_timepoints) == 'days_overdue'] <- paste0("days_until_overdue_as_of_",Sys.Date()) 
  
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For separate function: Days out of window.
# 
  #Select only columns which are relevant to "out of window timepoints".
  oow_timepoints <- dplyr::select(oow_timepoints, !days_overdue)
  oow_timepoints <- dplyr::select(oow_timepoints, !additional_comment)
  
  #Sort "out of window timepoints" by "days_out_of_window"
  oow_timepoints <- dplyr::arrange(oow_timepoints, redcap_data_access_group, desc(days_out_of_window))
#
#              
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow# 
  
  #EXPORT PREPARATION-----------------------------------------------------------SECTION 6-----------------------------------------------------
  
  #Set file name.
  file_name_final <- paste0(study_name, "_CSM_Missing_Overdue_And_OOW_Timepoints_", Sys.Date(), ".xlsx")
  
  
  #EXCLUDED RESULTS can be disabled with #s if "Missing NOT Overdue Timepoints", "In Window Timepoints", and "Excluded Timepoints" are not desired for output.
  
  #Define list of "datasets" for spreadsheet export.
  datasets <- list("Missing And Overdue" = missing_and_overdue_timepoints,
                   "Missing NOT Overdue" = missing_not_overdue_timepoints,
                   "Attended Out Of Window" = oow_timepoints,
                   "Attended In Window" = in_window_timepoints,
                   "Excluded" = excluded_timepoints
                   )
 
  #Define header style "hs" for spreadsheet export. 
  hs <- openxlsx::createStyle(
    textDecoration = "BOLD", 
    fontColour = "#000000", 
    fontSize = 12,
    fgFill = "#BBBBBB",
    border = "TopBottomLeftRight"
  )
  
  #Define list of tab colours to use for clarity.
  tabColourlist <- list("#ed8796","#f5a97f","#eed49f","#a6da95","#7dc4e4")
  #Alternative colouring.
#  tabColourlist <- rainbow(5, s = 0.63, v = 0.87, start = 0, end = 0.44)
  
  #Write data as .xlsx spreadsheet.
  openxlsx::write.xlsx(datasets,
                       file = file_name_final, 
                       borders = "all", 
                       colWidths = "auto",
                       headerStyle = hs,
                       tabColour = list("#ed8796","#f5a97f","#eed49f","#a6da95","#7dc4e4") ,
                       firstRow = TRUE
                       )#, keepNA = TRUE)
  
  
  #TESTING↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓TESTING

  #Create an empty workbook to fill with sheets for each data set for final export.
  export_workbook <- openxlsx::createWorkbook()
  
  #Define list of names to use for each of the workbook's sheets.
  nameslist <- list("Missing And Overdue",
                    "Missing NOT Overdue",
                    "Attended Out Of Window",
                    "Attended In Window",
                    "Excluded"
                    )
  
  for (wbi in 1:length(nameslist)) {
    
    #Add worksheets to fill with data sets.
    openxlsx::addWorksheet(export_workbook,
                           nameslist[wbi],
                           tabColour = tabColourlist[wbi]
                           )
    
    #Define header style "hs" for spreadsheet export. 
    hs <- openxlsx::createStyle(
      textDecoration = "BOLD", 
      fontColour = "#000000", 
      fontSize = 12,
      fgFill = tabColourlist[wbi],
      border = "TopBottomLeftRight"
      )
    
    #Write data into worksheets within worksheet to be exported.
    openxlsx::writeData(export_workbook, 
                        wbi, 
                        datasets[[wbi]], 
                        borders = "all",
                        headerStyle = hs
                        )
    
    #Freeze the first row of each sheet for ease of use.
    openxlsx::freezePane(export_workbook,
                         wbi,
                         firstActiveRow = NULL,
                         firstActiveCol = NULL,
                         firstRow = TRUE,
                         firstCol = FALSE
    )
    
    #Set column widths to "Auto" to show all text.
    openxlsx::setColWidths(export_workbook, 
                           wbi, 
                           cols = 1:ncol(datasets[[wbi]]),
                           widths = "auto"
                           )
    
    #FUTURE FIX
    #Retrieve column widths for further widening as previous function often underestimates required width.
    
    #Define vector to store site/DAG cell colours for conditional formatting.
    dag_colours <- rainbow(nrow(all_sites), s = 0.4, v = 1, start = 0.15, end = 0.95)
    
    
    #Conditional format by site/DAG
    for (dag in 1:nrow(all_sites)) {
      
      #Create style by site, rotating through rainbow colours twice and looping around for contrast.
      dag_style <- openxlsx::createStyle(bgFill = dag_colours[1+(2*dag) %% nrow(all_sites)])
      
      dag_rule <- paste0(as.character(all_sites[[dag,1]]))
      
      #Add conditional formatting to highlight different magnitudes of days overdue.
      openxlsx::conditionalFormatting(export_workbook,
                                      wbi, 
                                      rows = 2:(1+nrow(datasets[[wbi]])), 
                                      cols = 1,
                                      style = dag_style,
                                      rule = dag_rule,
                                      type = "contains"
                                      )
    }
  }
  
  #Conditional formatting set for sheets 1 to 3. This is not generalised and will need to be in the future if which sheets are included changes.
  for (wsi in 1:3) {
    
    #Add conditional formatting to highlight different magnitudes of days overdue.
    openxlsx::conditionalFormatting(export_workbook,
                                    wsi, 
                                    rows = 2:(1+nrow(datasets[[wsi]])), 
                                    cols = (ncol(datasets[[wsi]])-3),
                                    style = c("white", "red"),
                                    type = "colourScale"
    )
  }
  
    
  
  #Export data as .xlsx spreadsheet/workbook.
  openxlsx::saveWorkbook(export_workbook, file = "file_name_final.xlsx", overwrite = TRUE)

  #TESTING↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑TESTING
  
  
    #Write data as CSV.
    write.csv (missing_and_overdue_timepoints, file_name_1, row.names = FALSE)
    
    #Write data as CSV.
    write.csv (missing_not_overdue_timepoints, file_name_2, row.names = FALSE)
    
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
#            
#            #For separate function: Days out of window.
#   
  #Write data as CSV.
  write.csv (oow_timepoints, file_name_3, row.names = FALSE)
#
#              
# # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
  
  #PLOT-------------------------------------------------------------------------SECTION 7-----------------------------------------------------
  
  timepoint_plot <- function (metadata, dated_timepoints, shape_number_1, shape_number_2) {
  
#  dated_timepoints <- dplyr::
  xmin <- min(dated_timepoints["days_after_baseline"],0)
  xmax <- max(dated_timepoints["days_after_baseline"])
  ymin <- min(dated_timepoints["participant_count"],0)
  ymax <- max(dated_timepoints["participant_count"])
  
  timepoint_windows <- dplyr::select(metadata, timepoint_name, expected_days_after_baseline, minus_days, plus_days)
  timepoint_windows <- dplyr::mutate(timepoint_windows, timepoint_window_min = expected_days_after_baseline - minus_days)
  timepoint_windows <- dplyr::mutate(timepoint_windows, timepoint_window_max = expected_days_after_baseline + plus_days)
  timepoint_windows <- dplyr::select(timepoint_windows, timepoint_name, "timepoint_window_min", "timepoint_window_max")
  
  #Define X-axis breaks
  x_breaks <- seq(from = 50*(floor(xmin/50)-1), to = xmax + 50, by = 50)
  #Define Y-axis breaks
  y_breaks <- seq(from = ymin, to = ymax+5, by = 1)
  #Define colour mapping for events and timepoint windows.
#  timepoint_colours <- rainbow(nrow(metadata), s = 0.1, v = 1, start = 0.15, end = 0.95)
  
  ggplot(data = dated_timepoints, 
         aes(xmin = xmin, 
             xmax = xmax, 
             ymin = ymin, 
             ymax = ymax)
         ) +
    geom_rect(data = timepoint_windows, 
              aes(xmin = timepoint_window_min, 
                  xmax = timepoint_window_max, 
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = timepoint_name
                  ), 
              alpha = 0.2
              ) +
    # Plot data points
    geom_point(data = dated_timepoints, 
               aes(x = days_after_baseline, 
                   y = participant_count, 
                   colour = redcap_event_name,
                   shape = is.na(days_out_of_window)
                   ), 
               size = 3, 
               alpha = 0.8) +
    #Set X-axis breaks
    scale_x_continuous(breaks = x_breaks) +
    #Set Y-axis breaks
    scale_y_continuous(breaks = y_breaks) +
    #Set custom colours for event timepoints and timepoint windows
#    scale_color_manual(values = timepoint_colours, name = "Timepoint") +
    scale_color_viridis_d(aesthetics = "fill") +
    scale_color_viridis_d(aesthetics = "colour") +
    scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 4), guide = "none") +
    labs(
      title = "Follow-up Window Completion",
      x = "Days after Baseline Visit",
      y = "Participant Count"
    ) +
    theme_minimal()
  }
  
  timepoint_plot(metadata, dated_timepoints)
  
#  geom_point(data = dated_timepoints, aes(x = dated_timepoints["days_after_baseline"], y = dated_timepoints["participant_count"]))

  #  plot(x = dated_timepoints["days_after_baseline"], y = dated_timepoints["participant_count"])
  
}
