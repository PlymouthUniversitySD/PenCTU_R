#Title: Central Statistical Monitoring - Missing, Overdue, and Out-Of-Window Timepoints Data Function
#Author: Eduard Mazuru
#Version & Date: V1.5.0 20AUG2026
#R version: 4.4.3
#' Exports '.xlsx' excel workbook with sheets corresponding to study-wide required event timepoints that are:
#' missing - and the days overdue, out-of-window, in-window, or excluded.
#'
#' This function lists timepoints for required events and the number of days overdue or out of window,
#' using expected field names, timepoint names and date window definitions from a metadata csv.
#' The function separates the list by category into different sheets on an excel workbook.
#' Function also exports .CSV files for out-of-window timepoints and missing timepoints.
#' The follow-up window completion plot is saved and inserted as its own "Plot" sheet in the
#' exported workbook. Each participant is placed in a compact block for their own site (sized to that
#' site's own participant count, so sites of very different sizes don't waste space), and the y-axis
#' is labelled with just each site's short prefix (e.g. "01", "02"), matching the site/DAG already
#' used throughout the rest of the workbook.
#'
#' @param data A REDCap export dataset.
#' @param metadata A critical data item metadata dataframe.
#' @param id_number_delimiter Character(s) the participant ID is split on to isolate the participant
#'   number, e.g. "-" for an ID like "01-001". Defaults to "-". Used only to sort participants sensibly
#'   within their own site on the plot - never shown on the plot itself.
#' @param id_number_position Which piece of the split ID holds the participant number: "last" (default),
#'   "first", or a numeric index (as a string, e.g. "2") for a specific piece.
#'
#' @return An excel workbook containing timepoints, the days out of window, days overdue, and orders by participant ID and event name.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr "filter"
#' @importFrom dplyr "select"
#' @importFrom dplyr "setdiff"
#' @importFrom dplyr "arrange"
#' @importFrom dplyr "bind_rows"
#' @importFrom dplyr "pull"
#' @importFrom dplyr "relocate"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyselect "last_col"
#' @importFrom tidyselect "everything"
#' @importFrom tidyr "pivot_wider"
#' @importFrom tidyr "pivot_longer"
#' @importFrom ggplot2 "ggsave"
#' @importFrom openxlsx "createStyle"
#' @importFrom openxlsx "createWorkbook"
#' @importFrom openxlsx "addWorksheet"
#' @importFrom openxlsx "writeData"
#' @importFrom openxlsx "freezePane"
#' @importFrom openxlsx "setColWidths"
#' @importFrom openxlsx "conditionalFormatting"
#' @importFrom openxlsx "insertImage"
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
missing_overdue_and_oow_timepoints <- function(data, metadata, by_site = FALSE, include_split_CSVs = FALSE, simplify_ID = FALSE, simplify_event_date = FALSE, id_number_delimiter = "-", id_number_position = "last") {
  
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
        
        
        #COLUMN ADDITIONS AND TRANSFORMATIONS-----------------------------------Section 5b----------------------------------------------------
        
        #Stores the current participant id field into a new variable to later simplify the output.
        temp_participant_id <- (dplyr::pull(data_unique_IDs[yi,xi]))
        
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
        #Adds a column corresponding to the participant ID in order to more cleanly display data.
        dfIDevent <- add_column(dfIDevent, participant_identifier_by_event = temp_participant_id)
        #        #Adds a column corresponding to the event date in order to optionally simplify columns down later.
        #        dfIDevent <- add_column(dfIDevent, temp_event_date = temp_event_date)
        
        #Bind together the rows from the pre-defined variable "all_timepoints" and the temporary data frame "dfIDevent"
        #for each event and each participant.
        all_timepoints <- dplyr::bind_rows(all_timepoints,dfIDevent)
        
        
        
      }
      #Close For Loop: By Timepoint / Event-----------------------------------------CLOSE-FOR-3
      
    }
    #Close For Loop: By Unique ID-------------------------------------------------CLOSE-FOR-2
    
  }
  #Close For Loop: By ID Type---------------------------------------------------CLOSE-FOR-1
  
  
  #  #Relocate the event date for clarity.
  #  all_timepoints <- dplyr::relocate(all_timepoints, temp_event_date)
  #Relocate "redcap_event_name" for clarity.
  all_timepoints <- dplyr::relocate(all_timepoints, redcap_event_name)
  #Relocate the participant ID for clarity.
  all_timepoints <- dplyr::relocate(all_timepoints, participant_identifier_by_event)
  #Relocate the site/DAG for clarity.
  all_timepoints <- dplyr::relocate(all_timepoints, redcap_data_access_group)
  
  #EXPORT PREPARATION-----------------------------------------------------------SECTION 6a-----------------------------------------------------
  
  
  #Filter out the empty row from when "all_timepoints" data frame was originally defined.
  #(There is probably a way to have done this without orginally defining the empty row.)
  #Here it is filtered by removing any rows in which all possible record ID fields are NA.
  all_timepoints <- dplyr::filter(all_timepoints,
                                  if_all(
                                    all_of(record_ids),
                                    ~ !is.na(.x)
                                  )
  )
  
  #FOLLOW-UP WINDOW PLOT DATA----------------------------------------------------SECTION 6b-----------------------------------------------------
  
  #Build a simple one-row-per-participant table for the plot: one column per timepoint, holding "days
  #after baseline" for that timepoint. This is deliberately separate from (and much simpler than) the
  #detailed missing/overdue/out-of-window logic above - it reuses the same "occurred"/"date" field
  #names already pulled from "metadata" in Section 3, just pivoted into a wide, easy-to-plot shape,
  #the same way as: test <- subset(data, is.na(redcap_repeat_instance) & <event> & <occurred>==1);
  #pivot_wider(test, id_cols = record_id, names_from = redcap_event_name, values_from = <date field>)
  
  #Map each event name to its own "date" field (from metadata), so each row's date is pulled from the
  #right column even when different timepoints use different field names.
  event_date_field_lookup <- data.frame(
    redcap_event_name = c(baselines, events),
    date_field = c(field_baseline_date, field_timepoint_date),
    stringsAsFactors = FALSE
  )
  event_date_field_lookup <- dplyr::distinct(event_date_field_lookup)
  
  #"df" (defined in Section 4) is already filtered to required events, non-repeating rows, and
  #occurred == 1 - exactly what's needed here too, so it's reused as-is rather than re-filtering data.
  plot_source <- df
  plot_source$date_field <- event_date_field_lookup$date_field[
    match(plot_source$redcap_event_name, event_date_field_lookup$redcap_event_name)
  ]
  #Pull each row's date from whichever field the lookup above says holds it - looped over the handful
  #of distinct date field names (not over every row, which would be much slower).
  plot_source$event_date <- as.Date(NA)
  for (date_field_name in unique(stats::na.omit(plot_source$date_field))) {
    field_rows <- which(plot_source$date_field == date_field_name)
    plot_source$event_date[field_rows] <- as.Date(plot_source[[date_field_name]][field_rows])
  }
  
  #Pivot to one row per participant, one column per event (values_fn keeps just the first date if a
  #participant somehow has more than one row for the same event).
  plot_wide <- dplyr::select(plot_source, dplyr::all_of(record_ids[1]), redcap_event_name, event_date)
  plot_wide <- tidyr::pivot_wider(plot_wide,
                                  id_cols = dplyr::all_of(record_ids[1]),
                                  names_from = redcap_event_name,
                                  values_from = event_date,
                                  values_fn = ~ .x[1]
  )
  
  #Compute "days after baseline" for each timepoint, using that timepoint's own associated baseline
  #(matching "events" and "baselines" up by position, same pairing the detailed loop above uses).
  for (j in seq_along(events)) {
    days_col <- paste0(events[j], "_days")
    if (all(c(as.character(events[j]), as.character(baselines[j])) %in% names(plot_wide))) {
      plot_wide[[days_col]] <- as.numeric(plot_wide[[events[j]]] - plot_wide[[baselines[j]]])
    } else {
      plot_wide[[days_col]] <- NA_real_
    }
  }
  
  #Attach each participant's site, matching the same site/DAG assignment used for the rest of the
  #workbook (rather than re-parsing a site prefix out of the ID text).
  plot_table <- dplyr::left_join(plot_wide, sites_by_unique_IDs, by = record_ids[1])
  
  #Parse a sortable participant number from the ID (letters ignored, e.g. "01-A014B" -> 14) purely to
  #order participants sensibly within their own site - this number is never shown on the plot itself.
  extract_participant_number <- function(id, delimiter = "-", position = "last") {
    id <- as.character(id)
    segments <- strsplit(id, delimiter, fixed = TRUE)[[1]]
    if (length(segments) == 0) {
      return(NA_real_)
    }
    segment <- if (position == "last") {
      segments[length(segments)]
    } else if (position == "first") {
      segments[1]
    } else {
      segments[as.numeric(position)]
    }
    if (is.na(segment)) {
      return(NA_real_)
    }
    digits_only <- gsub("[^0-9]", "", segment)
    if (digits_only == "") {
      return(NA_real_)
    }
    as.numeric(digits_only)
  }
  plot_table <- dplyr::mutate(plot_table,
                              participant_sort_number = sapply(.data[[record_ids[1]]],
                                                               extract_participant_number,
                                                               delimiter = id_number_delimiter,
                                                               position = id_number_position
                              )
  )
  
  #Compact per-site y-position: each participant's rank within their own site, stacked onto a running
  #offset so every site gets its own block sized to its OWN participant count (plus a small gap) - not
  #a fixed allowance per site, which would waste a lot of vertical space once sites vary a lot in size.
  plot_positions <- dplyr::arrange(plot_table, redcap_data_access_group, participant_sort_number)
  plot_positions <- dplyr::group_by(plot_positions, redcap_data_access_group)
  plot_positions <- dplyr::mutate(plot_positions, rank_in_site = dplyr::row_number())
  plot_positions <- dplyr::ungroup(plot_positions)
  
  site_blocks <- dplyr::group_by(plot_positions, redcap_data_access_group)
  site_blocks <- dplyr::summarise(site_blocks, site_n = dplyr::n())
  site_blocks <- dplyr::arrange(site_blocks, redcap_data_access_group)
  site_blocks <- dplyr::mutate(site_blocks, block_start = dplyr::lag(cumsum(site_n + 2), default = 0))
  site_blocks <- dplyr::select(site_blocks, redcap_data_access_group, block_start)
  
  plot_positions <- dplyr::left_join(plot_positions, site_blocks, by = "redcap_data_access_group")
  plot_positions <- dplyr::mutate(plot_positions, plot_position = block_start + rank_in_site)
  plot_table <- dplyr::left_join(plot_table,
                                 dplyr::select(plot_positions, dplyr::all_of(record_ids[1]), plot_position),
                                 by = record_ids[1]
  )
  
  #One y-axis label per site, positioned at that site's own block midpoint. DAG values are often
  #formatted like "01 - Risley" - only the leading token (before any space/hyphen) is used so the axis
  #stays short, e.g. "01".
  label_rows <- dplyr::group_by(plot_positions, redcap_data_access_group)
  label_rows <- dplyr::summarise(label_rows, label_position = mean(range(plot_position)))
  label_rows <- dplyr::mutate(label_rows, site_label = sub("^([^\\s-]+).*$", "\\1", trimws(as.character(redcap_data_access_group))))
  label_rows <- dplyr::arrange(label_rows, label_position)
  
  #EXPORT PLOT------------------------------------------------------------------SECTION 6c-----------------------------------------------------
  
  timepoint_plot <- function(plot_table, metadata) {
    
    days_cols <- paste0(events, "_days")
    
    plot_long <- tidyr::pivot_longer(plot_table,
                                     cols = dplyr::all_of(days_cols),
                                     names_to = "timepoint_name",
                                     values_to = "days_after_baseline"
    )
    plot_long <- dplyr::mutate(plot_long, timepoint_name = sub("_days$", "", timepoint_name))
    plot_long <- dplyr::mutate(plot_long, timepoint_name = factor(timepoint_name, levels = as.character(events)))
    plot_long <- dplyr::filter(plot_long, !is.na(days_after_baseline))
    
    xmin <- suppressWarnings(min(plot_long$days_after_baseline, 0, na.rm = TRUE))
    xmax <- suppressWarnings(max(plot_long$days_after_baseline, na.rm = TRUE))
    x_breaks <- seq(from = 50 * (floor(xmin / 50) - 1), to = xmax + 50, by = 50)
    
    timepoint_windows <- dplyr::select(metadata, timepoint_name, expected_days_after_baseline, minus_days, plus_days)
    timepoint_windows <- dplyr::mutate(timepoint_windows,
                                       timepoint_window_min = expected_days_after_baseline - minus_days,
                                       timepoint_window_max = expected_days_after_baseline + plus_days,
                                       timepoint_name = factor(as.character(timepoint_name), levels = levels(plot_long$timepoint_name))
    )
    
    #Define a distinct, easily distinguishable colour per timepoint (rather than a continuous viridis
    #gradient), so timepoints are easy to tell apart at a glance. Recycled via colorRampPalette if
    #there are more timepoints than defined colours.
    base_event_colours <- c("#8aadf4", "#ed8796", "#a6da95", "#eed49f", "#c6a0f6", "#f5a97f", "#91d7e3")
    if (length(events) > length(base_event_colours)) {
      base_event_colours <- grDevices::colorRampPalette(base_event_colours)(length(events))
    }
    event_colours <- setNames(base_event_colours[seq_along(events)], as.character(events))
    
    p <- ggplot(data = plot_long, aes(x = days_after_baseline, y = plot_position)) +
      #Shaded window band, spanning the full height of each panel. ymin/ymax are fixed at -Inf/Inf
      #(rather than a computed min/max) specifically so this works correctly per-facet: a computed
      #min/max would be the same constant on every panel, forcing every facet to expand its own
      #y-range out to the full study-wide range instead of just that site's own participants. -Inf/Inf
      #always fills whatever range the panel actually ends up with.
      geom_rect(data = timepoint_windows,
                aes(xmin = timepoint_window_min, xmax = timepoint_window_max, fill = timepoint_name),
                ymin = -Inf, ymax = Inf, alpha = 0.1, inherit.aes = FALSE
      ) +
      #Dashed line marking each timepoint's expected/target day within its window.
      geom_vline(data = timepoint_windows,
                 aes(xintercept = expected_days_after_baseline),
                 linetype = "dashed", colour = "grey30", linewidth = 0.4
      ) +
      geom_point(aes(colour = timepoint_name), size = 2, alpha = 0.8) +
      scale_x_continuous(breaks = x_breaks) +
      #Label the Y-axis with one tick per site (e.g. "01", "02"...). Even in facet mode, each panel
      #now only ever shows the labels that actually fall inside its own (correctly free-scaled) range,
      #since the geom_rect fix above stopped every panel's range from being inflated to the full
      #study-wide extent.
      scale_y_continuous(breaks = label_rows$label_position, labels = label_rows$site_label) +
      #Set distinct, easily distinguishable colours for event timepoints and timepoint windows.
      scale_fill_manual(values = event_colours, name = "Timepoint") +
      scale_colour_manual(values = event_colours, name = "Timepoint") +
      labs(
        title = "Follow-up Window Completion",
        x = "Days after Baseline Visit",
        y = "Site"
      ) +
      theme_minimal()
    
    if (by_site == TRUE) {
      p <- p + facet_grid(redcap_data_access_group ~ ., scales = "free_y")
    }
    
    p
    
  }
  
  #Store the plot object (rather than just calling it) so it can be saved and inserted into the
  #exported workbook further down. A bare function call mid-function does not print or save a ggplot
  #object.
  csm_plot <- timepoint_plot(plot_table, metadata)
  
  if (simplify_ID == TRUE) {
    #Remove each unique "record_id" column, leaving only one generic "participant_identifier" column.
    all_timepoints <- dplyr::select(all_timepoints,
                                    !any_of(record_ids)
    )
    #Simplify "participant_identifier" column name if there is only one ID type.
    if (length(record_ids) == 1) {
      colnames(all_timepoints)[colnames(all_timepoints) == 'participant_identifier_by_event'] <- "participant_id"
    }
  } else {
    #Remove the "participant_identifier_by_event" column, which is only required for plotting.
    all_timepoints <- dplyr::select(all_timepoints,
                                    !participant_identifier_by_event
    )
  }
  
  #  if (simplify_event_date == TRUE) {
  #    #Remove each unique "event date" column, leaving only one generic "event_date" column.
  #    all_timepoints <- dplyr::select(all_timepoints,
  #                                    !any_of(field_timepoint_date)
  #    )
  #    #Simplify "temp_event_date" column name.
  #    if (length(record_ids) == 1) {
  #      colnames(all_timepoints)[colnames(all_timepoints) == 'temp_event_date'] <- "event_date"
  #    }
  #  } else {
  #    #Remove the "temp_event_date" column.
  #    all_timepoints <- dplyr::select(all_timepoints,
  #                                    !temp_event_date
  #    )
  #  }
  
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
  
  #If statement checks if include_split_CSVs has been set to TRUE. If TRUE, writes CSV files for each separate output sheet.
  if (include_split_CSVs == TRUE) {
    #Write data as CSV.
    write.csv (in_window_timepoints, file_name_4, row.names = FALSE)
    
    #Write data as CSV.
    write.csv (excluded_timepoints, file_name_5, row.names = FALSE)
  }
  
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
  
  #EXPORT XLSX PREPARATION------------------------------------------------------SECTION 6d----------------------------------------------------
  
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
  
  
  #This .xlsx spreadsheet write is redundant and has been replaced by the code below it.
  #Write data as .xlsx spreadsheet.
  #  openxlsx::write.xlsx(datasets,
  #                       file = file_name_final,
  #                       borders = "all",
  #                       colWidths = "auto",
  #                       headerStyle = hs,
  #                       tabColour = list("#ed8796","#f5a97f","#eed49f","#a6da95","#7dc4e4") ,
  #                       firstRow = TRUE
  #                       )#, keepNA = TRUE)
  #
  
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
  
  
  #ADD FOLLOW-UP WINDOW PLOT AS ITS OWN SHEET------------------------------------------------------------------------------------------------
  
  #Add a worksheet to hold the follow-up window completion plot, matching the style of the other tabs.
  openxlsx::addWorksheet(export_workbook, "Plot", tabColour = "#8aadf4")
  
  #Set a plot image file name, following the same naming convention as the other exports.
  plot_file_name <- paste0(study_name, "_CSM_Followup_Window_Plot_", Sys.Date(), ".png")
  
  #Height scales with how many participants (or how wide a numeric range) need to be shown vertically,
  #rather than a fixed constant - a fixed height works fine for a small pilot study but crushes every
  #participant's dots into what looks like a single line once a study has more than a handful of people.
  #~0.12in per unit of participant-number range keeps adjacent participants' dots visibly separated at
  #150dpi; a 7in floor and 50in ceiling keep small studies sensibly proportioned and huge ones exportable
  #(a very large study may still need the ceiling raised further, or geom_point's size reduced, by hand).
  INCHES_PER_PARTICIPANT_UNIT <- 0.12
  
  if (by_site == TRUE) {
    #Facet panels are equal height by default, so the height needed is driven by whichever single site
    #has the most participants, multiplied by the number of site panels.
    site_ranges <- dplyr::group_by(plot_positions, redcap_data_access_group)
    site_ranges <- dplyr::summarise(site_ranges, site_range = suppressWarnings(max(rank_in_site, na.rm = TRUE)))
    site_ranges <- dplyr::mutate(site_ranges, site_range = ifelse(is.infinite(site_range), 1, site_range))
    widest_site_range <- max(site_ranges$site_range, 1)
    plot_height_in <- min(50, max(6, nrow(all_sites) * max(2, widest_site_range * INCHES_PER_PARTICIPANT_UNIT)))
  } else {
    combined_range <- suppressWarnings(max(plot_positions$plot_position, na.rm = TRUE))
    if (!is.finite(combined_range) || combined_range <= 0) {
      combined_range <- 1
    }
    plot_height_in <- min(50, max(7, combined_range * INCHES_PER_PARTICIPANT_UNIT))
  }
  
  #Save the plot to file so it can be inserted into the workbook (a bare ggplot object can't be
  #inserted directly - openxlsx::insertImage needs an actual image file on disk).
  ggsave(
    filename = plot_file_name,
    plot = csm_plot,
    width = 12,
    height = plot_height_in,
    dpi = 150,
    limitsize = FALSE
  )
  
  #Insert the saved plot image into the "Plot" worksheet.
  openxlsx::insertImage(
    export_workbook,
    "Plot",
    plot_file_name,
    width = 12,
    height = plot_height_in,
    units = "in",
    dpi = 150
  )
  
  
  
  #Export data as .xlsx spreadsheet/workbook.
  openxlsx::saveWorkbook(export_workbook, file = file_name_final, overwrite = TRUE)
  #TESTING↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑TESTING
  
  #If statement checks if include_split_CSVs has been set to TRUE. If TRUE, writes CSV files for each separate output sheet.
  if (include_split_CSVs == TRUE) {
    #Write data as CSV.
    write.csv (missing_and_overdue_timepoints, file_name_1, row.names = FALSE)
    
    #Write data as CSV.
    write.csv (missing_not_overdue_timepoints, file_name_2, row.names = FALSE)
  }
  
  # # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
  #
  #            #For separate function: Days out of window.
  #
  #If statement checks if include_split_CSVs has been set to TRUE. If TRUE, writes CSV files for each separate output sheet.
  if (include_split_CSVs == TRUE) {
    #Write data as CSV.
    write.csv (oow_timepoints, file_name_3, row.names = FALSE)
  }
  #
  #
  # # # # # # # # # # # ############################################EXCLUSIVELY FOR OOW FUNCTION###############################oow#oow#oow#
  
  
}
