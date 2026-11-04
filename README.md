# PenCTU R package

R functions used by the Peninsula Clinical Trials Unit (PenCTU) data team for REDCap data management.

Validation records, risk levels and current versions are in the Script Register.

## Installing and updating

Install the package from GitHub (you only need to install `remotes` once):

```r
install.packages("remotes")
remotes::install_github("PlymouthUniversitySD/PenCTU_R")
```

To update to the latest version, run the same `install_github()` line again and restart R.

To check which version you have:

```r
packageVersion("PenCTU")
```

Load it at the top of a script with `library(PenCTU)`.

## Storing API tokens

Never type an API token into a script or the console. Store each token once on your computer using the Windows Credential Manager, through the `keyring` package:

```r
install.packages("keyring")                 # once
keyring::key_set("STUDY_PURPOSE_TOKEN")     # a box opens: paste the token and click OK
```

Then read it in your script:

```r
token <- keyring::key_get("STUDY_PURPOSE_TOKEN")
```

- **Names:** use the study and what the token is for, for example `SAFE2_EXPORTS_TOKEN`. Use the exact name the script expects.
- **See which tokens are stored:** `keyring::key_list()` shows the names only, never the tokens.
- **Remove a token:** `keyring::key_delete("STUDY_PURPOSE_TOKEN")`.
- **New computer or Windows login:** tokens are stored per computer and login, so run `key_set()` again.
- **If a token may have been exposed** (for example typed into a script or shared), regenerate it in REDCap on the project's API page.

## Functions

| Function | What it does |
|---|---|
| `atr_unhelpful_reasons()` | Gives the list of reasons for change that the audit trail review flags as unhelpful, with any the study adds. |
| `audit_trail_review()` | Summarises staff changes in a REDCap audit trail by site, user, field and time, and saves the audit trail review workbook. |
| `build_nc_log()` | Finds non-compliances that have not been logged yet, writes them to an NC log file and flags them in REDCap. |
| `build_rfc_script()` | Makes a study's copy of the reason-for-change JavaScript, with the study's fields that never need a reason. |
| `completeness_thresholds()` | Sets the green, amber and red thresholds used in the data completeness report. |
| `completeness_tracker_data_prep()` | Exports a study's form statuses from REDCap and counts complete, partially complete and incomplete forms by event and site. |
| `get_audit_trail()` | Downloads a REDCap project's log and splits it into one row per field change, with the old and new values and that field's reason. |
| `get_redcap_exports()` | Exports a study's REDCap reports and saves each one as a CSV file, with an export log. |
| `missing_overdue_and_oow_timepoints()` | Shows, for each participant and timepoint, whether it was attended in or out of its window, is missing, did not take place or is no longer expected. |
| `rfc_exempt_fields()` | Lists the fields in a data dictionary that never need a reason for change. |
| `write_excel_completeness()` | Saves the data completeness report as an Excel workbook, with percentages coloured green, amber or red. |

### audit_trail_review()

Takes the audit trail from `get_audit_trail()` (one or more projects) and the data dictionary, and summarises staff edits on forms where a value changed. Data management, system and trial management fields and form status fields are left out. Each record's site comes from its DAG. Saves one Excel workbook, outside any GitHub repository, with a cover sheet for the review's findings and these sheets: Site Summary, Critical Data Items, Changes per User (by month), Unstable Fields, Time to Change, Out of Hours Changes, Reasons for Change and Deletions. Working hours, the time-to-change threshold and extra unhelpful reasons can be set by the study. Used by `TEMPLATE_ATR`.

### build_nc_log()

Works through a study's non-compliance (NC) presets, one row per NC rule, and finds the records where an NC has been triggered but not yet logged. It writes these to a date-stamped NC log file, then flags each record in REDCap so it isn't picked up again. Set `upload = FALSE` for a dry run that creates the log without saving the file or flagging any records. Used by `TEMPLATE_AutomatedNonCompliances`.

### completeness_thresholds()

Returns the thresholds for the data completeness report's green, amber and red ratings. The defaults are 85% or more complete for green and 75% to under 85% for amber, and 15% or less partially complete or incomplete for green and over 15% to 25% for amber. A study can set its own, for example `completeness_thresholds(complete_green = 0.9)`. Used by `write_excel_completeness()` and `TEMPLATE_DataCompleteness`.

### completeness_tracker_data_prep()

Exports a study's data and form statuses from REDCap (live or test) and counts, for each form at each event and site, how many are complete, partially complete or incomplete. Every instance of a repeating form or event is counted. Sites come from the DAGs; records with no DAG are shown as "Unknown", and a project with no DAGs as "All participants". You can choose events, leave out forms, count some forms only where they have been started (such as an adverse event log), and use the timepoints file to leave out timepoints that did not take place. Used with `write_excel_completeness()` by `TEMPLATE_DataCompleteness`.

### get_audit_trail()

Downloads the whole log of a REDCap project through the API. Each data entry is split into one row per field, with the value before and after the change, who made it, when, and the reason given. Where the reason was given for each field (by PenCTU_ReasonForChange.js), each field's row has its own reason; REDCap's original reason is kept in `log_reason`. Entries that aren't data changes, such as exports and design changes, are kept as they are. A CSV is saved only if you ask for one, and never inside a GitHub repository. Used by `audit_trail_review()` and `TEMPLATE_ATR`.

### get_redcap_exports()

Exports each REDCap report listed in a study's export metadata file and saves it as a CSV file, exactly as REDCap provides it, ready to send to the statistics team. It also saves an export log. Files are saved outside any GitHub repository. Used by `TEMPLATE_GetExports`.

### missing_overdue_and_oow_timepoints()

Works out each participant's visit windows from their baseline date and the study's timepoints file, and shows whether each timepoint was attended in or out of its window, is missing (overdue or not yet overdue), did not take place, or cannot be assessed. An optional stop rules file (for example full withdrawal or death) marks timepoints due after the stop date as "Not expected". Days overdue are counted from a date you can set (`as_of`, default today). Saves an Excel workbook with a summary by site, one sheet per category and the follow-up window plot, outside any GitHub repository. Calls written for version 1 (data and metadata only) still work. Used by `TEMPLATE_TimepointMonitoring`.

### rfc_exempt_fields()

Reads a study's data dictionary (the downloaded CSV or the API export) and lists the fields that never need a reason for change: calculated fields, @CALCTEXT and @CALCDATE fields, and fields annotated #DataManagement, #TrialManagement or #SystemFunctionality. Used by `build_rfc_script()`.

### write_excel_completeness()

Saves the counts from `completeness_tracker_data_prep()` as one Excel workbook: a cover sheet with the versions, export time and settings, then one sheet per event. Percentages are coloured green, amber or red using `completeness_thresholds()`. Saved outside any GitHub repository. Used by `TEMPLATE_DataCompleteness`.