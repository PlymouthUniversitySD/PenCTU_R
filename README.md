# PenCTU R package

R functions used by the Peninsula Clinical Trials Unit (PenCTU) data team for REDCap data management.

Validation records, risk levels and current versions are in the Script Register. 

## Installing and updating

Install the package from GitHub (you only need to install `remotes` once):

```r
install.packages("remotes")
remotes::install_github("<organisation>/PenCTU_R")
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
| `build_nc_log()` | Finds non-compliances that have not been logged yet, writes them to an NC log file and flags them in REDCap. |
| `get_audit_trail()` | Downloads a REDCap project's log and splits it into one row per field change, with the old and new values. |
| `get_redcap_exports()` | Exports a study's REDCap reports and saves each one as a CSV file, with an export log. |

### build_nc_log()

Works through a study's non-compliance (NC) presets, one row per NC rule, and finds the records where an NC has been triggered but not yet logged. It writes these to a date-stamped NC log file, then flags each record in REDCap so it isn't picked up again. Set `upload = FALSE` for a dry run that creates the log without saving the file or flagging any records. Used by `TEMPLATE_AutomatedNonCompliances`.

Specification: [specifications/build_nc_log.md](specifications/build_nc_log.md)

### get_audit_trail()

Downloads the whole log of a REDCap project through the API. Each data entry is split into one row per field, with the value before and after the change, who made it, when, and the reason given. Entries that aren't data changes, such as exports and design changes, are kept as they are. A CSV is saved only if you ask for one, and never inside a GitHub repository. Used by the audit trail review (ATR) functions and `TEMPLATE_ATR`.

Specification: [specifications/get_audit_trail.md](specifications/get_audit_trail.md)

### get_redcap_exports()

Exports each REDCap report listed in a study's export metadata file and saves it as a CSV file, exactly as REDCap provides it, ready to send to the statistics team. It also saves an export log. Files are saved outside any GitHub repository. Used by `TEMPLATE_GetExports`.

Specification: [specifications/get_redcap_exports.md](specifications/get_redcap_exports.md)