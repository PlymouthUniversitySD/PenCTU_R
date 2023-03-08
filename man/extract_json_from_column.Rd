% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_json_from_column.R
\name{extract_json_from_column}
\alias{extract_json_from_column}
\title{Extract json from data frame column}
\usage{
extract_json_from_column(data, col_name, prop_list = NULL)
}
\arguments{
\item{data}{The data frame which contains a column of json data}

\item{col_name}{The name of the column containing the json data e.g. "redcap_record_metadata"}

\item{prop_list}{A vector containing JSON properties that the user would like to extract from the json column (col_name) e.g. c("column_name_1", "another_column_name"), if not specified returns all extracted json properties.}
}
\value{
The original data frame with each JSON property becoming its own column and the original json column (col_nam) being removed
}
\description{
This function takes a data frame and extracts all json properties, from all JSON objects in a specified column, creates a new column for each property and populates the correct value of the property for each row. Also allows user to specify which JSON properties they are interested in - all others will be discarded.
}
\examples{
example1 <- extract_json_from_column(data = df, col_name = "JSON")
example2 <- extract_json_from_column(data = df, col_name = "JSON", prop_list = c("event.name", "event.status"))
}