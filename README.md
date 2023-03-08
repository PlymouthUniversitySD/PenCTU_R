# PENCTU R

## extract_json_from_column

### Description
This function takes a data frame and extracts all json properties, from all JSON objects in a specified column, creates a new column for each property and populates the correct value of the property for each row. Also allows user to specify which JSON properties they are interested in - all others will be discarded.

### Usage
```
extract_json_from_column(data, col_name, prop_list)
```

### Arguments
-   `data`: df - The data frame which contains a column of json data

-   `col_name`: chr - The name of the column containing the json data e.g. "redcap_record_metadata" 

-   `prop_list`: _Optional_ c(chr) -  A vector containing JSON properties that the user would like to extract from the json column (col_name) e.g. c("column_name_1", "another_column_name"), if not specified returns all extracted json properties. 

### Examples
Suppose you have a dataframe (df) with 4 columns (ID: int, Value: chr, Description: chr, JSON: chr). The JSON column contains a JSON object for each ID with the following structure: 
```
{
    event: {
        name: chr, 
        status: int, 
        eventDate: chr
    },
    item: {
        item1: int
    }
}

```
To extract all json properties from this column use: 

```
extract_json_from_column(data = df, col_name = "JSON")
```

this will return a new dataframe containing the following columns:
(ID, Value, Description, event.name, event.status, event.eventDate, item.item1)

To view the column names in the returned data frame use: 

```
result <- extract_json_from_column(data = df, col_name = "JSON")
names(result)
```

The user may only be interested in the event name and status. In this case, use: 

```
extract_json_from_column(data = df, col_name = "JSON", prop_list = c("event.name", "event.status"))
```
this will return a new dataframe containing the following columns:
(ID, Value, Description, event.name, event.status).