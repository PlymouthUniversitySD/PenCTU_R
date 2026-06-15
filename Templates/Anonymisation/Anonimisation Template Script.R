#Step 1: Remove any participants that ar enot required, for example due to rescreening.

result <- remove_participants_meeting_logic(
  data = erase_data,
  participant_id_field = "record_id",
  exclusion_logic = horescreen___1 == "1")

dataset <- result[["data"]]

#Step 2: Run the risk report to identify which quasi-identifiers appear in low numbers. Use this to complete the annotated data dictionary.

risk <- analyse_quasi_identifier_risk(
  dataset = dataset,
  data_dictionary = erase_data_dictionary,
  participant_id_field = "record_id")

#Step 3: Run the anonymisation with the updated anotated data dictionary


