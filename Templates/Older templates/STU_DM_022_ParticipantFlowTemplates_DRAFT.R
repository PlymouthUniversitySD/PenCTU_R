#\select the most appropriate participant flow template based on the structure of recruitment in your study.
#\note that you may still need to make alterations to the template as these diagrams are highly specific
#\to each study

#\Template 1: randomised trial with one round of eligibility, where baseline does not need to be 
#\completed for randomisation to occur and where participants can be excluded at randomisation

#\isolate recruitment data
participant_flow_data <- subset(labelled_dataset, redcap_event_name == 'Registration') #\replace registration with the registration event name
participant_flow_data <- select(participant_flow_data, record_id, Site, referred, consent, consent_exclude,
                                consent_exclusionreason, eligibility1, eligibility1_exclude, eligibility1_exclusionreason,
                                pdexcludeyn, pdexcludereason, randomise, randomise_exclude, randomise_excludereason)

#Add register field
participant_flow_data$Registered <- ifelse(!is.na(participant_flow_data$record_id), 'Yes', 'No')

#Add awaiting fields
participant_flow_data$awaiting_eligibility1 <- ifelse((participant_flow_data$Registered=='Yes' & participant_flow_data$eligibility1=='No' & participant_flow_data$eligibility1_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_consent <- ifelse((participant_flow_data$eligibility1=='Yes' & participant_flow_data$consent=='No' & participant_flow_data$consent_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_randomisation <- ifelse((participant_flow_data$baseline=='Yes' & participant_flow_data$randomise=='No' & participant_flow_data$randomise_exclude=='No'),'Yes','No')

#Define values for participant flow diagram
study_cohorts <- 
  participant_flow_data %>%
  cohort_start("Registered") %>%
  # Define cohorts using named expressions --------------------
cohort_define(
  <SITE1> = .full %>% filter(Site=='<SITE1>'), #\replace <SITE1> with site name
  <SITE2>  = .full %>% filter(Site=='<SITE2>'), #\replace <SITE2> with site name. add as many repeats of this row are required
  pdexcludeyn = .full %>% filter(pdexcludeyn=='Yes'),
  pdexcludereason_1 = .full %>% filter(pdexcludereason=='<registration exclusion reason 1>'), #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
  pdexcludereason_2 = .full %>% filter(pdexcludereason=='<registration exclusion reason 2>'),  #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
  pdexcludereason_3 = .full %>% filter(pdexcludereason=='<registration exclusion reason 3>'), #\add as many repeats of this row as required for registration exclusion reasons.
  awaiting_eligibility1 = .full %>% filter(awaiting_eligibility1=='Yes'),
  eligibility1 = .full %>% filter(eligibility1=='Yes'),
  eligibility1_exclude = .full %>% filter(eligibility1_exclude=='Yes'),
  eligibility1_exclusionreason_1 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 1>'), #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_2 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 2>'), #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_3 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 3>'), #\add as many repeats of this row as required for eligibility exclusion reasons.
  awaiting_consent = .full %>% filter(awaiting_consent=='Yes'),
  consent = .full %>% filter(consent=='Yes'),
  consent_exclude = .full %>% filter(consent_exclude=='Yes'), 
  consent_exclusionreason = .full %>% filter(consent_exclusionreason=='Yes'), 
  consent_exclusionreason_1 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 1>'), #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_2 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 2>'), #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_3 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 3>'), #\add as many repeats of this row as required for consent exclusion reasons.
  awaiting_randomisation = .full %>% filter(awaiting_randomisation=='Yes'),
  randomise = .full %>% filter(randomise=='Yes'),
  randomise_exclude = .full %>% filter(randomise_exclude=='Yes'),
  randomise_excludereason_1 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 1>'), #\replace <randomise exclusion reason 1> with the first response option for exclusion reason at randomisation as it appears in the data
  randomise_excludereason_2 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 2>'), #\replace <randomise exclusion reason 2> with the second response option for exclusion reason at randomisation as it appears in the data
  randomise_excludereason_3 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 3>'), #\add as many repeats of this row as required for randomisation exclusion reasons.
  control = .full %>% filter(Allocation=='Control'),
  intervention = .full %>% filter(Allocation=='Intervention')
  )%>% 
  cohort_label(
    <SITE1> = '<SITE1>', #\replace <SITE1> with site name
    <SITE2>  = '<SITE2>', #\replace <SITE2> with site name. add as many repeats of this row are required
    pdexcludeyn = 'Excluded at registration',
    pdexcludereason_1 = '<registration exclusion reason 1>', #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
    pdexcludereason_2 = '<registration exclusion reason 2>', #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
    pdexcludereason_3 = '<registration exclusion reason 3>', #\add as many repeats of this row as required for registration exclusion reasons.
    awaiting_eligibility1 = 'Awaiting eligibility',
    eligibility1 = 'Eligible',
    eligibility1_exclude = 'Excluded at eligibility',
    eligibility1_exclusionreason_1 = '<eligibility exclusion reason 1>', #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_2 = '<eligibility exclusion reason 2>', #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_3 = '<eligibility exclusion reason 3>', #\add as many repeats of this row as required for eligibility exclusion reasons.
    awaiting_consent = 'Awaiting consent',
    consent = 'Consented',
    consent_exclude = 'Excluded at consent', 
    consent_exclusionreason_1 = '<consent exclusion reason 1>', #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_2 = '<consent exclusion reason 2>', #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_3 = '<consent exclusion reason 3>', #\add as many repeats of this row as required for consent exclusion reasons.
    awaiting_randomisation = 'Awaiting randomisation',
    randomise = 'Randomised',
    randomise_exclude = 'Excluded at randomisation',
    randomise_excludereason_1 = '<randomise exclusion reason 1>', #\replace <randomise exclusion reason 1> with the first response option for exclusion reason at randomisation as it appears in the data
    randomise_excludereason_2 = '<randomise exclusion reason 2>', #\replace <randomise exclusion reason 2> with the second response option for exclusion reason at randomisation as it appears in the data
    randomise_excludereason_3 = '<randomise exclusion reason 3>', #\add as many repeats of this row as required for randomisation exclusion reasons.
    control = 'Control',
    intervention = 'Intervention')

#Define the participant flow boxes and arrows
study_flow <- study_cohorts %>% 
  consort_box_add("full", 0, 50, glue::glue(
    '{cohort_count_adorn(study_cohorts, .full)}<br>
                  {cohort_count_adorn(study_cohorts, <SITE1>)}, #\replace <SITE1> with site name
                  {cohort_count_adorn(study_cohorts, <SITE...>)}, #\replace <SITE...> with site name. add as many repeats of this row are required
                  {cohort_count_adorn(study_cohorts, <SITE3>)}')) %>%  #\replace <SITE3> with site name
  consort_box_add(
    "pdexcludeyn", 0.3 , 50, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, pdexcludeyn)}</b><br>
    {cohort_count_adorn(study_cohorts, pdexcludereason_1)},<br> 
      {cohort_count_adorn(study_cohorts, pdexcludereason_...)},<br> #\add as many repeats of this row are required to include all of the registration exclusion reasons
    {cohort_count_adorn(study_cohorts, pdexcludereason_3)}')) %>%
  consort_box_add(
    "awaiting_eligibility1", 0.3, 48.5, cohort_count_adorn(study_cohorts, awaiting_eligibility1)) %>%
  consort_box_add(
    "eligibility1", 0, 44.5, cohort_count_adorn(study_cohorts, eligibility1))%>%
  consort_box_add(
    "eligibility1_exclude", 0.3 , 46, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, eligibility1_exclude)}</b><br>
     {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_1)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_...)},<br> #\add as many repeats of this row are required to include all of the eligibility exclusion reasons
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_3)}')) %>%
  consort_box_add(
    "awaiting_consent", 0.3, 43.5, cohort_count_adorn(study_cohorts, awaiting_consent)) %>%
  consort_box_add(
    "consent", 0, 39.5, cohort_count_adorn(study_cohorts, consent)) %>%
  consort_box_add(
    "consent_exclude", 0.3 , 41, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, consent_exclude)},</b><br>
    {cohort_count_adorn(study_cohorts, consent_exclusionreason_1)}, <br>
    {cohort_count_adorn(study_cohorts, consent_exclusionreason_...)}, <br> #\add as many repeats of this row are required to include all of the consent exclusion reasons
    {cohort_count_adorn(study_cohorts, consent_exclusionreason_3)}')) %>%
  consort_box_add(
    "awaiting_randomisation", 0.3, 38.5, cohort_count_adorn(study_cohorts, awaiting_randomisation)) %>%
  consort_box_add(
    "randomise", 0, 35, cohort_count_adorn(study_cohorts, randomise)) %>%
  consort_box_add(
    "randomise_exclude", 0.3 , 36, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, randomise_exclude)}</b><br>
      {cohort_count_adorn(study_cohorts, randomise_excludereason_...)}, <br> #\add as many repeats of this row are required to include all of the consent exclusion reasons
      {cohort_count_adorn(study_cohorts, randomise_excludereason_3)}')) %>%
  consort_box_add(
    "control", 0.1, 33.5, cohort_count_adorn(study_cohorts, control)) %>%
  consort_box_add(
    "intervention", -0.1, 33.5, cohort_count_adorn(study_cohorts, intervention)) %>%
  consort_arrow_add(
    end = "eligibility1", end_side = "top", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "consent", end_side = "top", start_x = 0, start_y = 44.5) %>%
  consort_arrow_add(
    end = "randomise", end_side = "top", start_x = 0, start_y = 39.5) %>%
  consort_arrow_add(
    end = "control", end_side = "top", start_x = 0, start_y = 35) %>%
  consort_arrow_add(
    end = "intervention", end_side = "top", start_x = 0, start_y = 35) %>%
  consort_arrow_add(
    end = "pdexcludeyn", end_side = "left", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "awaiting_eligibility1", end_side = "left", start_x = 0, start_y = 48.5) %>%
  consort_arrow_add(
    end = "eligibility1_exclude", end_side = "left", start_x = 0, start_y = 46) %>%
  consort_arrow_add(
    end = "awaiting_consent", end_side = "left", start_x = 0, start_y = 43.5) %>%
  consort_arrow_add(
    end = "consent_exclude", end_side = "left", start_x = 0, start_y = 41) %>%
  consort_arrow_add(
    end = "awaiting_randomisation", end_side = "left", start_x = 0, start_y = 38.5) %>%
  consort_arrow_add(
    end = "randomise_exclude", end_side = "left", start_x = 0, start_y = 36)


#Plot the participant flow
participant_flow_plot <- study_flow %>%
  ggplot() + 
  geom_consort() +
  theme_consort(margin_h = 20, margin_v = 3) 

#Save the participant flow diagram
file_path_flow <- paste0(today, "<STUDY NAME>_ParticipantFlow.png") #\replace <STUDY NAME> with the name of the study
ggsave(file_path_flow, participant_flow_plot, bg="white", width=14, height = 14)

#Insert the participant flow
weekly_report <-body_add_img(weekly_report, file_path_flow, width = 7, height = 7)

#\Template 2: randomised trial with one round of eligibility, baseline must be complete for randomisation to occur
#\and participants cannot be excluded at randomisation (INCOMPLETE)

#\isolate recruitment data
participant_flow_data <- subset(labelled_dataset, redcap_event_name == 'Registration') #\replace registration with the registration event name
participant_flow_data <- select(participant_flow_data, record_id, Site, pdregisterdat, referred, consent, consent_exclude,
                                consent_exclusionreason, eligibility1, eligibility1_exclude, eligibility1_exclusionreason,
                                baseline, baseline_occur, pdexcludeyn, pdexcludereason, randomise, randomise_exclude, randomise_excludereason)

#add withdrawal data
withdrawal_data <- subset(labelled_dataset, redcap_event_name == 'Withdrawal' & #\replace Withdrawal with the name of the withdrawal event
                            (witype=='Withdrawal from study' | witype=='Loss to follow-up - before treatment start'|
                               witype=='Death of participant'))
withdrawal_data <- select(withdrawal_data, record_id, witype)
participant_flow_data <- merge(participant_flow_data, withdrawal_data, by='record_id', all.x=TRUE)

#Add register field
participant_flow_data$Registered <- ifelse(!is.na(participant_flow_data$pdregisterdat), 'Yes', 'No')

#Add a withdrawn before baseline field
participant_flow_data$baseline_withdraw <- ifelse((participant_flow_data$baseline_occur=='No' & !is.na(participant_flow_data$witype)),'Yes','No')

#Add awaiting fields
participant_flow_data$awaiting_eligibility1 <- ifelse((participant_flow_data$Registered=='Yes' & participant_flow_data$eligibility1=='No' & participant_flow_data$eligibility1_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_consent <- ifelse((participant_flow_data$eligibility1=='Yes' & participant_flow_data$consent=='No' & participant_flow_data$consent_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_baseline <- ifelse((participant_flow_data$consent=='Yes' & participant_flow_data$baseline=='No' & participant_flow_data$baseline_withdraw=='No'),'Yes','No')
participant_flow_data$awaiting_randomisation <- ifelse((participant_flow_data$baseline=='Yes' & participant_flow_data$randomise=='No' & participant_flow_data$randomise_exclude=='No'),'Yes','No')


#Define values for participant flow diagram
study_cohorts <- 
  participant_flow_data %>%
  cohort_start("Registered") %>%
  # Define cohorts using named expressions --------------------
cohort_define(
  <SITE1> = .full %>% filter(Site=='<SITE1>'), #\replace <SITE1> with site name
  <SITE2>  = .full %>% filter(Site=='<SITE2>'), #\replace <SITE2> with site name. add as many repeats of this row are required
  pdexcludeyn = .full %>% filter(pdexcludeyn=='Yes'),
  pdexcludereason_1 = .full %>% filter(pdexcludereason=='<registration exclusion reason 1>'), #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
  pdexcludereason_2 = .full %>% filter(pdexcludereason=='<registration exclusion reason 2>'),  #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
  pdexcludereason_3 = .full %>% filter(pdexcludereason=='<registration exclusion reason 3>'), #\add as many repeats of this row as required for registration exclusion reasons.
  awaiting_eligibility1 = .full %>% filter(awaiting_eligibility1=='Yes'),
  eligibility1 = .full %>% filter(eligibility1=='Yes'),
  eligibility1_exclude = .full %>% filter(eligibility1_exclude=='Yes'),
  eligibility1_exclusionreason_1 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 1>'), #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_2 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 2>'), #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_3 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 3>'), #\add as many repeats of this row as required for eligibility exclusion reasons.
  awaiting_consent = .full %>% filter(awaiting_consent=='Yes'),
  consent = .full %>% filter(consent=='Yes'),
  consent_exclude = .full %>% filter(consent_exclude=='Yes'), 
  consent_exclusionreason = .full %>% filter(consent_exclusionreason=='Yes'), 
  consent_exclusionreason_1 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 1>'), #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_2 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 2>'), #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_3 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 3>'), #\add as many repeats of this row as required for consent exclusion reasons.
  awaiting_baseline = .full %>% filter(awaiting_baseline=='Yes'),
  baseline = .full %>% filter(baseline=='Yes'),
  baseline_withdraw = .full %>% filter(baseline_withdraw=='Yes'),
  witype_1 = .full %>% filter(witype=='Withdrawal from study'), 
  witype_2 = .full %>% filter(witype=='Loss to follow-up - before treatment start'), 
  witype_3 = .full %>% filter(witype=='Death of participant'),
  randomise = .full %>% filter(randomise=='Yes'),
  randomise_exclude = .full %>% filter(randomise_exclude=='Yes'),
  randomise_excludereason_1 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 1>'), #\replace <randomise exclusion reason 1> with the first response option for exclusion reason at randomisation as it appears in the data
  randomise_excludereason_2 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 2>'), #\replace <randomise exclusion reason 2> with the second response option for exclusion reason at randomisation as it appears in the data
  randomise_excludereason_3 = .full %>% filter(randomise_excludereason=='<randomise exclusion reason 3>'), #\add as many repeats of this row as required for randomisation exclusion reasons.
)%>% 
  cohort_label(
    <SITE1> = '<SITE1>', #\replace <SITE1> with site name
    <SITE2>  = '<SITE2>', #\replace <SITE2> with site name. add as many repeats of this row are required
    pdexcludeyn = 'Excluded at registration',
    pdexcludereason_1 = '<registration exclusion reason 1>', #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
    pdexcludereason_2 = '<registration exclusion reason 2>', #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
    pdexcludereason_3 = '<registration exclusion reason 3>', #\add as many repeats of this row as required for registration exclusion reasons.
    awaiting_eligibility1 = 'Awaiting eligibility',
    eligibility1 = 'Eligible',
    eligibility1_exclude = 'Excluded at eligibility',
    eligibility1_exclusionreason_1 = '<eligibility exclusion reason 1>', #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_2 = '<eligibility exclusion reason 2>', #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_3 = '<eligibility exclusion reason 3>', #\add as many repeats of this row as required for eligibility exclusion reasons.
    awaiting_consent = 'Awaiting consent',
    consent = 'Consented',
    consent_exclude = 'Excluded at consent', 
    consent_exclusionreason_1 = '<consent exclusion reason 1>', #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_2 = '<consent exclusion reason 2>', #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_3 = '<consent exclusion reason 3>', #\add as many repeats of this row as required for consent exclusion reasons.
    awaiting_baseline = 'Awaiting baseline',
    baseline = 'Enrolled',
    baseline_withdraw = 'Withdrawn prior to baseline',
    witype_1 = 'Withdrawal from study', 
    witype_2 = 'Loss to follow-up - before treatment start', 
    witype_3 = 'Death of participant'),


#Define the participant flow boxes and arrows
study_flow <- study_cohorts %>% 
  consort_box_add("full", 0, 50, glue::glue(
    '{cohort_count_adorn(study_cohorts, .full)}<br>
                  {cohort_count_adorn(study_cohorts, Derby)},
                  {cohort_count_adorn(study_cohorts, Exeter)}')) %>%
  consort_box_add(
    "pdexcludeyn", 0.3 , 50, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, pdexcludeyn)}</b><br>
    {cohort_count_adorn(study_cohorts, pdexcludereason_1)},<br>
      {cohort_count_adorn(study_cohorts, pdexcludereason_2)},<br>
    {cohort_count_adorn(study_cohorts, pdexcludereason_3)}')) %>%
  consort_box_add(
    "awaiting_eligibility1", 0.3, 48.5, cohort_count_adorn(study_cohorts, awaiting_eligibility1)) %>%
  consort_box_add(
    "eligibility1", 0, 44.5, cohort_count_adorn(study_cohorts, eligibility1))%>%
  consort_box_add(
    "eligibility1_exclude", 0.3 , 46, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, eligibility1_exclude)}</b><br>
     {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_1)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_2)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_3)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_4)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_5)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_6)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_7)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_8)}')) %>%
  consort_box_add(
    "awaiting_eligibility2", 0.3, 43.5, cohort_count_adorn(study_cohorts, awaiting_eligibility2)) %>%
  consort_box_add(
    "eligibility2", 0, 39.5, cohort_count_adorn(study_cohorts, eligibility2)) %>%
  consort_box_add(
    "eligibility2_exclude", 0.3 , 41, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, eligibility2_exclude)},</b><br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_1)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_2)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_3)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_4)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_5)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_6)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_7)}, <br>
    {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_8)}')) %>%
  consort_box_add(
    "awaiting_consent", 0.3, 38.5, cohort_count_adorn(study_cohorts, awaiting_consent)) %>%
  consort_box_add(
    "consent", 0, 35, cohort_count_adorn(study_cohorts, consent)) %>%
  consort_box_add(
    "consent_exclude", 0.3 , 36, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, consent_exclude)}</b><br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_1)}, <br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_2)},<br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_3)},<br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_4)},<br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_5)},<br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_6)},<br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_7)}')) %>%
  consort_box_add(
    "awaiting_baseline", 0.3, 33.5, cohort_count_adorn(study_cohorts, awaiting_baseline)) %>%
  consort_box_add(
    "baseline", 0, 30.5, cohort_count_adorn(study_cohorts, baseline)) %>%
  consort_box_add(
    "baseline_withdraw", 0.3 , 32, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, baseline_withdraw)}</b><br>
      {cohort_count_adorn(study_cohorts, witype_1)}, <br>
      {cohort_count_adorn(study_cohorts, witype_2)},<br>
      {cohort_count_adorn(study_cohorts, witype_3)}')) %>%
  consort_arrow_add(
    end = "eligibility1", end_side = "top", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "eligibility2", end_side = "top", start_x = 0, start_y = 44.5) %>%
  consort_arrow_add(
    end = "consent", end_side = "top", start_x = 0, start_y = 39.5) %>%
  consort_arrow_add(
    end = "baseline", end_side = "top", start_x = 0, start_y = 35) %>%
  consort_arrow_add(
    end = "pdexcludeyn", end_side = "left", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "awaiting_eligibility1", end_side = "left", start_x = 0, start_y = 48.5) %>%
  consort_arrow_add(
    end = "eligibility1_exclude", end_side = "left", start_x = 0, start_y = 46) %>%
  consort_arrow_add(
    end = "awaiting_eligibility2", end_side = "left", start_x = 0, start_y = 43.5) %>%
  consort_arrow_add(
    end = "eligibility2_exclude", end_side = "left", start_x = 0, start_y = 41) %>%
  consort_arrow_add(
    end = "awaiting_consent", end_side = "left", start_x = 0, start_y = 38.5) %>%
  consort_arrow_add(
    end = "consent_exclude", end_side = "left", start_x = 0, start_y = 36) %>%
  consort_arrow_add(
    end = "awaiting_baseline", end_side = "left", start_x = 0, start_y = 33.5) %>%
  consort_arrow_add(
    end = "baseline_withdraw", end_side = "left", start_x = 0, start_y = 32)

#Plot the participant flow
participant_flow_plot <- study_flow %>%
  ggplot() + 
  geom_consort() +
  theme_consort(margin_h = 20, margin_v = 3) 

#Save the participant flow diagram
file_path_flow <- paste0(today, "ERASE_ParticipantFlow.png")
ggsave(file_path_flow, participant_flow_plot, bg="white", width=14, height = 14)

#Insert the participant flow
weekly_report <-body_add_img(weekly_report, file_path_flow, width = 7, height = 7)

#\Template 3: non-randomised trial with two rounds of recruitment, where participants are considered
#\enrolled after baseline

#Select participant flow data fields
participant_flow_data <- subset(labelled_dataset, redcap_event_name == 'Registration') #\replace registration with the registration event name
participant_flow_data <- select(participant_flow_data, record_id, Site, referred, consent, consent_exclude,
                                consent_exclusionreason, eligibility1, eligibility1_exclude, eligibility1_exclusionreason,
                                eligibility2, eligibility2_exclude, eligibility2_exclusionreason, baseline, baseline_occur,
                                pdexcludeyn, pdexcludereason)

#Add withdrawal data
withdrawal_data <- subset(labelled_dataset, redcap_event_name == 'Withdrawal' &
                            (witype=='Withdrawal from study' | witype=='Loss to follow-up - before treatment start'|
                               witype=='Death of participant')) #\replace withdrawal types with those used in your study
withdrawal_data <- select(withdrawal_data, record_id, witype)
participant_flow_data <- merge(participant_flow_data, withdrawal_data, by='record_id', all.x=TRUE)

#Add register field
participant_flow_data$Register <- ifelse(!is.na(participant_flow_data$record_id), 'Yes', 'No') 

#Add a withdrawn before baseline field
participant_flow_data$baseline_withdraw <- ifelse((participant_flow_data$baseline_occur=='No' & !is.na(participant_flow_data$witype)),'Yes','No')

#Add awaiting fields
participant_flow_data$awaiting_eligibility1 <- ifelse((participant_flow_data$referred=='Yes' & participant_flow_data$eligibility1=='No' & participant_flow_data$eligibility1_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_consent <- ifelse((participant_flow_data$eligibility1=='Yes' & participant_flow_data$consent=='No' & participant_flow_data$consent_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_eligibility2 <- ifelse((participant_flow_data$consent=='Yes' & participant_flow_data$eligibility2=='No' & participant_flow_data$eligibility2_exclude=='No'),'Yes','No')
participant_flow_data$awaiting_baseline <- ifelse((participant_flow_data$eligibility2=='Yes' & participant_flow_data$baseline=='No' & participant_flow_data$baseline_withdraw=='No'),'Yes','No')


#Define values for participant flow diagram
study_cohorts <- 
  participant_flow_data %>%
  cohort_start("Register") %>%
  # Define cohorts using named expressions --------------------
cohort_define(
  <SITE1> = .full %>% filter(Site=='<SITE1>'), #\replace <SITE1> with site name
  <SITE2>  = .full %>% filter(Site=='<SITE2>'), #\replace <SITE2> with site name. add as many repeats of this row are required
  pdexcludeyn = .full %>% filter(pdexcludeyn=='Yes'),
  pdexcludereason_1 = .full %>% filter(pdexcludereason=='<registration exclusion reason 1>'), #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
  pdexcludereason_2 = .full %>% filter(pdexcludereason=='<registration exclusion reason 2>'),  #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
  pdexcludereason_3 = .full %>% filter(pdexcludereason=='<registration exclusion reason 3>'), #\add as many repeats of this row as required for registration exclusion reasons.
  awaiting_eligibility1 = .full %>% filter(awaiting_eligibility1=='Yes'),
  eligibility1 = .full %>% filter(eligibility1=='Yes'),
  eligibility1_exclude = .full %>% filter(eligibility1_exclude=='Yes'),
  eligibility1_exclusionreason_1 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 1>'), #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_2 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 2>'), #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
  eligibility1_exclusionreason_3 = .full %>% filter(eligibility1_exclusionreason=='<eligibility exclusion reason 3>'), #\add as many repeats of this row as required for eligibility exclusion reasons.
  awaiting_consent = .full %>% filter(awaiting_consent=='Yes'),
  consent = .full %>% filter(consent=='Yes'),
  consent_exclude = .full %>% filter(consent_exclude=='Yes'), 
  consent_exclusionreason = .full %>% filter(consent_exclusionreason=='Yes'), 
  consent_exclusionreason_1 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 1>'), #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_2 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 2>'), #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
  consent_exclusionreason_3 = .full %>% filter(consent_exclusionreason=='<consent exclusion reason 3>'), #\add as many repeats of this row as required for consent exclusion reasons.
  awaiting_eligibility2 = .full %>% filter(awaiting_eligibility2=='Yes'), 
  eligibility2 = .full %>% filter(eligibility2=='Yes'), 
  eligibility2_exclude = .full %>% filter(eligibility2_exclude=='Yes'),
  eligibility2_exclusionreason_1 = .full %>% filter(eligibility2_exclusionreason=='<eligibility2 exclusion reason 1>'), #\replace <eligibility2 exclusion reason 1> with the first response option for exclusion reason at eligibility 2 as it appears in the data
  eligibility2_exclusionreason_2 = .full %>% filter(eligibility2_exclusionreason=='<eligibility2 exclusion reason 2>'), #\replace <eligibility2 exclusion reason 2> with the second response option for exclusion reason at eligibility 2 as it appears in the data
  eligibility2_exclusionreason_3 = .full %>% filter(eligibility2_exclusionreason=='<eligibility2 exclusion reason 3>'), #\add as many repeats of this row as required for eligibility 2 exclusion reasons.
  awaiting_baseline = .full %>% filter(awaiting_baseline=='Yes'),
  baseline = .full %>% filter(baseline=='Yes'),
  baseline_withdraw = .full %>% filter(baseline_withdraw=='Yes'),
  witype_1 = .full %>% filter(witype=='<withdrawal reason 1>'), #\replace <withdrawal reason 1> with the first response option for withdrawal reason at withdrawal as it appears in the data
  witype_2 = .full %>% filter(witype=='<withdrawal reason 2>'), #\replace <withdrawal reason 2> with the second response option for withdrawal reason at withdrawal as it appears in the data
  witype_3 = .full %>% filter(witype=='<withdrawal reason 2>'))%>% #\add as many repeats of this row as required for withdrawal reasons.
  cohort_label(
    SITE1> = '<SITE1>', #\replace <SITE1> with site name
    <SITE2>  = '<SITE2>', #\replace <SITE2> with site name. add as many repeats of this row are required
    pdexcludeyn = 'Excluded at referral',
    pdexcludereason_1 = '<registration exclusion reason 1>', #\replace <registration exclusion reason 1> with the first response option for exclusion reason at registration as it appears in the data
    pdexcludereason_2 = '<registration exclusion reason 2>', #\replace <registration exclusion reason 2> with the second response option for exclusion reason at registration as it appears in the data
    pdexcludereason_3 = '<registration exclusion reason 3>', #\add as many repeats of this row as required for registration exclusion reasons.
    awaiting_eligibility1 = 'Awaiting initial screening',
    eligibility1 = 'Eligible at intial screening',
    eligibility1_exclude = 'Excluded at initial screening',
    eligibility1_exclusionreason_1 = '<eligibility exclusion reason 1>', #\replace <eligibility exclusion reason 1> with the first response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_2 = '<eligibility exclusion reason 2>', #\replace <eligibility exclusion reason 2> with the second response option for exclusion reason at eligibility as it appears in the data
    eligibility1_exclusionreason_3 = '<eligibility exclusion reason 3>', #\add as many repeats of this row as required for eligibility exclusion reasons.
    awaiting_consent = 'Awaiting consent',
    consent = 'Consented',
    consent_exclude = 'Excluded at consent', 
    consent_exclusionreason_1 = '<consent exclusion reason 1>', #\replace <consent exclusion reason 1> with the first response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_2 = '<consent exclusion reason 2>', #\replace <consent exclusion reason 2> with the second response option for exclusion reason at consent as it appears in the data
    consent_exclusionreason_3 = '<consent exclusion reason 3>', #\add as many repeats of this row as required for consent exclusion reasons.
    awaiting_eligibility2 = 'Awaiting detailed screening', 
    eligibility2 = 'Eligible at detailed screening', 
    eligibility2_exclude = 'Excluded at detailed screening',
    eligibility2_exclusionreason_1 = '<eligibility2 exclusion reason 1>', #\replace <eligibility2 exclusion reason 1> with the first response option for exclusion reason at eligibility 2 as it appears in the data
    eligibility2_exclusionreason_2 = '<eligibility2 exclusion reason 2>', #\replace <eligibility2 exclusion reason 2> with the second response option for exclusion reason at eligibility 2 as it appears in the data
    eligibility2_exclusionreason_3 = '<eligibility2 exclusion reason 3>', #\add as many repeats of this row as required for eligibility 2 exclusion reasons.
    awaiting_baseline = 'Awaiting baseline',
    baseline = 'Enrolled',
    baseline_withdraw = 'Withdrawn prior to baseline',
    witype_1 = '<withdrawal reason 1>', #\replace <withdrawal reason 1> with the first response option for withdrawal reason at withdrawal as it appears in the data
    witype_2 = '<withdrawal reason 2>', #\replace <withdrawal reason 2> with the second response option for withdrawal reason at withdrawal as it appears in the data
    witype_3 = '<withdrawal reason 2>') #\add as many repeats of this row as required for withdrawal reasons.

#Define the participant flow boxes and arrows
study_flow <- study_cohorts %>% 
  consort_box_add("full", 0, 50, glue::glue(
    '{cohort_count_adorn(study_cohorts, .full)}<br>
    {cohort_count_adorn(study_cohorts, <SITE1>)}, #\replace <SITE1> with site name
    {cohort_count_adorn(study_cohorts, <SITE...>)}, #\replace <SITE...> with site name. add as many repeats of this row are required
    {cohort_count_adorn(study_cohorts, <SITE3>)}')) %>%  #\replace <SITE3> with site name
  
  consort_box_add(
    "pdexcludeyn", 0.3 , 50, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, pdexcludeyn)}</b><br>
      {cohort_count_adorn(study_cohorts, pdexcludereason_1)},<br> 
      {cohort_count_adorn(study_cohorts, pdexcludereason_...)},<br> #\add as many repeats of this row are required to include all of the registration exclusion reasons
      {cohort_count_adorn(study_cohorts, pdexcludereason_3)}')) %>%
  consort_box_add(
    "awaiting_eligibility1", 0.3, 48.5, cohort_count_adorn(study_cohorts, awaiting_eligibility1)) %>%
  consort_box_add(
    "eligibility1", 0, 44.5, cohort_count_adorn(study_cohorts, eligibility1))%>%
  consort_box_add(
    "eligibility1_exclude", 0.3 , 46, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, eligibility1_exclude)}</b><br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_1)},<br>
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_...)},<br> #\add as many repeats of this row are required to include all of the eligibility exclusion reasons
      {cohort_count_adorn(study_cohorts, eligibility1_exclusionreason_3)}')) %>%
  consort_box_add(
    "awaiting_eligibility2", 0.3, 43.5, cohort_count_adorn(study_cohorts, awaiting_eligibility2)) %>%
  consort_box_add(
    "eligibility2", 0, 39.5, cohort_count_adorn(study_cohorts, eligibility2)) %>%
  consort_box_add(
    "eligibility2_exclude", 0.3 , 41, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, eligibility2_exclude)},</b><br>
      {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_)},<br>
      {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_...)},<br> #\add as many repeats of this row are required to include all of the eligibility 2 exclusion reasons
      {cohort_count_adorn(study_cohorts, eligibility2_exclusionreason_3)}')) %>%
  consort_box_add(
    "awaiting_consent", 0.3, 38.5, cohort_count_adorn(study_cohorts, awaiting_consent)) %>%
  consort_box_add(
    "consent", 0, 35, cohort_count_adorn(study_cohorts, consent)) %>%
  consort_box_add(
    "consent_exclude", 0.3 , 36, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, consent_exclude)}</b><br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_1)}, <br>
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_...)}, <br> #\add as many repeats of this row are required to include all of the consent exclusion reasons
      {cohort_count_adorn(study_cohorts, consent_exclusionreason_3)}')) %>%
  consort_box_add(
    "awaiting_baseline", 0.3, 33.5, cohort_count_adorn(study_cohorts, awaiting_baseline)) %>%
  consort_box_add(
    "baseline", 0, 30.5, cohort_count_adorn(study_cohorts, baseline)) %>%
  consort_box_add(
    "baseline_withdraw", 0.3 , 32, glue::glue(
      '<b> {cohort_count_adorn(study_cohorts, baseline_withdraw)}</b><br>
      {cohort_count_adorn(study_cohorts, witype_1)}, <br>
      {cohort_count_adorn(study_cohorts, witype_...)},<br> #\add as many repeats of this row are required to include all of the withdrawal reasons
      {cohort_count_adorn(study_cohorts, witype_3)}')) %>%
  consort_arrow_add(
    end = "eligibility1", end_side = "top", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "eligibility2", end_side = "top", start_x = 0, start_y = 44.5) %>%
  consort_arrow_add(
    end = "consent", end_side = "top", start_x = 0, start_y = 39.5) %>%
  consort_arrow_add(
    end = "baseline", end_side = "top", start_x = 0, start_y = 35) %>%
  consort_arrow_add(
    end = "pdexcludeyn", end_side = "left", start_x = 0, start_y = 50) %>%
  consort_arrow_add(
    end = "awaiting_eligibility1", end_side = "left", start_x = 0, start_y = 48.5) %>%
  consort_arrow_add(
    end = "eligibility1_exclude", end_side = "left", start_x = 0, start_y = 46) %>%
  consort_arrow_add(
    end = "awaiting_eligibility2", end_side = "left", start_x = 0, start_y = 43.5) %>%
  consort_arrow_add(
    end = "eligibility2_exclude", end_side = "left", start_x = 0, start_y = 41) %>%
  consort_arrow_add(
    end = "awaiting_consent", end_side = "left", start_x = 0, start_y = 38.5) %>%
  consort_arrow_add(
    end = "consent_exclude", end_side = "left", start_x = 0, start_y = 36) %>%
  consort_arrow_add(
    end = "awaiting_baseline", end_side = "left", start_x = 0, start_y = 33.5) %>%
  consort_arrow_add(
    end = "baseline_withdraw", end_side = "left", start_x = 0, start_y = 32)

#Plot the participant flow
participant_flow_plot <- study_flow %>%
  ggplot() + 
  geom_consort() +
  theme_consort(margin_h = 20, margin_v = 3) 

#Save the participant flow diagram
file_path_flow <- paste0(today, "<STUDY NAME>_ParticipantFlow.png") #\replace <STUDY NAME> with the name of the study
ggsave(file_path_flow, participant_flow_plot, bg="white", width=14, height = 14)

#Insert the participant flow
weekly_report <-body_add_img(weekly_report, file_path_flow, width = 7, height = 7)