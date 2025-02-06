CREATE TABLE @concept_set_occurrence_table (
  target_cohort_id INT NOT NULL,
  subject_id BIGINT NOT NULL,
  cohort_start_date DATE NOT NULL,
  cohort_end_date DATE NOT NULL,
  domain_table VARCHAR(50) NOT NULL,
  time_label VARCHAR(50) NOT NULL,
  raw_occurrence_id INT NOT NULL,
  raw_occurrence_description VARCHAR(50) NOT NULL,
  event_date DATE NOT NULL
)
