INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'person' AS domain_table,
  'age' AS patient_line,
  'demographic' AS value_type,
  -999 AS  value_id,
  a.value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      YEAR(t.cohort_start_date) - d.year_of_birth AS value
    FROM @target_table t
    JOIN @cdm_database_schema.person d
    ON t.subject_id = d.person_id
) a
;
