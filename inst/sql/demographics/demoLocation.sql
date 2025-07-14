/* Identify patient location */
INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'location' AS domain_table,
  'location' AS patient_line,
  'location_id' AS value_type,
  -999 AS value_id,
  COALESCE(a.location_id, 0) AS value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      d.location_id
    FROM @target_table t
    JOIN @cdm_database_schema.person d
    ON t.subject_id = d.person_id
) a
;
