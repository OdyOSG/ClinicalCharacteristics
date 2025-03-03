/* Identify patient race */
INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'person' AS domain_table,
  'race' AS patient_line,
  'race_concept_id' AS value_type,
  -999 AS value_id,
  a.race_concept_id AS value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      d.race_concept_id
    FROM @target_table t
    JOIN @cdm_database_schema.person d
    ON t.subject_id = d.person_id
) a
;
