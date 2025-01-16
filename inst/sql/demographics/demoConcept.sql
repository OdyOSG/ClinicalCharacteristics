INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'person' AS domain_table,
  'binary' AS patient_line,
  {valueId} AS  value_id,
  1 AS value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      d.{valueDescription}
    FROM @target_table t
    JOIN @cdm_database_schema.person d
    ON t.subject_id = d.person_id
    WHERE d.{valueDescription} = {valueId}
) a
;
