INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'cost' AS domain_table,
  'cost' AS patient_line,
  'totalCost' AS value_type,
  -999 AS  value_id,
  a.value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      sum(c.cost) AS value
    FROM @target_cohort_table t
    JOIN @cdm_database_schema.cost c
    ON t.subject_id = c.person_id
    GROUP BY cohort_definition_id, subject_id
)a;
