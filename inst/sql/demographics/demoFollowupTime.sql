INSERT INTO @patient_level_data
SELECT
  b.cohort_definition_id AS target_cohort_id,
  b.subject_id,
  'Static at Index' AS time_label,
  'cohort' AS domain_table,
  'cohort_follow_up' AS patient_line,
  'cohort_follow_up' AS value_type,
  -999 AS value_id,
  b.value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      DATEDIFF(day, t.cohort_start_date, t.cohort_end_date) AS value
    FROM @target_table t
) b
;
