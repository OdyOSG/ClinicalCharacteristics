INSERT INTO @patient_level_data
SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.subject_id,
  'Static at Index' AS time_label,
  'person' AS domain_table,
  'age' AS patient_line,
  'year_of_birth' AS value_type,
  -999 AS  value_id,
  a.value
FROM (
    SELECT
      t.cohort_definition_id,
      t.subject_id,
      YEAR(t.cohort_start_date) - d.year_of_birth AS value
    FROM (
      SELECT a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date
        FROM(
          SELECT *, ROW_NUMBER() OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY cohort_start_date) AS rn
          FROM @target_table
          ) a
        WHERE a.rn = 1
    ) t
    JOIN @cdm_database_schema.person d
    ON t.subject_id = d.person_id
) a
;
