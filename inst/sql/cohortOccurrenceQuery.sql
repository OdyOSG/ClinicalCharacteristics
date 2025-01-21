/* Find presence of cohort in cohort table {domain}*/
CREATE TABLE @cohort_occurrence_table AS
SELECT
  t.cohort_definition_id AS target_cohort_id,
  t.subject_id,
  t.cohort_start_date,
  t.cohort_end_date,
  'cohort' AS domain_table,
  tw.time_label,
  ch.cohort_definition_id AS raw_occurrence_id,
  'cohort_definition_id' AS raw_occurrence_description,
  ch.cohort_start_date AS event_start_date,
  ch.cohort_end_date AS event_end_date
FROM @target_cohort_table t
JOIN (
  SELECT *
  FROM @work_database_schema.{domain}
  WHERE cohort_definition_id IN ({cohort_ids})
  ) ch ON t.subject_id = ch.subject_id
INNER JOIN (
  /* Get time windows */
  SELECT *
  FROM @time_window_table tt
  WHERE time_label IN ('{time_labels}')
) tw
ON DATEADD(day, tw.time_b, t.cohort_start_date) >= ch.cohort_start_date
  AND DATEADD(day, tw.time_a, t.cohort_start_date) <= ch.cohort_end_date
;
