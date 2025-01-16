SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        d.raw_occurrence_id,
        'timeToFirst' AS patient_line,
        DATEDIFF(day, d.cohort_start_date, d.event_date) AS value
FROM (
  SELECT a.target_cohort_id, a.subject_id, a.time_label, a.domain_table, a.raw_occurrence_id, a.cohort_start_date, a.event_date
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.target_cohort_id, l.subject_id, l.time_label, l.raw_occurrence_id order by l.event_date ASC) as ordinal
        FROM @concept_set_occurrence_table l
      ) a
      WHERE ordinal = 1
) d
