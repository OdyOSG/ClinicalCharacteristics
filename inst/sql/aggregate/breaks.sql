INSERT INTO @categorical_table
SELECT
  j.target_cohort_id,
  j.ordinal_id,
  j.time_label,
  j.line_item_label,
  j.patient_line,
  COUNT(DISTINCT j.subject_count) AS subject_count
FROM(
  SELECT
    h.*,
    {breaksStrategy}
  FROM (
    SELECT
      m.*,
      COUNT(DISTINCT SUBJECT_ID) AS subject_count
    FROM (
        SELECT d.*
        FROM #pat_ts_tab d
        WHERE d.statistic_type = 'breaks'
    ) m
    GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label
  ) h
) j
;
