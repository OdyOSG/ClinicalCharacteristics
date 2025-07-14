INSERT INTO @continuous_table
SELECT
    t.target_cohort_id,
    t.ordinal_id,
    t.time_label,
    t.line_item_label,
    t.patient_line,
    t.statistic_type,
    t.subject_count,
    t.mean,
    CASE WHEN t.sd IS NULL THEN -5 ELSE t.sd END AS sd,
    t.min,
    t.p10,
    t.p25,
    t.median,
    t.p75,
    t.p90,
    t.max
FROM (
  SELECT
    m.target_cohort_id,
    m.ordinal_id,
    m.time_label,
    m.line_item_label,
    m.patient_line,
    m.statistic_type,
    COUNT(DISTINCT subject_id) AS subject_count,
    AVG(m.value) As mean,
    STDDEV(m.value) AS sd,
    min(m.value) AS min,
    PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY m.value) as p10,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY m.value) as p25,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY m.value) as median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY m.value) as p75,
    PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY m.value) as p90,
    max(m.value) AS max
  FROM (
      SELECT d.*
      FROM @pat_ts_tab d
      WHERE d.statistic_type = 'continuousDistribution'
  ) m
  GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, patient_line, statistic_type
) t
;
