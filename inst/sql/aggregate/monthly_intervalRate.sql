INSERT INTO @continuous_table
SELECT
    t.target_cohort_id,
    t.ordinal_id,
    t.time_label,
    t.line_item_label,
    t.patient_line,
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
    COUNT(DISTINCT subject_id) AS subject_count,
    AVG(m.event_per_interval) As mean,
    STDDEV(m.event_per_interval) AS sd,
    min(m.event_per_interval) AS min,
    PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY m.event_per_interval) as p10,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY m.event_per_interval) as p25,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY m.event_per_interval) as median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY m.event_per_interval) as p75,
    PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY m.event_per_interval) as p90,
    max(m.event_per_interval) AS max
  FROM (
      SELECT
        d.target_cohort_id,
        d.subject_id,
        d.ordinal_id,
        d.time_label,
        d.line_item_label,
        d.patient_line,
        r.interval_time,
        (d.value * 1.0) / r.interval_time AS event_per_interval
      FROM @pat_ts_tab d
      LEFT JOIN (
          SELECT
            t.cohort_definition_id,
            t.subject_id,
            time_label,
            (DATEDIFF(day, DATEADD(day, t.time_a, t.cohort_start_date), LEAST(t.cohort_end_date, DATEADD(day, t.time_b, t.cohort_start_date)))*1.0 + 1) / 365 AS interval_time
          FROM (
            SELECT cohort_definition_id,
                   subject_id,
                   cohort_start_date,
                   cohort_end_date,
                   tw.time_label,
                   tw.time_a,
                   tw.time_b
            FROM @target_cohort_table
            CROSS JOIN @time_window tw
          ) t
      ) r
      ON d.target_cohort_id = r.cohort_definition_id AND d.subject_id = r.subject_id AND d.time_label = r.time_label
      WHERE d.statistic_type = 'monthly_intervalRate' AND d.patient_line = 'observedCount'
) m
  GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, patient_line
) t
;
