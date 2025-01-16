SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        d.raw_occurrence_id,
        d.ordinal_id
        d.statistic_type,
        d.line_item_class,
        COUNT(DISTINCT d.event_date) AS value
FROM (
  SELECT d.*, m.ordinal_id, m.statistic_type, m.line_item_class
  FROM @concept_set_occurrence_table d
  INNER JOIN (
    SELECT * FROM @ts_meta_table WHERE statistic_type = 'CategoricalPrevalence'
  ) m
  ON d.raw_occurrence_id = m.value_id AND d.time_label = m.time_label
  WHERE d.cohort_end_date >= d.event_date
) d
GROUP BY d.target_cohort_id, d.subject_id, d.time_label, d.domain_table, d.raw_occurrence_id
