SELECT
  a.cohort_definition_id AS target_cohort_id,
  a.tot_subjects
INTO #cohort_denom
FROM (
  SELECT
    t.cohort_definition_id,
    COUNT(DISTINCT t.subject_id) as tot_subjects
  FROM @target_cohort_table t
  GROUP BY t.cohort_definition_id
) a
;
