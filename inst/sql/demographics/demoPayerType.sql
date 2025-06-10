/* Identify Patient payer type (rank by lowest payer concept)*/
INSERT INTO @patient_level_data
SELECT
  c.cohort_definition_id AS target_cohort_id,
  c.subject_id,
  'Static at Index' AS time_label,
  'payer_plan_period' AS domain_table,
  'payer_type' AS patient_line,
  'payer_concept_id' AS value_type,
  -999 AS value_id,
  c.payer_concept_id AS value
FROM (
  SELECT b.cohort_definition_id, b.subject_id, b.payer_concept_id
  FROM(
    SELECT *,
        ROW_NUMBER() OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY t_diff, payer_concept_id DESC) AS rn
    FROM (
      SELECT
            t.*,
            d.payer_concept_id,
            d.payer_plan_period_start_date,
            d.payer_plan_period_end_date,
            ABS(DATEDIFF(day, t.cohort_start_date, d.payer_plan_period_start_date)) as t_diff
      FROM @target_table t
      JOIN @cdm_database_schema.payer_plan_period d
        ON t.subject_id = d.person_id
        AND (t.cohort_end_date <= d.payer_plan_period_end_date OR t.cohort_start_date <= d.payer_plan_period_end_date)
      ) a
    ) b
    WHERE b.rn = 1
) c
;
