INSERT INTO @patient_level_data
SELECT
  b.cohort_definition_id AS target_cohort_id,
  b.subject_id,
  'Static at Index' AS time_label,
  'payer_plan_period' AS domain_table,
  'payer_type' AS patient_line,
  'payer_concept_id' AS value_type,
  -999 AS value_id,
  b.payer_concept_id AS value
FROM (
  SELECT a.cohort_definition_id, a.subject_id, a.payer_concept_id
    FROM(
      SELECT
        t.*,
        d.payer_concept_id,
        ROW_NUMBER() OVER (PARTITION BY t.cohort_definition_id, t.subject_id ORDER BY d.payer_plan_period_id, d.payer_concept_id DESC) AS rn
      FROM @target_table t
      JOIN @cdm_database_schema.payer_plan_period d
        ON t.subject_id = d.person_id
        AND t.cohort_start_date BETWEEN d.payer_plan_period_start_date AND d.payer_plan_period_end_date
      ) a
    WHERE a.rn = 1
  ) b

;
