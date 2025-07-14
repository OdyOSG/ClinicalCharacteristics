DROP TABLE IF EXISTS #pat_ts_score;

CREATE TABLE #pat_ts_score AS
  SELECT  t.cohort_definition_id AS target_cohort_id, t.subject_id,
    '{timeLabel}' AS time_label,
    '{patientLine}' AS patient_line,
    '{sectionLabel}' AS section_label,
    'scoreTransformation' AS statistic_type,
    CASE WHEN d.charlson_score IS NULL THEN 0 ELSE d.charlson_score END AS charlson_score
    FROM (
      SELECT tt.target_cohort_id, tt.subject_id,
      SUM(score_value) as charlson_score
      FROM(
        SELECT a.*,
          CASE
            {scoreCaseWhen}
            ELSE 0 END AS score_value
        FROM @pat_ts_tab a
        WHERE a.statistic_type = 'scoreTransformation'
      ) tt
      GROUP BY tt.target_cohort_id, tt.subject_id
    ) d
    RIGHT JOIN @target_cohort_table t
      ON d.target_cohort_id = t.cohort_definition_id AND d.subject_id = t.subject_id
;
