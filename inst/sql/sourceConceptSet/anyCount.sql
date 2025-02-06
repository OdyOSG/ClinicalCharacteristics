INSERT INTO @patient_level_data
SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        'anyCount' AS patient_line,
        d.raw_occurrence_id as value_id,
        COUNT(DISTINCT d.event_date) AS value
FROM @concept_set_occurrence_table d
GROUP BY d.target_cohort_id, d.subject_id, d.time_label, d.domain_table, d.raw_occurrence_id
;
