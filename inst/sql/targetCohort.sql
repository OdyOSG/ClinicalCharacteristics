/*Make target cohort temp table*/
SELECT *
INTO @target_table
FROM @work_database_schema.@cohort_table
WHERE cohort_definition_id IN ({cohortIds})
;
