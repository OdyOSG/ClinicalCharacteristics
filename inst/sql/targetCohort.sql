/*Make target cohort temp table*/
DROP TABLE IF EXISTS  @target_table;
SELECT *
INTO @target_table
FROM @work_database_schema.@cohort_table
WHERE cohort_definition_id IN ({cohortIds})
;
