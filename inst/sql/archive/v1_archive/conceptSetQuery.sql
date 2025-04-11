/* Query for a Concept Set */
WITH T1 AS (
  /* Get time windows */
  SELECT * FROM @timeWindowTable tw
  WHERE time_id IN ({timeIds})
),
T2 AS (
  /* Get codesets */
  SELECT * FROM @codesetTable cs
  WHERE codeset_id IN ({codesetIds})
)
INSERT INTO @conceptSetRawOccurrenceTable
/* Find presence of covariate in {domain}*/
SELECT
  --{ordId} AS ordinal_id,
  t.cohort_definition_id AS target_cohort_id,
  t.subject_id,
  t.cohort_start_date,
  t.cohort_end_date,
  {domain} AS domain_table,
  tw.time_label,
  cs.codeset_id AS raw_occurrence_id,
  'codeset_id' AS raw_occurrence_description,
  d.{domain_trans$event_date} AS record_date
FROM @targetTable t
JOIN @cdmDatabaseSchema.{domain} d ON t.subject_id = d.person_id
JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
INNER JOIN T1 tw
      ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
      AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
;
