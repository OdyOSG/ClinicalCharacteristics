/* Create Patient Level data Table */
DROP TABLE IF EXISTS @patient_level_data;
CREATE TABLE @patient_level_data (
	target_cohort_id BIGINT NOT NULL,
	subject_id BIGINT NOT NULL,
	time_label VARCHAR(50) NOT NULL,
	domain_table VARCHAR(50) NOT NULL,
	patient_line VARCHAR(50) NOT NULL,
	value_type VARCHAR(50) NOT NULL,
	value_id BIGINT NOT NULL,
	value FLOAT NOT NULL
	);
