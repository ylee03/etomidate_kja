/*
	SQL code
	used to build the schema `mimic.etomidate'
	
	Developed by Younsuk Lee (ylee@dgu.ac.kr)
	2020 Distributed under the terms of the Creative Commons Attribution license (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted use, distribution, and reproduction in any medium, provided the original work is properly cited.

	Prerequisite: 
		1) A copy of the MIMIC-III on a local machine
		1`) A copy of the MIMIC-III tools
		2) A qualification for use of the MIMIC-III
*/

-- main cohort table
--
drop materialized view if exist etomidate._etomidate_propofol_cohort;
create materialized view etomidate._etomidate_propofol_cohort AS 
WITH e00 AS (
        SELECT DISTINCT diagnoses_icd.hadm_id
          FROM mimiciii.diagnoses_icd
         WHERE diagnoses_icd.icd9_code::text = '25541'::text OR diagnoses_icd.icd9_code::text ~* '^255[2-5]'::text AND diagnoses_icd.seq_num = 1
       ), e0 AS (
        SELECT prescriptions.row_id,
           prescriptions.subject_id,
           prescriptions.hadm_id,
           prescriptions.icustay_id,
           prescriptions.startdate,
           prescriptions.enddate,
           prescriptions.drug_type,
           prescriptions.drug,
           prescriptions.drug_name_poe,
           prescriptions.drug_name_generic,
           prescriptions.formulary_drug_cd,
           prescriptions.gsn,
           prescriptions.ndc,
           prescriptions.prod_strength,
           prescriptions.dose_val_rx,
           prescriptions.dose_unit_rx,
           prescriptions.form_val_disp,
           prescriptions.form_unit_disp,
           prescriptions.route
          FROM mimiciii.prescriptions
         WHERE prescriptions.drug::text ~* 'etomidate'::text
       ), p0 AS (
        SELECT prescriptions.row_id,
           prescriptions.subject_id,
           prescriptions.hadm_id,
           prescriptions.icustay_id,
           prescriptions.startdate,
           prescriptions.enddate,
           prescriptions.drug_type,
           prescriptions.drug,
           prescriptions.drug_name_poe,
           prescriptions.drug_name_generic,
           prescriptions.formulary_drug_cd,
           prescriptions.gsn,
           prescriptions.ndc,
           prescriptions.prod_strength,
           prescriptions.dose_val_rx,
           prescriptions.dose_unit_rx,
           prescriptions.form_val_disp,
           prescriptions.form_unit_disp,
           prescriptions.route
          FROM mimiciii.prescriptions
         WHERE prescriptions.drug::text ~* 'propofol'::text AND NOT (prescriptions.hadm_id IN ( SELECT e0.hadm_id
                  FROM e0))
       ), e0p0 AS (
        SELECT 'etomidate'::text AS study,
           e0.row_id,
           e0.subject_id,
           e0.hadm_id,
           e0.icustay_id,
           e0.startdate,
           e0.enddate,
           e0.drug_type,
           e0.drug,
           e0.drug_name_poe,
           e0.drug_name_generic,
           e0.formulary_drug_cd,
           e0.gsn,
           e0.ndc,
           e0.prod_strength,
           e0.dose_val_rx,
           e0.dose_unit_rx,
           e0.form_val_disp,
           e0.form_unit_disp,
           e0.route
          FROM e0
       UNION ALL
        SELECT 'propofol'::text AS study,
           p0.row_id,
           p0.subject_id,
           p0.hadm_id,
           p0.icustay_id,
           p0.startdate,
           p0.enddate,
           p0.drug_type,
           p0.drug,
           p0.drug_name_poe,
           p0.drug_name_generic,
           p0.formulary_drug_cd,
           p0.gsn,
           p0.ndc,
           p0.prod_strength,
           p0.dose_val_rx,
           p0.dose_unit_rx,
           p0.form_val_disp,
           p0.form_unit_disp,
           p0.route
          FROM p0
       ), v0 AS (
        SELECT icustays.hadm_id,
           vent.icustay_id,
           vent.starttime AS vent_start,
           vent.endtime AS vent_end
          FROM mimiciii.ventdurations vent
            JOIN mimiciii.icustays ON vent.icustay_id = icustays.icustay_id
         WHERE vent.duration_hours > 0::double precision
       ), e0p0v0 AS (
        SELECT e0p0.study,
           e0p0.row_id,
           e0p0.subject_id,
           e0p0.hadm_id,
           e0p0.icustay_id,
           e0p0.startdate,
           e0p0.enddate,
           e0p0.drug_type,
           e0p0.drug,
           e0p0.drug_name_poe,
           e0p0.drug_name_generic,
           e0p0.formulary_drug_cd,
           e0p0.gsn,
           e0p0.ndc,
           e0p0.prod_strength,
           e0p0.dose_val_rx,
           e0p0.dose_unit_rx,
           e0p0.form_val_disp,
           e0p0.form_unit_disp,
           e0p0.route,
           sofa.sofa
          FROM e0p0
            JOIN mimiciii.sofa ON e0p0.icustay_id = sofa.icustay_id
         WHERE (e0p0.hadm_id IN ( SELECT v0.hadm_id
                  FROM v0))
       ), e1 AS (
        SELECT e_1.study,
           e_1.hadm_id,
           i.icustay_id,
           i.intime,
           i.outtime,
           i.los,
           e_1.sofa,
           e_1.startdate,
           e_1.enddate,
           e_1.dose_val_rx,
               CASE
                   WHEN i.intime < e_1.startdate AND e_1.enddate < i.outtime THEN 1
                   ELSE 0
               END AS icu_flag
          FROM e0p0v0 e_1
            JOIN mimiciii.icustays i ON e_1.icustay_id = i.icustay_id
         ORDER BY e_1.hadm_id, i.icustay_id, e_1.startdate
       ), e2 AS (
        SELECT a.subject_id,
           e_1.study,
           e_1.hadm_id,
           e_1.icustay_id,
           e_1.intime,
           e_1.outtime,
           e_1.los,
           e_1.sofa,
           e_1.startdate,
           e_1.enddate,
           e_1.dose_val_rx,
           e_1.icu_flag,
           a.admittime,
           a.dischtime,
           a.deathtime,
           (a.deathtime::date - e_1.startdate::date) < 31 AS mortality_30,
           a.deathtime >= a.admittime AND a.deathtime <= a.dischtime AS hosp_death,
           a.ethnicity
          FROM e1 e_1
            JOIN mimiciii.admissions a ON e_1.hadm_id = a.hadm_id
         ORDER BY e_1.hadm_id, a.admittime, e_1.startdate
       ), e3 AS (
        SELECT e_1.subject_id,
           e_1.study,
           e_1.hadm_id,
           e_1.icustay_id,
           e_1.intime,
           e_1.outtime,
           e_1.los,
           e_1.sofa,
           e_1.startdate,
           e_1.enddate,
           e_1.dose_val_rx,
           e_1.icu_flag,
           e_1.admittime,
           e_1.dischtime,
           e_1.deathtime,
           e_1.mortality_30,
           e_1.hosp_death,
           e_1.ethnicity,
           p.gender,
           (e_1.admittime::date - p.dob::date)::numeric / 365.242 AS age,
           p.dob
          FROM e2 e_1
            JOIN mimiciii.patients p ON e_1.subject_id = p.subject_id
       ), e4 AS (
        SELECT e_1.subject_id,
           e_1.study,
           e_1.hadm_id,
           e_1.icustay_id,
           e_1.intime,
           e_1.outtime,
           e_1.los,
           e_1.sofa,
           e_1.startdate,
           e_1.enddate,
           e_1.dose_val_rx,
           e_1.icu_flag,
           e_1.admittime,
           e_1.dischtime,
           e_1.deathtime,
           e_1.mortality_30,
           e_1.hosp_death,
           e_1.ethnicity,
           e_1.gender,
           e_1.age,
           e_1.dob,
               CASE
                   WHEN (e_1.hadm_id IN ( SELECT e00.hadm_id
                      FROM e00)) THEN 1
                   ELSE 0
               END AS adrenal_insufficiency,
           h_1.congestive_heart_failure,
           h_1.cardiac_arrhythmias,
           h_1.valvular_disease,
           h_1.pulmonary_circulation,
           h_1.peripheral_vascular,
           h_1.hypertension,
           h_1.paralysis,
           h_1.other_neurological,
           h_1.chronic_pulmonary,
           h_1.diabetes_uncomplicated,
           h_1.diabetes_complicated,
           h_1.hypothyroidism,
           h_1.renal_failure,
           h_1.liver_disease,
           h_1.peptic_ulcer,
           h_1.aids,
           h_1.lymphoma,
           h_1.metastatic_cancer,
           h_1.solid_tumor,
           h_1.rheumatoid_arthritis,
           h_1.coagulopathy,
           h_1.obesity,
           h_1.weight_loss,
           h_1.fluid_electrolyte,
           h_1.blood_loss_anemia,
           h_1.deficiency_anemias,
           h_1.alcohol_abuse,
           h_1.drug_abuse,
           h_1.psychoses,
           h_1.depression
          FROM e3 e_1
            JOIN mimiciii.elixhauser_ahrq_no_drg_all_icd h_1 ON e_1.hadm_id = h_1.hadm_id
       )
SELECT DISTINCT ON (e.hadm_id) e.hadm_id AS id,
   e.subject_id,
   e.study,
   e.hadm_id,
   e.icustay_id,
   e.intime,
   e.outtime,
   e.los,
   e.sofa,
   e.startdate,
   e.enddate,
   e.dose_val_rx,
   e.icu_flag,
   e.admittime,
   e.dischtime,
   e.deathtime,
   e.mortality_30,
   e.hosp_death,
   e.ethnicity,
   e.gender,
   e.age,
   e.dob,
   e.adrenal_insufficiency,
   e.congestive_heart_failure,
   e.cardiac_arrhythmias,
   e.valvular_disease,
   e.pulmonary_circulation,
   e.peripheral_vascular,
   e.hypertension,
   e.paralysis,
   e.other_neurological,
   e.chronic_pulmonary,
   e.diabetes_uncomplicated,
   e.diabetes_complicated,
   e.hypothyroidism,
   e.renal_failure,
   e.liver_disease,
   e.peptic_ulcer,
   e.aids,
   e.lymphoma,
   e.metastatic_cancer,
   e.solid_tumor,
   e.rheumatoid_arthritis,
   e.coagulopathy,
   e.obesity,
   e.weight_loss,
   e.fluid_electrolyte,
   e.blood_loss_anemia,
   e.deficiency_anemias,
   e.alcohol_abuse,
   e.drug_abuse,
   e.psychoses,
   e.depression,
   h.height_first / 100::numeric AS ht,
   h.weight_first AS wt,
       CASE
           WHEN h.height_first IS NULL OR h.weight_first IS NULL 
		   	THEN NULL::numeric
           ELSE h.weight_first / (h.height_first / 100::numeric * (h.height_first / 100::numeric))
       END AS bmi
  FROM e4 e
    JOIN mimiciii.heightweight h ON e.icustay_id = h.icustay_id;
	
	
-- examining previous hospitalization
--
drop materialized view if exists etomidate._prev_discharge;
create materialized view etomidate._prev_discharge AS 
WITH t1 AS (
    SELECT _etomidate_propofol_cohort.hadm_id,
       _etomidate_propofol_cohort.admittime AS cohortstart,
       _etomidate_propofol_cohort.dischtime AS cohortend
      FROM etomidate._etomidate_propofol_cohort
   ), 
   
   t2 AS (
    SELECT ma.subject_id,
       t1.hadm_id,
       t1.cohortstart,
       t1.cohortend
      FROM mimiciii.admissions ma
        JOIN t1 ON ma.hadm_id = t1.hadm_id
   )

SELECT t2.subject_id,
	t2.hadm_id,
	t2.cohortstart,
	t2.cohortend,
	mad.dischtime AS prev_disch
FROM t2
LEFT JOIN mimiciii.admissions mad 
ON t2.subject_id = mad.subject_id 
	AND date_part('day'::text, t2.cohortstart - mad.dischtime) >= 0::double precision 
	AND date_part('day'::text, t2.cohortstart - mad.dischtime) <= 7::double precision;
		

-- examining previous hx of suppressors
--
drop materialized view if exists etomidate._patients_suppressors;
create materialized view etomidate._patients_suppressors AS 	
WITH t1 AS (
	SELECT e.study,
		e.hadm_id,
		e.icustay_id,
		e.startdate,
		p.startdate AS suppressors
	FROM etomidate._etomidate_propofol_cohort e
	LEFT JOIN etomidate._suppressors p 
	ON e.hadm_id = p.hadm_id 
		AND date_part('day'::text, e.startdate - p.startdate) >= 1::double precision 
		AND date_part('day'::text, e.startdate - p.startdate) <= 30::double precision
	), 
	
	t2 AS (
	SELECT t1.study,
		t1.hadm_id,
		t1.icustay_id,
		t1.startdate,
	CASE
		WHEN t1.suppressors IS NOT NULL THEN 1
		ELSE 0 END AS suppressors
	FROM t1
	)
	
SELECT t2.study,
	t2.hadm_id,
	t2.icustay_id,
	max(t2.suppressors) AS suppressors
FROM t2
GROUP BY t2.study, t2.hadm_id, t2.icustay_id
ORDER BY t2.hadm_id;


-- combining outcome of etomidate cohort
--
drop materialized view if exists etomidate._etomidate_outcome;
create materialized view etomidate._etomidate_outcome AS 	
WITH e0 AS (
        SELECT DISTINCT _etomidate_cohort.hadm_id
          FROM etomidate._etomidate_cohort
       		), 
			
	 cv AS (
        SELECT DISTINCT diagnoses_icd.hadm_id
          FROM mimiciii.diagnoses_icd
         WHERE diagnoses_icd.icd9_code::text = ANY (ARRAY['4582'::character varying, '45821'::character varying, '45829'::character varying, '9971'::character varying, '9972'::character varying, '9980'::character varying, '78550'::character varying, '7855'::character varying, '78551'::character varying, '78559'::character varying, '27652'::character varying, '4233'::character varying, '9980'::character varying, '458'::character varying, '27652'::character varying]::text[])
       ), 
	   
	   infc AS (
        SELECT DISTINCT diagnoses_icd.hadm_id
          FROM mimiciii.diagnoses_icd
         WHERE (diagnoses_icd.icd9_code::text = ANY (ARRAY['51901'::character varying::text, '53641'::character varying::text, '53086'::character varying::text, '99762'::character varying::text, '9985'::character varying::text, '99851'::character varying::text, '99859'::character varying::text, '9993'::character varying::text, '56961'::character varying::text, '99931'::character varying::text, '99731'::character varying::text])) OR diagnoses_icd.icd9_code::text ~* '^9966\d{1}'::text
       )

SELECT e0.hadm_id,
       CASE
           WHEN (e0.hadm_id IN ( SELECT _etomidate_cohort.hadm_id
              FROM etomidate._etomidate_cohort
             WHERE _etomidate_cohort.mortality_30 = true)) THEN 1
           ELSE 0
       END AS mortality,
       CASE
           WHEN (e0.hadm_id IN ( SELECT cv.hadm_id
              FROM cv)) THEN 1
           ELSE 0
       END AS cv_morbidity,
       CASE
           WHEN (e0.hadm_id IN ( SELECT infc.hadm_id
              FROM infc)) THEN 1
           ELSE 0
       END AS infectious_morbidity
FROM e0;	 


-- combining outcome of propofol cohort
--
drop materialized view if exists etomidate._propofol_outcome;
create materialized view etomidate._propofol_outcome AS 		 
    WITH e0 AS (
            SELECT DISTINCT _propofol_cohort.hadm_id
              FROM etomidate._propofol_cohort
           ), cv AS (
            SELECT DISTINCT diagnoses_icd.hadm_id
              FROM mimiciii.diagnoses_icd
             WHERE diagnoses_icd.icd9_code::text = ANY (ARRAY['4582'::character varying, '45821'::character varying, '45829'::character varying, '9971'::character varying, '9972'::character varying, '9980'::character varying, '78550'::character varying, '7855'::character varying, '78551'::character varying, '78559'::character varying, '27652'::character varying, '4233'::character varying, '9980'::character varying, '458'::character varying, '27652'::character varying]::text[])
           ), infc AS (
            SELECT DISTINCT diagnoses_icd.hadm_id
              FROM mimiciii.diagnoses_icd
             WHERE (diagnoses_icd.icd9_code::text = ANY (ARRAY['51901'::character varying::text, '53641'::character varying::text, '53086'::character varying::text, '99762'::character varying::text, '9985'::character varying::text, '99851'::character varying::text, '99859'::character varying::text, '9993'::character varying::text, '56961'::character varying::text, '99931'::character varying::text, '99731'::character varying::text])) OR diagnoses_icd.icd9_code::text ~* '^9966\d{1}'::text
           )
    SELECT e0.hadm_id,
           CASE
               WHEN (e0.hadm_id IN ( SELECT _propofol_cohort.hadm_id
                  FROM etomidate._propofol_cohort
                 WHERE _propofol_cohort.mortality_30 = true)) THEN 1
               ELSE 0
           END AS mortality,
           CASE
               WHEN (e0.hadm_id IN ( SELECT cv.hadm_id
                  FROM cv)) THEN 1
               ELSE 0
           END AS cv_morbidity,
           CASE
               WHEN (e0.hadm_id IN ( SELECT infc.hadm_id
                  FROM infc)) THEN 1
               ELSE 0
           END AS infectious_morbidity
      FROM e0;
	  

-- examining whether steroids were replaced or not
--
drop materialized view if exists etomidate._replacement;
create materialized view etomidate._replacement AS 	
WITH both_simple AS (
        SELECT 'etomidate'::text AS study,
           _etomidate_cohort.hadm_id,
           _etomidate_cohort.startdate::date AS day
          FROM etomidate._etomidate_cohort
       UNION ALL
        SELECT 'propofol'::text AS study,
           _propofol_cohort.hadm_id,
           _propofol_cohort.startdate::date AS day
          FROM etomidate._propofol_cohort
       ), _cortisol AS (
        SELECT labevents.hadm_id,
           labevents.charttime::date AS c_day,
           labevents.charttime::time without time zone AS c_time,
           labevents.value AS cortisol
          FROM mimiciii.labevents
         WHERE labevents.itemid = 50909
       ), _supplement AS (
        SELECT prescriptions.row_id,
           prescriptions.subject_id,
           prescriptions.hadm_id,
           prescriptions.icustay_id,
           prescriptions.startdate,
           prescriptions.enddate,
           prescriptions.drug_type,
           prescriptions.drug,
           prescriptions.drug_name_poe,
           prescriptions.drug_name_generic,
           prescriptions.formulary_drug_cd,
           prescriptions.gsn,
           prescriptions.ndc,
           prescriptions.prod_strength,
           prescriptions.dose_val_rx,
           prescriptions.dose_unit_rx,
           prescriptions.form_val_disp,
           prescriptions.form_unit_disp,
           prescriptions.route
          FROM mimiciii.prescriptions
         WHERE (prescriptions.drug_name_generic::text ~* 'dexamethason'::text OR prescriptions.drug_name_generic::text ~* 'cort'::text OR prescriptions.drug_name_generic::text ~* 'predni'::text) AND (prescriptions.route::text = ANY (ARRAY['IV'::character varying::text, 'ORAL'::character varying::text, 'PO/NG'::character varying::text, 'PO'::character varying::text, 'NG'::character varying::text]))
       ), _with_cortisol AS (
        SELECT ep.study,
           ep.hadm_id,
           ep.day,
           c.c_day,
           c.c_time,
           c.cortisol,
               CASE
                   WHEN (ep.hadm_id IN ( SELECT _supplement.hadm_id
                      FROM _supplement)) AND c.c_day < (( SELECT min(_supplement.startdate)::date AS min
                      FROM _supplement
                     WHERE _supplement.hadm_id = ep.hadm_id)) THEN 1
                   ELSE 0
               END AS replacement,
               CASE
                   WHEN c.c_day < ep.day THEN 'earlier'::text
                   WHEN c.c_day = ep.day THEN 'same'::text
                   ELSE 'after'::text
               END AS before,
               CASE
                   WHEN c.c_time <= '12:00:00'::time without time zone THEN 'am'::text
                   ELSE 'pm'::text
               END AS ampm
          FROM both_simple ep
            LEFT JOIN _cortisol c ON ep.hadm_id = c.hadm_id
       ), _repla AS (
        SELECT _with_cortisol.hadm_id,
           _with_cortisol.study,
           max(_with_cortisol.replacement) OVER w AS replacement,
           row_number() OVER w AS seq_no
          FROM _with_cortisol
         WINDOW w AS (PARTITION BY _with_cortisol.hadm_id)
       )
SELECT _repla.hadm_id,
   _repla.study,
   _repla.replacement,
   _repla.seq_no
  FROM _repla
 WHERE _repla.seq_no = 1;
 

-- getting cortisol concentration
--
drop materialized view if exists etomidate._cortisol_full;
create materialized view etomidate._cortisol_full AS 	
WITH both_simple AS (
        SELECT 'etomidate'::text AS study,
           _etomidate_cohort.hadm_id,
           _etomidate_cohort.startdate::date AS day
          FROM etomidate._etomidate_cohort
       UNION ALL
        SELECT 'propofol'::text AS study,
           _propofol_cohort.hadm_id,
           _propofol_cohort.startdate::date AS day
          FROM etomidate._propofol_cohort
       ), _cortisol AS (
        SELECT labevents.hadm_id,
           labevents.charttime::date AS c_day,
           labevents.charttime::time without time zone AS c_time,
           labevents.value AS cortisol
          FROM mimiciii.labevents
         WHERE labevents.itemid = 50909
       ), _supplement AS (
        SELECT prescriptions.row_id,
           prescriptions.subject_id,
           prescriptions.hadm_id,
           prescriptions.icustay_id,
           prescriptions.startdate,
           prescriptions.enddate,
           prescriptions.drug_type,
           prescriptions.drug,
           prescriptions.drug_name_poe,
           prescriptions.drug_name_generic,
           prescriptions.formulary_drug_cd,
           prescriptions.gsn,
           prescriptions.ndc,
           prescriptions.prod_strength,
           prescriptions.dose_val_rx,
           prescriptions.dose_unit_rx,
           prescriptions.form_val_disp,
           prescriptions.form_unit_disp,
           prescriptions.route
          FROM mimiciii.prescriptions
         WHERE prescriptions.drug_name_generic::text ~* 'cort'::text AND (prescriptions.route::text = ANY (ARRAY['IV'::character varying, 'ORAL'::character varying, 'PO/NG'::character varying, 'PO'::character varying, 'NG'::character varying]::text[]))
       ), _with_cortisol AS (
        SELECT ep.study,
           ep.hadm_id,
           ep.day,
           c.c_day,
           c.c_time,
           c.cortisol,
               CASE
                   WHEN (ep.hadm_id IN ( SELECT _supplement.hadm_id
                      FROM _supplement)) AND c.c_day < (( SELECT min(_supplement.startdate)::date AS min
                      FROM _supplement
                     WHERE _supplement.hadm_id = ep.hadm_id)) THEN 1
                   ELSE 0
               END AS replacement,
               CASE
                   WHEN c.c_day < ep.day THEN 'earlier'::text
                   WHEN c.c_day = ep.day THEN 'same'::text
                   ELSE 'after'::text
               END AS before,
               CASE
                   WHEN c.c_time <= '12:00:00'::time without time zone THEN 'am'::text
                   ELSE 'pm'::text
               END AS ampm
          FROM both_simple ep
            LEFT JOIN _cortisol c ON ep.hadm_id = c.hadm_id
       )
SELECT _with_cortisol.study,
   _with_cortisol.hadm_id,
   _with_cortisol.day,
   _with_cortisol.c_day,
   _with_cortisol.c_time,
   _with_cortisol.cortisol,
   _with_cortisol.replacement,
   _with_cortisol.before,
   _with_cortisol.ampm
  FROM _with_cortisol;
 
  
-- getting BPS
-- 
drop materialized view if exists etomidate._bpss;
create materialized view etomidate._bpss AS 	
WITH both_simple AS (
        SELECT 'etomidate'::text AS study,
           _etomidate_cohort.hadm_id,
           _etomidate_cohort.startdate
          FROM etomidate._etomidate_cohort
       UNION ALL
        SELECT 'propofol'::text AS study,
           _propofol_cohort.hadm_id,
           _propofol_cohort.startdate
          FROM etomidate._propofol_cohort
       ), _bps AS (
        SELECT e.study,
           e.hadm_id,
           c.icustay_id,
           e.startdate,
           c.charttime,
               CASE
                   WHEN c.itemid = ANY (ARRAY[6, 51, 442, 455, 1449, 3313, 3315, 3317, 3321, 220050, 220179, 224167, 225309]) THEN 'sbp'::text
                   ELSE 'mbp'::text
               END AS bp,
           c.valuenum
          FROM both_simple e
            JOIN mimiciii.chartevents c ON e.hadm_id = c.hadm_id
         WHERE (c.itemid = ANY (ARRAY[6, 51, 442, 455, 1449, 3313, 3315, 3317, 3321, 220050, 220179, 224167, 225309, 52, 438, 443, 456, 1321, 2309, 2353, 2369, 2544, 2732, 2770, 2974, 3067, 3312, 3314, 3316, 3320, 3322, 5680, 5731, 6399, 6653, 6702, 7618, 7620, 7622, 220052, 220181, 225312])) AND e.startdate::date = c.charttime::date
       ), bps_summary AS (
        SELECT _bps.study,
           _bps.hadm_id,
           _bps.icustay_id,
           _bps.charttime,
           _bps.bp,
           min(_bps.valuenum) OVER w AS min_bp,
           avg(_bps.valuenum) OVER w AS avg_bp,
           max(_bps.valuenum) OVER w AS max_bp,
           row_number() OVER w AS seq_no
          FROM _bps
         WINDOW w AS (PARTITION BY _bps.icustay_id, _bps.bp)
       )
SELECT bps_summary.study,
   bps_summary.hadm_id,
   bps_summary.icustay_id,
   bps_summary.bp,
   bps_summary.min_bp,
   bps_summary.avg_bp,
   bps_summary.max_bp
  FROM bps_summary
 WHERE bps_summary.seq_no = 1;



-- admissions before and after mid-2008 
--
drop materialized view if exists etomidate._admissions_half;
create materialized view etomidate._admissions_half AS 
SELECT admissions.hadm_id,
   admissions.admittime,
       CASE
           WHEN (admissions.hadm_id IN ( SELECT DISTINCT inputevents_cv.hadm_id
              FROM mimiciii.inputevents_cv)) THEN '1st'::text
           ELSE '2nd'::text
       END AS a_half
FROM mimiciii.admissions;


-- listing all morbidities (NOT included in the article)
--
drop materialized view if exists etomidate._all_morbidity;
create materialized view etomidate._all_morbidity AS 
 SELECT '458.2'::text AS icd9_code,
    'Iatrogenic hypotension'::text AS diagnoses,
    etomidate._get_count_for_icd('4582'::character varying) AS "all",
    etomidate._get_count_for_icd('4582'::character varying, 1) AS etomidate
UNION ALL
 SELECT '458.21'::text AS icd9_code,
    'Hypotension - hemodialysis associated'::text AS diagnoses,
    etomidate._get_count_for_icd('45821'::character varying) AS "all",
    etomidate._get_count_for_icd('45821'::character varying, 1) AS etomidate
UNION ALL
 SELECT '458.29'::text AS icd9_code,
    'Iatrogenic hypotension NEC'::text AS diagnoses,
    etomidate._get_count_for_icd('45829'::character varying) AS "all",
    etomidate._get_count_for_icd('45829'::character varying, 1) AS etomidate
UNION ALL
 SELECT '997.1'::text AS icd9_code,
    'Cardiac arrest/insufficiency'::text AS diagnoses,
    etomidate._get_count_for_icd('9971'::character varying) AS "all",
    etomidate._get_count_for_icd('9971'::character varying, 1) AS etomidate
UNION ALL
 SELECT '997.2'::text AS icd9_code,
    'Peripheral vascular complications'::text AS diagnoses,
    etomidate._get_count_for_icd('9972'::character varying) AS "all",
    etomidate._get_count_for_icd('9972'::character varying, 1) AS etomidate
UNION ALL
 SELECT '998.0'::text AS icd9_code,
    'Postoperative shock'::text AS diagnoses,
    etomidate._get_count_for_icd('9980'::character varying) AS "all",
    etomidate._get_count_for_icd('9980'::character varying, 1) AS etomidate
UNION ALL
 SELECT '------'::text AS icd9_code,
    '-----------------------'::text AS diagnoses,
    NULL::bigint AS "all",
    NULL::bigint AS etomidate
UNION ALL
 SELECT '519.01'::text AS icd9_code,
    'Infection of tracheostomy'::text AS diagnoses,
    etomidate._get_count_for_icd('51901'::character varying) AS "all",
    etomidate._get_count_for_icd('51901'::character varying, 1) AS etomidate
UNION ALL
 SELECT '536.41'::text AS icd9_code,
    'Infection of gastrostomy'::text AS diagnoses,
    etomidate._get_count_for_icd('53641'::character varying) AS "all",
    etomidate._get_count_for_icd('53641'::character varying, 1) AS etomidate
UNION ALL
 SELECT '530.86'::text AS icd9_code,
    'Infection of esophagostomy'::text AS diagnoses,
    etomidate._get_count_for_icd('53086'::character varying) AS "all",
    etomidate._get_count_for_icd('53086'::character varying, 1) AS etomidate
UNION ALL
 SELECT '997.62'::text AS icd9_code,
    'Amputation stump complication: infection'::text AS diagnoses,
    etomidate._get_count_for_icd('99762'::character varying) AS "all",
    etomidate._get_count_for_icd('99762'::character varying, 1) AS etomidate
UNION ALL
 SELECT '998.5'::text AS icd9_code,
    'Postoperative infection'::text AS diagnoses,
    etomidate._get_count_for_icd('9985'::character varying) AS "all",
    etomidate._get_count_for_icd('9985'::character varying, 1) AS etomidate
UNION ALL
 SELECT '998.51'::text AS icd9_code,
    'Infected postoperative seroma'::text AS diagnoses,
    etomidate._get_count_for_icd('99851'::character varying) AS "all",
    etomidate._get_count_for_icd('99851'::character varying, 1) AS etomidate
UNION ALL
 SELECT '998.59'::text AS icd9_code,
    'Other postoperative infection'::text AS diagnoses,
    etomidate._get_count_for_icd('99859'::character varying) AS "all",
    etomidate._get_count_for_icd('99859'::character varying, 1) AS etomidate
UNION ALL
 SELECT '999.3'::text AS icd9_code,
    'Other infection'::text AS diagnoses,
    etomidate._get_count_for_icd('9993'::character varying) AS "all",
    etomidate._get_count_for_icd('9993'::character varying, 1) AS etomidate
UNION ALL
 SELECT '569.61'::text AS icd9_code,
    'Infection of gastrostomy or enterostomy'::text AS diagnoses,
    etomidate._get_count_for_icd('9993'::character varying) AS "all",
    etomidate._get_count_for_icd('9993'::character varying, 1) AS etomidate
UNION ALL
 SELECT '996.6'::text AS icd9_code,
    'Infection and inflammatory reaction due to ...'::text AS diagnoses,
    etomidate._get_count_for_icd('9966'::character varying) AS "all",
    etomidate._get_count_for_icd('9966'::character varying, 1) AS etomidate
UNION ALL
 SELECT '996.6x'::text AS icd9_code,
    'Several infectious complications (x = 0 ~ 9)'::text AS diagnoses,
    ( SELECT count(*) AS count
           FROM mimiciii.diagnoses_icd md
          WHERE regexp_match(md.icd9_code::text, '^9966[0-9]'::text) IS NOT NULL) AS "all",
    ( SELECT count(*) AS count
           FROM mimiciii.diagnoses_icd md
          WHERE md.icd9_code::text ~* '^9966[0-9]'::text AND (md.hadm_id IN ( SELECT _admissions.hadm_id
                   FROM etomidate._admissions))) AS etomidate
UNION ALL
 SELECT '999.31'::text AS icd9_code,
    'Infection d/t CV catheter'::text AS diagnoses,
    etomidate._get_count_for_icd('99931'::character varying) AS "all",
    etomidate._get_count_for_icd('99931'::character varying, 1) AS etomidate
UNION ALL
 SELECT '997.31'::text AS icd9_code,
    'Ventilator-associated infection'::text AS diagnoses,
    etomidate._get_count_for_icd('99731'::character varying) AS "all",
    etomidate._get_count_for_icd('99731'::character varying, 1) AS etomidate
UNION ALL
 SELECT '------'::text AS icd9_code,
    '-----------------------'::text AS diagnoses,
    NULL::bigint AS "all",
    NULL::bigint AS etomidate
UNION ALL
 SELECT 'Total'::text AS icd9_code,
    'above morbidity'::text AS diagnoses,
    ( WITH temp AS (
                 SELECT DISTINCT diagnoses_icd.hadm_id
                   FROM mimiciii.diagnoses_icd
                  WHERE ((diagnoses_icd.icd9_code::text = ANY (ARRAY['4582'::character varying, '45821'::character varying, '45829'::character varying, '9971'::character varying, '9972'::character varying, '9980'::character varying, '51901'::character varying, '53641'::character varying, '53086'::character varying, '99762'::character varying, '9985'::character varying, '99851'::character varying, '99859'::character varying, '9993'::character varying, '9966'::character varying, '56961'::character varying, '99931'::character varying, '99731'::character varying, '9966'::character varying]::text[])) OR regexp_match(diagnoses_icd.icd9_code::text, '^9966[0-9]'::text) IS NOT NULL) AND (diagnoses_icd.hadm_id IN ( SELECT _admissions.hadm_id
                           FROM mimiciii._admissions))
                )
         SELECT count(*) AS count
           FROM temp) AS "all",
    ( WITH temp AS (
                 SELECT DISTINCT diagnoses_icd.hadm_id
                   FROM mimiciii.diagnoses_icd
                  WHERE ((diagnoses_icd.icd9_code::text = ANY (ARRAY['4582'::character varying, '45821'::character varying, '45829'::character varying, '9971'::character varying, '9972'::character varying, '9980'::character varying, '51901'::character varying, '53641'::character varying, '53086'::character varying, '99762'::character varying, '9985'::character varying, '99851'::character varying, '99859'::character varying, '9993'::character varying, '9966'::character varying, '56961'::character varying, '99931'::character varying, '99731'::character varying, '9966'::character varying]::text[])) OR regexp_match(diagnoses_icd.icd9_code::text, '^9966[0-9]'::text) IS NOT NULL) AND (diagnoses_icd.hadm_id IN ( SELECT _admissions.hadm_id
                           FROM mimiciii._admissions))
                )
         SELECT count(*) AS count
           FROM ( SELECT temp.hadm_id
                   FROM temp
                INTERSECT
                 SELECT _admissions.hadm_id
                   FROM etomidate._admissions) x) AS etomidate
UNION ALL
 SELECT 'Total death'::text AS icd9_code,
    'within 30 days'::text AS diagnoses,
    NULL::bigint AS "all",
    ( WITH temp AS (
                 SELECT DISTINCT _admissions.hadm_id
                   FROM etomidate._admissions
                  WHERE _admissions.death_hosp AND _admissions.post_etomidate < 31
                )
         SELECT count(*) AS count
           FROM temp) AS etomidate;
		  
    	