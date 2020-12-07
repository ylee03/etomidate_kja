#-----------------------------------------------------
#
# R Script for the investigation:
# EFFECT OF ETOMIDATE ON HOSPITAL MORTALITY
# ver. 2020jul08ev001
#
# Developed by Younsuk Lee et al. (ylee@dgu.ac.kr)
# Distributed under the terms of the Creative Commons Attribution license (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted use, distribution, and reproduction in any medium, provided the original work is properly cited.
#
# Date: Jun, 2019 -- Mar, 2020 completion 
#		-- May--Jul, 2020 tidying
# 		
#
# Version history:
#	7) Jul-03-2020: Some functions were added.
#	6) Jul-01-2020: Tidying
#	5) Mar-15-2020: 48-Hr version
#	4) Mar-14-2020: Combining admission type
#	3) Mar-01-2020: Considering 24-Hr survival 
#	2) Feb-28-2020: Eliminating both extremes of body measurements
#	1) Dec-24-2020: Dose-Mortality relationship (failed)
#	0) Oct, 2019:   Transferred from the KSA-2019 version
#	*) Jul-21-2019: First edition 
#-----------------------------------------------------
#
# Prerequisites:
#	- MIMIC-III at the PostgreSQL server, schema `mimiciii'
#	- In the MIMIC, concept tables made with MIMIC-III tools by MIT-LCP
#	- Dedicated schema named `mimic.etomidate,' 
# 		 pre-built with `buildSchemaEtomidate.sql'
#	- Connection between R and SQL via RPostgreSQL
# 	- Required R Packages:
#		RPostgreSQL (connection to MIMIC DB)
#		magrittr (preference)
#		MatchIt (propensity score matching)
#		effsize (Cohen's d, 1988)
#		MASS (stepwise variable selection using AIC)
#		xtable (latex decoration)
#
#-----------------------------------------------------
#-- delete '+' sign in the continued command line
options(continue = " ")

#-- loading packages
library(magrittr)
library(RPostgreSQL)
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#------------------ SQL CONNECTION -------------------
#-----------------------------------------------------
#--------------------<<<hidden>>>---------------------
#-----------------------------------------------------
source("get_connection.R")
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#------------------ MY FUNCTIONS ---------------------
#-----------------------------------------------------
#---<defining some super-simple useful functions>-----
#-----------------------------------------------------
##-- 
##	1) displaying odds ratio and CI in a row
getOdds <- function(t) 
	c(t$estimate, t$conf.int)
##  1') calcalating odds ratio [CI] with the conditional MLE
odds.ratio <- function(...) {
	f <- fisher.test(...)
	res <- c(f$estimate, f$conf.int)
	names(res) <- c("conditional MLE", "LWR", "UPR")
	
	p <- list(...)
	attr(res, "conf.level") <- ifelse( !is.null(p$conf.level), p$conf.level, 0.95 )
	return ( res )
}
## 	2) tabulating 2 x 2 tables
## 		  having columns of TRUE first and FALSE 
##  *** DANGER ***
##	      DO NOT DEFINE THIS FUNCTION
##		  UNLESS YOU UNDERSTAND ITS MEANING AND 
##			PREDICT THE RESULTS.
table <- function(...) {
	p <- list(...)
	if (length (p) != 2) return( base::table(...) )
	t <- base::table(p[[1]], p[[2]])
	if (colnames(t)[1] != "FALSE") return ( base::table(...) )
	return (t[, 2:1])
} # *** END OF DANGER ***
## 3) querying a table from a SQL connection
getTable <- selectFrom <- function( 
	schema = "mimiciii", 
	from, 
	select = "*", 
	where = NA) {
		# 
	require(magrittr)
	require(RPostgreSQL)
	query_state <- "SELECT @1 FROM @2"	
	query_state %<>% gsub("@1", select, .)
	query_state %<>% gsub("@2", schema %>% paste(from, sep = "."), .)
	if ( !is.na(where) ) query_state %<>% paste("WHERE", where)
	cat(query_state, "\n")
	return ( dbGetQuery(con, query_state) )
}
## 4) convert dose string having ranges into max value only string
valueSplit <- function(x) {
	require(magrittr)
	s <- strsplit(x, split = "-")  %>% unlist
	return(
		list(value0 = x,
			value1 = ifelse( length(s) == 1, s, s[2] ) )
	)
}
## 5) finding the nearest value to a scalar 'a' 
##-------- among a vector 'v'
##-------- if the nearest values exist,
##-------- one median value returns
pickCloser <- chooseNearest <- function(a, x) {
	if ( !(length(a) == 1 & length(x) >= 1) ) return("Error")
	require(magrittr)
	x <- x[ !is.na(x) ]
	if ( length(x) < 1 ) return ( a )
	return ( median( x[ which( abs(x - a) == min( abs( x - a)))] ))
}
## 6) calc unbiased Cohen's d
unbiased_cohen_d <- function ( x1, x2, normality = TRUE, alpha = 0.05 ) {
	
	n1 <- length(x1)
	n2 <- length(x2)
	t <- t.test(x1, x2)$statistic
	d <- t * sqrt( 1/n1 + 1/n2)
	df <- n1 + n2 - 2
	d_unb <- (1 - 3 / (4 * df - 1)) * d
	

	v <- ( (n1 + n2) / (n1*n2) + d^2 / (2 * (n1 + n2)) )
	se <- sqrt(v)
	
	qlimit <- if (normality) qnorm ( 1 - alpha / 2 ) else qt ( 1 - alpha / 2, df = df ) #
	ci <- d_unb + c(-qlimit * se, qlimit * se)
	attr(ci, "conf.level") <- paste0((1-alpha)*100, "%") #

	res <- list("Cohen's d" = unname(d), 
		"Unbiased d" = unname(d_unb), 
		"ci" = unname(ci))
	return( res )
}

## 7) display noticeable outputs
disp <- function(x, head = NA, max = 70) {
	
	n <- nchar(head)
	nstar <- ( max - n ) %/% 3
	star <- paste(paste(rep("*", nstar), collapse = ""), 
		head, 
		paste(rep("*", nstar * 2), collapse = ""), collapse = "") #
	
	cat(star, "\n")
	print(x)
	cat(paste(rep("*", max), collapse = ""), "\n" )	
	# return with nothing
}


#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-------------- DATA PREPARATION ---------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

#-- querying bulk data for before-matching cohorts
b4 <- getTable(schema = "ETOMIDATE", from = "_ETOMIDATE_PROPOFOL_COHORT")
#-- intermediate storage 
# b4_0 <- b4; save(b4_0, file = "b4_0.RData")
# load("b4_0.RData")
dim(b4)
table(b4$study)
#- converting candidate factors having 1/0 values into factors
cova_names <- c(	
	"adrenal_insufficiency", "congestive_heart_failure",
	"cardiac_arrhythmias", "valvular_disease",        
	"pulmonary_circulation", "peripheral_vascular",     
	"hypertension", "paralysis",
	"other_neurological", "chronic_pulmonary",
	"diabetes_uncomplicated", "diabetes_complicated",
	"hypothyroidism", "renal_failure",
	"liver_disease", "peptic_ulcer",
	"aids", "lymphoma",
	"metastatic_cancer", "solid_tumor",
	"rheumatoid_arthritis", "coagulopathy",
	"obesity", "weight_loss",
	"fluid_electrolyte", "blood_loss_anemia",
	"deficiency_anemias", "alcohol_abuse",
	"drug_abuse", "psychoses",
	"depression"
	)	
for (i in cova_names) b4[, i] <- factor(b4[, i])	
	
#-- age grouping
b4$age1 <- b4$age < 66
b4$age2 <- b4$age >= 66 & b4$age < 86
b4$age3 <- b4$age >= 86

#-- creating new variable, `white' as a logical
b4$white <- b4$ethnicity == "WHITE"


#-----------------------------------------------------
# Issues on body weight and height, and bmi
#

#-- exploring
qs <- c(0, 0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999, 1)
b4$wt %>% quantile(qs, na.rm = TRUE)
b4$wt %>% is.na %>% table
b4$ht <- b4$ht * 100

#-- obtaining 5 weights from the weightfirstday table
wt_firstday <- getTable(from = "WEIGHTFIRSTDAY")

#-- grouping age into a single variable
b4$age_group <- ifelse(b4$age1 == TRUE, 1, ifelse(b4$age2 == TRUE, 2, 3)) %>% factor #

#-- left-joining wt_firstday table (by icustay_id as the key value)
b4 %<>% merge(wt_firstday, all.x = TRUE, sort = FALSE)

#-- evauating age-group-and-gender-based median of body weight
( wts_meds <- with(b4, tapply(wt, list(age_group, gender), function(x) median(x, na.rm = TRUE)) ) ) #

#-- picking the nearest one (among wt, weight, weight_admit, weight_daily, 
#-- weight_echoinhosp, weight_echoprehosp) 
#-- to the age-gender-based median weight
b4$wt0 <- b4$wt
b4$wt0 <- sapply(1:nrow(b4), function(i)
	chooseNearest( wts_meds[ b4$age_group[i], b4$gender[i ]],
		 b4[i, ] %>% subset(select = c(
			 wt, weight, weight_admit, weight_daily,
			 weight_echoinhosp, weight_echoprehosp))) )
b4$wt0 %>% quantile(qs)

#-- repeating same procedures for height
b4 %<>% merge(getTable(from = "HEIGHTFIRSTDAY"),  by = "icustay_id", sort = FALSE, all.x = TRUE) #

#-- same procedure with slightly different algorithms
hts_meds <- with(b4, tapply(ht, list(age_group, gender), function(x) median(x, na.rm = TRUE)) ) #

#-- picking the nearest one 
( b4$ht0 <- b4$ht ) %>% quantile(qs, na.rm = TRUE)
b4$ht0 <- sapply(1:nrow(b4), function(i)
	chooseNearest( hts_meds[ b4$age_group[i], b4$gender[i ]],
		 b4[i, ] %>% subset(select = c(
			 ht, height, height_chart, height_echo)
			 )) )
b4$ht0 %>% quantile(qs)

#-- centimeter --> meter
b4$ht0  <- b4$ht0 / 100
b4$ht0 %>% quantile(qs, na.rm = TRUE)

#-- polishing needless columns regarding weight and height
deselected <- c("wt", "weight", "weight_admit", "weight_daily", "weight_echoinhosp", "weight_echoprehosp", "ht", "height", "height_chart", "height_echo") #
b4 <- b4[, -c(which( colnames(b4) %in% deselected ))]

#-- bmi calculation
b4$bmi0 <- b4$wt0 / (b4$ht0^2)
b4$bmi0 %>% quantile(qs)

#-- Remove extremes
#---- beyond upper and lower 0.1%
b3 <- b4
extremes <- list(
	wt_lower = quantile(b4$wt0, 0.001),
	wt_upper = quantile(b4$wt0, 0.999),
	ht_lower = quantile(b4$ht0, 0.001),
	ht_upper = quantile(b4$ht0, 0.999)
	)
b4$study %>% table
b4 %<>% subset(wt0 >= extremes$wt_lower & wt0 <= extremes$wt_upper & ht0 >= extremes$ht_lower & ht0 <= extremes$ht_upper) #

#-----------------------------------------------------
#-- Regarding etomidate dose
#
b4$dose_val <- sapply(b4$dose_val_rx, function(x) valueSplit(x)$value1 %>% as.numeric) #
b4$etomidate_val <- ifelse(b4$study == "etomidate", b4$dose_val, 0)

#-- Removing rows if etomidate dose is missing or dose > 20 mg
b4$study %>% table
b4 %<>% subset((study == "etomidate" & dose_val <= 20) | study == "propofol")
b4$study %>% table

#-- Calculating etomidate dose / bw (mg/kg)
b4$dose_wt <- b4$dose_val / b4$wt0

#-----------------------------------------------------
#-- Issues about admission
#
#-- removing patients having hx of previous admission 
#       (previous discharge within 7 days before this admission)
p4 <- getTable(schema = "ETOMIDATE", from = "_PREV_DISCHARGE")
b4$study %>% table
b4 %<>% merge(p4 %>% subset( select = c(hadm_id, prev_disch) ), by = "hadm_id", sort = FALSE) #
b4 %<>% subset( subset = is.na(prev_disch) )


##-- route of admission
QS <- "SELECT HADM_ID, ADMISSION_TYPE FROM MIMICIII.ADMISSIONS"
admit_type <- dbGetQuery(con, QS)
b4 %<>% merge(admit_type, by = "hadm_id", sort = FALSE)


##-- dividing admissions < 2008 and after
QS <- "SELECT * FROM ETOMIDATE._ADMISSIONS_HALF"
admit_2008 <- dbGetQuery(con, QS)
b4 %<>% merge(admit_2008 %>% subset( select = c(hadm_id, a_half) ), by = "hadm_id" ) #

##-- row count
b4$study %>% table  # 625 vs. 11901 

##-- intermediate storage 1
b4_1 <- b4; save(b4_1, file = "b4_1.RData")

#-----------------------------------------------------
#-- re-coding mortality

#-- If mortality_30 is NA, he/she alives
b4$mortality_30[is.na(b4$mortality_30)] <- FALSE
b4$study %>% table(b4$mortality_30)

#-- mortality_2to30 = mortality of the patients who survived a certain peroid - i.e. (24-hr or 48-hr) #

daysSurv <- (b4$deathtime - b4$startdate) / 24 / 60
b4$mortality_24hr <- ifelse(daysSurv <= 1, TRUE, FALSE ) #
b4$mortality_24hr[is.na(b4$mortality_24hr)] <- FALSE #
b4$mortality_48hr <- ifelse(daysSurv <= 2, TRUE, FALSE ) #
b4$mortality_48hr[is.na(b4$mortality_48hr)] <- FALSE #
b4$mortality_2to30 <- ifelse(!b4$mortality_48hr & daysSurv < 31, TRUE, FALSE) #
b4$mortality_2to30[is.na(b4$mortality_2to30)] <- FALSE #

b4$study %>% table(b4$mortality_2to30) #
#
#            TRUE FALSE
# etomidate   132   493
# propofol   1072 10829

#-- mortality_all = all mortality (incl. 24-Hr mortality) 
b4$mortality_all <- ifelse((b4$dischtime < b4$deathtime), FALSE, TRUE) #
b4$mortality_all[ is.na(b4$mortality_all) ] <- FALSE

#------summary-------------------------------------
# Variables regarding mortality must be handled again in a4 dataset
# * mortality_all	: all-cause in-hospital mortality
# * mortality_24hr  : deaths within 24-Hr
# * mortality_48hr  : deaths within 48-Hr
# * mortality_30 	: deaths within 30-Day
# * mortality_2to30 : deaths within 30-Day in 2-Day Survivors
#-----------------------------------------------------

#-----------------------------------------------------
#-- finding patients using adrenocortical suppressors 
#---- before start of the cohort
supp <- getTable(schema = "ETOMIDATE", from = "_PATIENTS_SUPPRESSORS")
supp$suppressors <- supp$suppressors == TRUE
supp$study %>% table(supp$suppressors)
b4 %<>% merge(supp %>% subset(select = c(hadm_id, suppressors)), by = "hadm_id", sort = FALSE) #

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#------------------- MATCHING ------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
require(MatchIt)

#-- selectinig variables needed for matching
conditions <- c("study", "hadm_id", "wt0", "gender", "age1", "age2", "age3", "bmi0", "white", "admission_type", "a_half", "adrenal_insufficiency", "suppressors", "sofa") #
comorbs <- cova_names[-1]
ss <- b4[, conditions] #
ss <- cbind(ss, b4[, comorbs])

# 1:10 matching
set.seed(100)
m3.out <- MatchIt::matchit( 
	eval(
		parse(
			text = paste(
		"study == 'etomidate' ~		
		gender +
		age1 + age2 + age3 +
		I(bmi0 < 18.5) +
		I(bmi0 >= 18.5 & bmi0 < 25) +
		I(bmi0 >= 25 & bmi0 < 30) +
		I(bmi0 >= 30) +
		white +
		
		I(admission_type != 'ELECTIVE') + 
		I(a_half == '1st') + 
		adrenal_insufficiency +	
		suppressors + 	
		I(sofa > 5.0) + ",
		
		paste0(comorbs, collapse = " + ") 
		
		) ) ) # paste < parse < eval
		, data = ss, method = "nearest", ratio = 10) #

# summarizing pre-matching tables into m1
m3.out %>% summary %>% "$"(sum.all) %>% "["(, c(1:2, 4)) -> m1

# summarizing post-matching tables into m2
m3.out %>% summary %>% "$"(sum.matched) %>% "["(, c(1:2, 4)) -> m2

# summarizing sample sizes into m3
m3.out %>% summary %>% "$"(nn) -> m3

# binding them
# DO NOT read distances and genderF
m12 <- cbind(m1, m2)

# making new column name "STD"
colnames(m12)[c(3, 6)] <- "STD"

#-----------------------------------------------------
#-- tidying the row names of the summary table
r <- rownames(m12)

#-- deleting 1s at the end of the row names
r[grep("1$", r)] <- sapply(grep("1$", r), function(x) substr(r[x], 1, nchar(r[x]) -1)) #
#-- deleting TRUEs at the end of the row names
r[grep("TRUE$", r)] <- sapply(grep("TRUE$", r), function(x) substr(r[x], 1, nchar(r[x]) - 4)) #
#-- deleting I( ... ) in the row names
r <- gsub("I\\(", "", r)
r <- gsub(")", "", r)
#-- deleting underscores in place of spaces of the row names
r <- gsub("_", " ", r)
rownames(m12) <- r    
#-- new column names
colnames(m12) <- c("E-before", "P-before", "STD", 
	"E-after", "P-after", "STD") #
colnames(m3) <- c("Propofol", "Etomidate")
n <- c(m3["All", "Etomidate"], m3["All", "Propofol"], NA, 
	m3["Matched", "Etomidate"], m3["Matched", "Propofol"], NA) %>% matrix(nrow = 1)#
dimnames(n) <- list("N", colnames(m12))

#-- printing whole summary
rbind(n, m12) %>% round(2)
# or LaTeXing (not here)
# xtable::xtable(m12)

#-----------------------------------------------------
# obtaining the matched-dataset
#	and saving it into data.frame 'a4'
a4 <- MatchIt::match.data(m3.out)
a4$study %>% table

#-----------------------------------------------------
# fetching the outcome variables 
#     and grafting into 'a4'

#-- primary outcomes
oc_1 <- getTable(schema = "ETOMIDATE", from = "_ETOMIDATE_OUTCOME")
oc_2 <- getTable(schema = "ETOMIDATE", from = "_PROPOFOL_OUTCOME")

#-- preparing 'b4outcome'
study <- c(rep("etomidate", nrow(oc_1)), rep("propofol", nrow(oc_2)))
b4outcome <- rbind(oc_1, oc_2)
b4outcome <- cbind(study, b4outcome)

#-- forcing the outcomes to logical values
#---- DO NOT READ mortality, 
#---------that will be replaced with detailed values, LATER
for (i in 3:5)
	b4outcome[, i] <- b4outcome[, i] == 1

#-- joining 3 outcomes (from b4ouctome) into a4
a4 %<>% merge(b4outcome %>% subset(select = -study), by = "hadm_id", sort = FALSE) #

#-- re-evaluating values pertaing to mortality
b4_mortal <- b4 %>% subset( select = c(hadm_id, mortality_all, mortality_24hr, mortality_48hr, mortality_30, mortality_2to30) ) #
a4 %<>% merge(b4_mortal, key = "hadm_id", sort = FALSE)
a4$study %>% table

#---- 48-Hr survival and death-after, simply
a4$mortality <- !a4$mortality_48hr & a4$mortality_all
a4$survival <- !a4$mortality_48hr
#---- showing
a4$study %>% table(a4$survival)
a4$study %>% table(a4$mortality)

#-----------------------------------------------------
#-- secondary outcome variables

#-- joining admittime, dischtime, los from 'b4' into 'a4'
a4 %<>% merge(b4 %>% subset( select = c(hadm_id, admittime, dischtime, startdate, deathtime, los) ), key = "hadm_id", all.x = TRUE, sort = FALSE) #

#-- creating a new variable 'stay,' 
#---- that equals total duration of stay (days)
a4$stay <- with(a4, dischtime - admittime)

#-- vasopressor time
QS <- "
	WITH V AS (
		SELECT HADM_ID, V.ICUSTAY_ID, DURATION_HOURS
		FROM MIMICIII.VASOPRESSORDURATIONS V
		JOIN MIMICIII.ICUSTAYS I
		ON V.ICUSTAY_ID = I.ICUSTAY_ID
		)
	SELECT DISTINCT ON (HADM_ID) HADM_ID,
			SUM(DURATION_HOURS) OVER W AS DUR
	FROM V
	WINDOW W AS (PARTITION BY HADM_ID)
	ORDER BY HADM_ID
	"
vasopressors <- dbGetQuery(con, QS)

#---- must be left-joined (variable 'dur')
a4 %<>% merge(vasopressors, key = "hadm_id", sort = FALSE, all.x = TRUE) 

# if dur is NA, by definition, vasopressor durations are zero.
a4$dur[ is.na(a4$dur) ] <-  0
a4$study %>% table

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#---------------------- ANALYSIS ---------------------
#------------------<PRIMARY OUTCOMES>-----------------
#-----------------------------------------------------
#-----------------------------------------------------

#-- alpha correction
n_of_primary_outcome <- 4
alpha <- 0.05 / n_of_primary_outcome
CL <- 1 - alpha

#-- Odds Ratio
ta0 <- table(a4$study, a4$survival) #
ta1 <- table(a4$study, a4$mortality) #
ta2 <- table(a4$study, a4$cv_morbidity) #
ta3 <- table(a4$study, a4$infectious_morbidity)  #
t0 <- ta0 %>%  odds.ratio(conf.level = CL) #
t1 <- ta1 %>%  odds.ratio(conf.level = CL) #
t2 <- ta2 %>% odds.ratio(conf.level = CL) #
t3 <- ta3 %>% odds.ratio(conf.level = CL) #

#-- incidences of primary outcome into 'primar'
primar <- list(survival = ta0, 
	mortality = ta1, 
	cv = ta2, 
	inf = ta3)

#-- combininig incidences with ORs 
incidence_table <- lapply(primar, function(el) 	
	c( el[, 1], 
		apply(el, 1, function(rw) rw[1] * 100 / sum(rw))	
	)[ c(1, 3, 2, 4) ] # etomidate first 2, propofol next 2
	) %>% unlist %>% matrix (byrow = TRUE, ncol = 4)

odds_table <- rbind(t0, t1, t2, t3)
rownames(odds_table) <- c("48-hr Survival", "Mortality", "CV Morbidity", "Infectious Morbidity") #
colnames(odds_table) <- c("Odds Ratio", "Lower", "Upper")
odds_table %<>% cbind(incidence_table, .)
colnames(odds_table)[1:5] <- c("Etomi (N)", "Etomi (%)", "Prop (N)", "Prop (%)", "OR") #
(odds_table %>% round(2) -> primary_outcome)

#-- this matrix will be used directly for drawing forestplot
#---- 

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#---------------------- ANALYSIS ---------------------
#-------------------<OTHER OUTCOMES>-----------------
#-----------------------------------------------------
#-----------------------------------------------------
#
#-- OTHER OUTCOME
#	1) corticosteroid replacement (T/F) after cohort startdate
#	1') cortisol measurement
#	2) vasopressor duration
#	3) vasopressor patients
#	4) ICU stay duration
#	5) Hospital duration

CL <- 0.95

#-- corticosteroid replacement
repl <- getTable(schema = "ETOMIDATE", from = "_REPLACEMENT")
selected <- a4$hadm_id
repl$selected <- repl$hadm_id %>% sapply(function(x) ifelse(x %in% selected, 1, 0))	#
(repl$selected == 1) %>% table	
repl <- repl[repl$selected == 1, ]
repl$replacement %<>% factor(levels = c(1, 0), labels = c("replaced", "no"))
(repl$study %>% table(repl$replacement) %>% print %>% odds.ratio -> f)

#-- drug names included the above query
Q <- "
SELECT DISTINCT drug_name_generic, drug_name_poe, route
FROM mimiciii.prescriptions
WHERE (drug_name_generic ~* 'dexamethason' 
	OR drug_name_generic ~* 'cort' 
	OR drug_name_generic ~* 'predni') 
	AND route IN ('IV', 'ORAL', 'PO/NG', 'PO', 'NG')
	" #
drug_names <- dbGetQuery(con, Q)
drug_names$drug_name_generic %>% unique

#-- blood cortisol measurement
cortisol <- getTable(schema = "ETOMIDATE", from = "_CORTISOL_FULL")
cortisol$cortisol[cortisol$cortisol %>% grep("GREAT", .)] <- 666
cortisol$cortisol[cortisol$cortisol %>% grep(">", .)] <- 666
cortisol$cortisol %<>% as.numeric
cortisol$cortisol %>% summary(na.rm = TRUE)
 
#-- selecting the matched admissions
cortisol$selected <- cortisol$hadm_id %>% sapply(function(x) ifelse(x %in% selected > 0, 1, 0))		#
#---- extreme values must be excluded when estimating mean
cortisol %<>% subset(selected == 1 & cortisol != 666)

with(cortisol %>% subset(before == "after" & ampm == "am"),
	tapply(cortisol, study, summary)
	)
bartlett.test(cortisol ~ study, cortisol %>% subset(before == "after" & ampm == "am") ) # heteroscedasticity assumed
( cortisol_val <- t.test(cortisol ~ study,
	cortisol %>% subset(before == "after" & ampm == "am") ) )

#-- vasopressor duration 
bartlett.test(dur ~ study, a4) # heteroscedasticity assumed
( vaso_time <- t.test(dur ~ study, a4) )

#-- vasopressor patients (admissions)
( vaso_patients <- table(a4$study, a4$dur > 0) %>%  print %>% odds.ratio )

#-- ICU stay duration
bartlett.test(los ~ study, a4)
( los_icu <- t.test(los ~ study, a4) ) 

#-- Hospitalization duration
bartlett.test(stay ~ study, a4)
( los_hospital <- t.test(stay  ~ study, a4) )


#-- calculating and binding all ES
#---- regarding other outcomes
#---- requiring the package, 'effsize'
secondaryOutcome <- rbind(	
	
	# row 1 : corticosteroid replacement
	c( 
		(t <- with(repl, table(study, replacement))) %>% "["(, "replaced"),
		t %>% odds.ratio %>% "["("conditional MLE"),
		t %>% odds.ratio %>% "["(2:3) )
		
	
	# row 2 : vasoprossor duration
	, c(table(a4$study, a4$dur > 0)[, "TRUE"], vaso_patients)
		
	# row 3 : cortisol measurement
	, c(getOdds(cortisol_val)[1:2], 
		(t <- 
			effsize::cohen.d(cortisol ~ study, subset(cortisol, ampm == "am" & before == "after")) )$estimate, #
			t$conf.int)
	
	# vasopressor time
	, c(getOdds(vaso_time)[1:2], 
		(t <- effsize::cohen.d(dur ~ study, a4))$estimate, 
		t$conf.int)
	
	# ICU stay
	, c(getOdds(los_icu)[1:2], 
		(t <- effsize::cohen.d(los ~ study, a4))$estimate, 
		t$conf.int)
	
	# Hospitalization
	, c(getOdds(los_hospital)[1:2], 
		(t <- effsize::cohen.d(stay %>% as.numeric ~ study, a4))$estimate, 
		t$conf.int ) #
	)
dimnames(secondaryOutcome) <- list(c("Steroid Replacement, N", "Vasopressor, N", "Cortisol level, mcg/dL", "Vasoprossor, hr", "ICU Stay, day", "Hospital Stay, day"), c("Etomidate", "Propofol", "d or OR", "LWR", "UPR")) #


#---- further tidying
(secondaryOutcome %>% "["(1:2, 1:2) %>% round(0) %>% cbind(secondaryOutcome %>% "["(1:2, -(1:2)) %>% round(2)) -> s1 )#
(secondaryOutcome %>% "["(3:6, 1:2) %>% round(1) %>% cbind(secondaryOutcome %>% "["(3:6, -(1:2)) %>% round(2)) -> s2 )#

#-- etomidate dose - mortality relationship
#---- skipping detailed calculation on etomidate dose summary
#---- dose mean 15.4 mg, median 14 mg, Q1, Q2 10, 20 mg
#---- dose/kg 0.20, 0,19, 0.14, 0.24 mg/kg
#------ TEST RUN
dose.glm <- glm((mortality_all & !mortality_48hr)  ~ I(etomidate_val  / wt0 * 10), b4, family = binomial) #
cbind(coefficients(dose.glm), confint(dose.glm)) %>% exp %>% round(2)

#-- Steroid replacement and Death
#---- Do patients given steroid replacment die more?
death_table <- a4 %>% subset( select = c(hadm_id, mortality) )
repl_table <- repl %>% subset( select = c(hadm_id, study, replacement) )
repl_table %<>% merge(death_table, key = "hadm_id", all.x = TRUE)
repl_table$replacement %>% table(repl_table$mortality) %>% print %>% odds.ratio

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#------------------- FULL MODELLING ------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
require(MASS)
#-- exploratory re-estimation of 
#---- etomidate effect
#---- on death (per 0.1 mg/kg)
a4 %<>% merge(b4 %>% subset(select = c(hadm_id, etomidate_val)), key = "hadm_id", sort = FALSE, all.x = TRUE) #

#---- 
dose.glm <- glm( 
	eval(
		parse(
			text = paste("
		(mortality_all & !mortality_48hr) ~	
		
		I(etomidate_val  / wt0 * 10) +		
		gender +
		age1 + age2 + age3 +
		I(bmi0 < 18.5) +
		I(bmi0 >= 18.5 & bmi0 < 25) +
		I(bmi0 >= 25 & bmi0 < 30) +
		I(bmi0 >= 30) +
		white +
		
		I(admission_type != 'ELECTIVE') + 
		I(a_half == '1st') + 
		adrenal_insufficiency +	
		suppressors + 	
		I(sofa > 5.0) + ",
		
		# comorbidities
		#-- among comorbidities, 
		#-- 'peptic ulcer' was excluded b/o non-existence
		paste0(comorbs %>% setdiff("peptic_ulcer"), collapse = " + ") 	
		) ) ) # paste < parse < eval
		, a4, family = binomial
	)
#---- next stepwise selection procedure 
dose.step <- MASS::stepAIC(dose.glm, trace = 0)
m.full <- cbind(coefficients(dose.glm), confint(dose.glm)) %>% exp %>% round(2)
m.step <- cbind(coefficients(dose.step), confint(dose.step)) %>% exp %>% round(2)

#------ tidying the output
#-- tidying the row names of the summary table
#---- m.full
r <- rownames(m.full)
#-- deleting 1s at the end of the row names
r[grep("1$", r)] <- sapply(grep("1$", r), function(x) substr(r[x], 1, nchar(r[x]) -1)) #
#-- deleting TRUEs at the end of the row names
r[grep("TRUE$", r)] <- sapply(grep("TRUE$", r), function(x) substr(r[x], 1, nchar(r[x]) - 4)) #
#-- deleting I( ... ) in the row names
r <- gsub("I\\(", "", r)
r <- gsub(")", "", r)
#-- deleting underscores in place of spaces of the row names
r <- gsub("_", " ", r)
rownames(m.full) <- r    
#---- m.step
r <- rownames(m.step)
#-- deleting 1s at the end of the row names
r[grep("1$", r)] <- sapply(grep("1$", r), function(x) substr(r[x], 1, nchar(r[x]) -1)) #
#-- deleting TRUEs at the end of the row names
r[grep("TRUE$", r)] <- sapply(grep("TRUE$", r), function(x) substr(r[x], 1, nchar(r[x]) - 4)) #
#-- deleting I( ... ) in the row names
r <- gsub("I\\(", "", r)
r <- gsub(")", "", r)
#-- deleting underscores in place of spaces of the row names
r <- gsub("_", " ", r)
rownames(m.step) <- r    

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#--------------------- BP PROFILE --------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

#-- fetching all itemid regarding blood pressure
#---- filtering hadm_id appearing in 'a4'
bps <- getTable(schema = "ETOMIDATE", from = "_BPSS" )
bpss <- bps[bps$hadm_id %in% a4$hadm_id,]

#-- bp profile at the cohort startdate
#---- by Cohen's d
#---- simplification
sbp_mbp <- sapply(c("sbp", "mbp"), function(s) 
	with( subset(bpss, bp == s),
		lapply(list(avg_bp, min_bp, max_bp), 
			function(x) tapply(x, study, function(x) 
			mean (x, na.rm = TRUE) ) ) )  %>% unlist
				) %>% as.vector %>% matrix(byrow = TRUE, ncol = 2) %>% round(1)
dimnames(sbp_mbp) <- list(c("sbp-avg", "sbp-min", "sbp-max", "mbp_avg", "mbp-min", "mbp-max"), c("etomidate", "propofol")) #
#------ calc Cohen's d and its CI
dif <- sapply(c("sbp", "mbp"), function(s) 
	with( subset(bpss, bp == s),
		lapply(list(avg_bp, min_bp, max_bp), 
			function(x) 
			effsize::cohen.d(x ~ study)$estimate ) ) ) %>% matrix(ncol = 1)
colnames(dif) <- "Cohen's d"		
intv <- sapply(c("sbp", "mbp"), function(s) 
	with( subset(bpss, bp == s),
		lapply(list(avg_bp, min_bp, max_bp), 
			function(x) 
			effsize::cohen.d(x ~ study)$conf.int ) ) %>% unlist
			) %>% matrix(ncol = 2, byrow = TRUE) %>% round(3)
colnames(intv) <- c("LWR", "UPR")
#------ combining them
( cbind(sbp_mbp, dif, intv) -> bps_summary )

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#--------------------- NAMES OF   --------------------
#---------- CV, INFECTIOUS COMPLICATIONs -------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

cv <- c(458
	, 4582
	, 45821
	, 45829
	, 9971
	, 9972
	, 9980
	, 78550
	, 7855
	, 78551
	, 78559
	, 27652
	, 4233)
cv2 <- paste0("('", cv %>% as.character %>% paste0(collapse = "', '"), "')") #
cv_list <- selectFrom(
	select = "LONG_TITLE, ICD9_CODE",
	from = "D_ICD_DIAGNOSES",
	where = paste0("ICD9_CODE IN ", cv2 ) )
inf <- c(56961, 51901, 53086, 53641	
	, 99660, 99661, 99662, 99663, 99664, 99665, 99666		
	, 99762
	, 99731
	, 99851
	, 99859
	, 99931
	, 99667
	, 99668
	, 99669)
inf2 <- paste0("('", inf %>% as.character %>% paste0(collapse = "', '"), "')") #
inf_list <- selectFrom(
	select = "LONG_TITLE, ICD9_CODE",
	from = "D_ICD_DIAGNOSES",
	where = paste0("ICD9_CODE IN ", inf2 ) )
	
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#--------------------- DIAGNOSIS  --------------------
#------------------- ON ADMISSION --------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

diagnosis_table <- selectFrom(
		select = "HADM_ID, DIAGNOSIS",
		from = "ADMISSIONS")
a4 %<>% merge(diagnosis_table, key = "hadm_id", all.x = TRUE, sort = FALSE)
with(a4 %>% subset(study == "etomidate"),
	table(diagnosis)[order(table(diagnosis), decreasing = TRUE)][1:25]
	) -> e
with(a4 %>% subset(study == "propofol"),
	table(diagnosis)[order(table(diagnosis), decreasing = TRUE)][1:25]
	) -> p
freq_diagnosis <- data.frame(
	etomidate_diag = e %>% as.vector,
	etomidate_diag_name = names(e),
	propofol_diag = p %>% as.vector,
	propofol_diag = names(p) )
	
#======================================================
save(a4, b4, primary_outcome, s1, s2, bps_summary, m.full, m.step, cv_list, inf_list, freq_diagnosis, file = "a4b4Jul2020.RData") #


#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#------------------ DISPLAY ONLY  --------------------
#--------------------- OVERALL -----------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

primary_outcome %>% disp(head = "Primary Outcome")
s1 %>% disp(head = "Other Outcome Part I")
s2 %>% disp(head = "Other Outcome Part II")
bps_summary %>% disp(head = "BP Summary")

