# process MS data

MS_UKB <- read.delim("./MS_UKB.tsv")

# recode and rename MS source of diagnosis and date of diagnosis
lvl.131043 <- c(20,21,30,31,40,41,50,51)
lbl.131043 <- c("Death register only",
	"Death register and other source(s)",
	"Primary care only",
  "Primary care and other source(s)",
	"Hospital admissions data only",
  "Hospital admissions data and other source(s)",
	"Self-report only",
  "Self-report and other source(s)")

MS_UKB$MS_source <- factor(MS_UKB$X131043.0.0, levels=lvl.131043, labels=lbl.131043)
MS_UKB$MS_year <-substring(MS_UKB$X131042.0.0,1,4)
MS_UKB$MS_age <- MS_UKB$MS_year - age

comment(MS_UKB$MS_source) <- "Data field 131043"
comment(MS_UKB$MS_year) <-"Data field 131042")

#MS source is the diagnosis of MS
#MS_year is the year of diagnosis
#MS_age is the age of diagnosis

#need to exclude cases after online work study?

#need to exclude data after diagnosis of MS
data$Measurement_Date <= diagnosis_date #rows of data collected after diagnosis are disregarded

#this is the model
model <- glmer(MS_Risk ~ work_type + age_of_exposure + dose_of_night_shift + (1|Individual_ID), 
               data = data, 
               family = binomial(link = "logit"))

#Table one stratified by MS and night, day and mix and non-shift
#table two regression table SW and MS risk.


