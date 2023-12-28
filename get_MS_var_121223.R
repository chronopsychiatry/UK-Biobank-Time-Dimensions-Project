library(psych)


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


comment(MS_UKB$MS_source) <- "Data field 131043"
comment(MS_UKB$MS_year) <-"Data field 131042"


#MS source is the diagnosis of MS
#MS_year is the year of diagnosis

#add to UKB Master
UKB_master <- merge(UKB_master,MS_UKB, by="eid")

#MS_year is the year of diagnosis
UKB_master$MS_year <- as.integer(UKB_master$MS_year)

#MS_age is the age at diagnosis
UKB_master$MS_age <- UKB_master$MS_year - UKB_master$year_born

#check if anyone was diagnosed before age 20
remove1 <- UKB_master$eid[!is.na(UKB_master$MS_age) & UKB_master$MS_age < 20]

#check if anyone was diagnosed before birth
remove2 <- UKB_master$eid[!is.na(UKB_master$MS_year) & UKB_master$MS_year < UKB_master$year_born]

#eids to remove
UKB_master <- subset(UKB_master, !(eid %in% remove1 | eid %in% remove2))

# decide not to exclude cases after online work study?
# #need to exclude data after diagnosis of MS

# data$Measurement_Date <= diagnosis_date #rows of data collected after diagnosis are disregarded - this will take them out of the study altogether?  Not sure how to match controls shiftwork before and after diagnosis?
