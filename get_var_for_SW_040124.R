library(tidyverse)
library(dplyr)
library(pillar)
library(readxl)
library(hms)


### R extract & data prep #########
#set cwd to folder you want to do your analysis in
setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

#import the downloaded data and run encoding R code
script_path <- file.path(getwd(), "helper", "ukb673864.r")

# Run the script
source(script_path)

#rename default 'bd' dataframe as core_vars
core_vars <- bd

# withdraw latest participants (download latest withdraw list from basket)
withdrawn <- read.csv("./withdraw99491_10_20231013.txt")
core_vars <- core_vars[! core_vars$f.eid %in% withdrawn$X1195908,]

#make new data frame to store variables
UKB_master <- data.frame(core_vars$f.eid)
comment(UKB_master )<-c("Core variables for 'Rhythms of Life' project")
colnames(UKB_master)[1]<-"eid"

# can double-check the withdrawal worked with: which(UKB_master$eid == withdrawnid)



## Primary demographics ####################

#sex (31)
sex <- core_vars$f.31.0.0
comment(sex)<-c("Datafield=31.0.0")
sex <- factor(sex)

#age (21003)
age <- core_vars$f.21003.0.0
comment(age)<-c("Datafield = 21003.0.0")

#year of birth (34)
year_born <- core_vars$f.34.0.0
comment(year_born)<-c("Datafield = 34.0.0")

#month of birth (52)
month_born <- core_vars$f.52.0.0
comment(month_born)<-c("Datafield = 52.0.0")

#ethnicity (21000)
ethnicity <- core_vars$f.21000.0.0
comment(ethnicity)<-c("Datafield = 21000.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sex, age, year_born, month_born, ethnicity)

#remove environment variables no longer needed outside of dataframe
rm(sex, age, year_born, month_born, ethnicity)

#small ethicnicty var 
UKB_master <- UKB_master %>%
  mutate(ethnicity_5 = case_when(
    
    ethnicity == "Prefer not to answer" ~ NA,
    ethnicity == "Do not know" ~ NA,
    ethnicity == "White" ~ "White",
    ethnicity == "Mixed" ~ "Mixed",
    ethnicity == "Asian or Asian British" ~ "Asian",
    ethnicity == "Black or Black British" ~ "Black",
    ethnicity == "Chinese" ~ "Chinese",
    ethnicity == "Other ethnic group" ~ "Other",
    ethnicity == "British" ~ "White",
    ethnicity == "Irish" ~ "White",
    ethnicity == "Any other white background" ~ "White",
    ethnicity == "White and Black Caribbean" ~ "Mixed",
    ethnicity == "White and Black African" ~ "Mixed",
    ethnicity == "White and Asian" ~ "Mixed",
    ethnicity == "Any other mixed background" ~ "Mixed",
    ethnicity == "Indian" ~ "Asian",
    ethnicity == "Pakistani" ~ "Asian",
    ethnicity == "Bangladeshi" ~ "Asian",
    ethnicity == "Any other Asian background" ~ "Asian",
    ethnicity == "Caribbean" ~ "Black",
    ethnicity == "African" ~ "Black",
    ethnicity == "Any other Black background" ~ "Black",
    TRUE ~ NA
  )
  )     

#get townsend index
townsend <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/townsend.tsv")
names(townsend) <- c("eid", "townsend")
comment(townsend$townsend)<-c("Data field = 22189.0.0")
UKB_master <- merge(UKB_master, townsend, by="eid")



## Assessment Centre ####################

#assess_date (53)
assess_date <- core_vars$f.53.0.0
comment(assess_date)<-c("Datafield = 53.0.0")

#assess_centre (54)
assess_centre <- core_vars$f.54.0.0
comment(assess_centre)<-c("Datafield = 54.0.0")

#import table of assessment centre latitudes
Table_latitude_assesment_centres <- read.csv("./Table_latitude_assesment_centres.csv")

#make latitude table for merge
eid <- as.data.frame(core_vars$f.eid)
names(eid)<- "eid"
centre <- as.integer(core_vars$f.54.0.0)
centre <- as.data.frame(centre)
eid_centre <- cbind(eid,centre)

#merge eid_centre and latitude
latitude_with_eid <- merge(eid_centre,Table_latitude_assesment_centres, by="centre")
latitude_with_eid <- latitude_with_eid[order(latitude_with_eid$eid),]

#	Assessment centre name (derived field)
assess_name <- latitude_with_eid$short_name
comment(assess_name)<-c("derived field")

#	Assessment centre latitude (derived field)
assess_latitude <- latitude_with_eid$lat
comment(assess_latitude)<-c("derived field")

#	Assessment centre longitude (derived field)
assess_longitude <- latitude_with_eid$long
comment(assess_longitude)<-c("derived field")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, assess_date, assess_centre, assess_name, assess_latitude, assess_longitude)

#remove environment variables no longer needed outside of dataframe
rm(assess_date, assess_centre, centre, eid, latitude_with_eid, Table_latitude_assesment_centres, assess_name,
   assess_latitude, assess_longitude, eid_centre)

# add latitude of birth
latitude_birth_lookup <- read.csv("./latitude_birth_lookup.csv")
UKB_master <- merge(UKB_master,latitude_birth_lookup[,c(1:2)], all.x = TRUE, by = "country_birth_nonuk")
UKB_master <- UKB_master[order(UKB_master$eid, decreasing = FALSE), ]

#add UK latitude
UKB_master$birth_latitude <- ifelse(UKB_master$country_birth_uk == "England", 52,
                                ifelse(UKB_master$country_birth_uk == "Scotland", 56,
                                       ifelse(UKB_master$country_birth_uk == "Wales", 52,
                                              UKB_master$latitude)))











## Education & Employment #############################################

#	Age completed full-time education (845)
age_completed_education <- core_vars$f.845.0.0
comment(age_completed_education)<-c("Datafield = 845")
age_completed_education <- ifelse(age_completed_education == -2, 0,
                                         ifelse(age_completed_education == -1, NA,
                                                ifelse(age_completed_education == -3, NA,
                                                       age_completed_education)))
#-2 represents "Never went to school"
#-1 represents "Do not know"
#-3 represents "Prefer not to answer"
#	Qualifications (6138)
qualifications <- core_vars$f.6138.0.0
comment(qualifications)<-c("Datafield = 6138")

#	Current employment status (6142)
employed <- core_vars$f.6142.0.0
comment(employed)<-c("Datafield = 6142")

#	Current employment status (corrected) (20119)
employed_corr <- core_vars$f.20119.0.0
comment(employed_corr)<-c("Datafield = 20119")

#	Job involves night shift work (3426)
night_shift <- core_vars$f.3426.0.0
comment(night_shift)<-c("Datafield = 3426")

#	Job involves shift work (826)
shift_work <- core_vars$f.826.0.0
comment(shift_work)<-c("Datafield = 826")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, age_completed_education, qualifications, employed,
                    employed_corr, night_shift, shift_work)

#remove environment variables no longer needed outside of dataframe
rm(age_completed_education, qualifications, employed, employed_corr, night_shift,
   shift_work)


#get PA from touchscreen data
PA <- read.delim("./PA.tsv")

names (PA) <- c("eid", "MET_week", "IPAQ","modPA" )
comment(PA$MET_week)<-c("Data field = 22040.0.0")
comment(PA$IPAQ)<-c("Data field = 22032.0.0")
comment(PA$modPA)<-c("Data field = 884.0.0")

# recode modPA
PA$modPA <- ifelse(PA$modPA == -1, NA,PA$modPA)
# Coding	Meaning
# -1	Do not know
# -3	Prefer not to answer


# recode IPAQ
PA$IPAQ <- factor (PA$IPAQ, levels = c(0:2), labels = c("low", "moderate","high"))
# Coding	Meaning
# 0	low
# 1	moderate
# 2	high
UKB_master <- merge(UKB_master, PA, by = "eid")





## Early life  #############################################

#Adopted as a child (1767)
adopted <- core_vars$f.1767.0.0
comment(adopted)<-c("Datafield = 1767")

#Birth weight (20022)
birth_weight  <- core_vars$f.20022.0.0
comment(birth_weight)<-c("Datafield = 20022")

#Birth weight metric (120)
birth_weight_metric  <- core_vars$f.120.0.0
comment(birth_weight_metric)<-c("Datafield = 120")

#Breastfed as a baby (1677)
breastfed <- core_vars$f.1677.0.0
comment(breastfed)<-c("Datafield = 1677")

#Country of Birth (non-UK origin) (20115)
country_birth_nonuk <- core_vars$f.20115.0.0
comment(country_birth_nonuk)<-c("Datafield = 20115")

#Country of birth (UK/elsewhere) (1647)
country_birth_uk <- core_vars$f.1647.0.0
comment(country_birth_uk)<-c("Datafield = 1647")

#Handedness (chirality/laterality) (1707)
handedness <- core_vars$f.1707.0.0
comment(handedness)<-c("Datafield = 1707")

#maternal smoking around birth (1787)
maternal_smoking <- core_vars$f.1787.0.0
comment(maternal_smoking)<-c("Datafield = 1787")


#childhood obesity
childhood_obesity <- read.delim("./childhood_obesity.tsv")
names (childhood_obesity) <- c("eid", "child_obesity")
comment(childhood_obesity$child_obesity)<-c("Data field = 1687.0.0")

levels_CO <- c(1, 2, 3, NA, NA)
labels_CO <- c("Thinner", "Plumper", "About average","NA",	"NA")
childhood_obesity$child_obesity <- factor(childhood_obesity$child_obesity, labels = lablels_CO, levels = levels_CO)
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Do not know")] <- NA
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Prefer not to answer")] <- NA
childhood_obesity$child_obesity<-droplevels(childhood_obesity$child_obesity)
UKB_master <- merge (UKB_master, childhood_obesity, by = "eid")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, adopted, birth_weight, birth_weight_metric,
                    breastfed, country_birth_uk, country_birth_nonuk, handedness,
                    maternal_smoking)

#remove environment variables no longer needed outside of dataframe
rm(adopted, birth_weight, birth_weight_metric, breastfed, country_birth_uk,
   country_birth_nonuk, maternal_smoking, handedness)


# clean smoked before age 20
smoking.early.life <- read.delim("./smoking early life.tsv")

names (smoking.early.life) <- c("eid", "smoking_status", "past_smoking", "start_smoke_P","current_smoking", "start_smoke_current")

comment(smoking.early.life$smoking_status)<-c("Data field = 20116.0.0")
comment(smoking.early.life$past_smoking)<-c("Data field = 1249.0.0")
comment(smoking.early.life$start_smoke_P)<-c("Data field = 2867.0.0")
comment(smoking.early.life$current_smoking)<-c("Data field = 1239.0.0")
comment(smoking.early.life$start_smoke_current)<-c("Data field = 3436.0.0")

#clean smoking status Data field = 20116.0.0
smoking.early.life$smoking_status <- ifelse(smoking.early.life$smoking_status == -3, NA, smoking.early.life$smoking_status)
smoking.early.life$smoking_status <- factor(smoking.early.life$smoking_status, levels = c(0:2), labels = c("Never","Previous","Current"))
# Coding	Meaning
# -3	Prefer not to answer
# 0	Never
# 1	Previous
# 2	Current

# clean past smoking - Data field = 1249.0.0
smoking.early.life$past_smoking <- ifelse(smoking.early.life$past_smoking == -3, NA, smoking.early.life$past_smoking)
smoking.early.life$past_smoking <- factor(smoking.early.life$past_smoking, levels = c(1:4), labels = c("Smoked on most or all days","Smoked occasionally", "Just tried once or twice", "I have never smoked"))

# Coding	Meaning
# 1	      Smoked on most or all days
# 2	      Smoked occasionally
# 3	      Just tried once or twice
# 4	      I have never smoked
# -3	    Prefer not to answer

# clean age started smoking (prior) Data field = 2867.0.0
smoking.early.life$start_smoke_P <- ifelse(smoking.early.life$start_smoke_P == -1, NA,
                                           ifelse(smoking.early.life$start_smoke_P == -3, NA,
                                                  smoking.early.life$start_smoke_P))
# Coding	Meaning
# -1	    Do not know
# -3	    Prefer not to answer

# clean current smoking - Data field = 1239.0.0
smoking.early.life$current_smoking <- ifelse(smoking.early.life$current_smoking == -3, NA, current_smoking)
smoking.early.life$current_smoking <- factor(smoking.early.life$current_smoking, levels = c(1:3), labels = c("No","Yes, on most or all days","Only occasionally"))

# Coding	Meaning
# 1	      Yes, on most or all days
# 2	      Only occasionally
# 0	      No
# -3	    Prefer not to answer

# clean age started smoking (current) data field = 3436.0.0")
smoking.early.life$start_smoke_current <- ifelse(smoking.early.life$start_smoke_current == -1, NA, 
                                                 ifelse(smoking.early.life$start_smoke_current == -3, NA, 
                                                        smoking.early.life$start_smoke_current))

# Coding	Meaning
# -1	    Do not know
# -3	    Prefer not to answer

# make variable for early_life_smoking - this is smoking between 15-20yo
smoking.early.life$early_life_smoke <- NA
smoking.early.life$early_life_smoke <- 
  ifelse(smoking.early.life$start_smoke_current <= 20 | smoking.early.life$start_smoke_P <= 20, 1, NA)

smoking.early.life$early_life_smoke[!(smoking.early.life$start_smoke_current < 20 | smoking.early.life$start_smoke_P < 20)] <- 0

UKB_master <- merge(x = UKB_master, y = smoking.early.life, by = "eid")

# make variable for smoking status through the age brackets for merging into my data
age_stop_smoking <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/age_stop_smoking.tsv")
age_stop_smoking <-age_stop_smoking[c(1,3)]
names (age_stop_smoking) <- c("eid", "age_stop_smoke")
comment(age_stop_smoking$age_stop_smoke)<-c("Data field = 2897.0.0")
age_stop_smoking$age_stop_smoke <- ifelse(age_stop_smoking$age_stop_smoke == -1, NA, 
                                          ifelse(age_stop_smoking$age_stop_smoke == -3, NA, 
                                                 age_stop_smoking$age_stop_smoke))
#Coding 	Meaning
#-1	      Do not know
#-3	      Prefer not to answer

UKB_master <- merge(UKB_master,age_stop_smoking, by="eid", all.x=TRUE) # this adds age of smoke stop



## Health  #############################################

#smoking status (20116)
smoking_status <- core_vars$f.20116.0.0
comment(smoking_status)<-c("Datafield = 20116")
smoking_status <- factor(smoking_status)
smoking_status <- ifelse(smoking_status == 'Prefer not to answer', NA, smoking_status)

#alcohol intake frequency (1558)
alcohol_intake <- core_vars$f.1558.0.0
comment(alcohol_intake)<-c("Datafield = 1558")
mydata$alcohol_intake <- ifelse(alcohol_intake == 'Prefer not to answer', NA, alcohol_intake)

#overall health rating (2178)
health_self_report <- core_vars$f.2178.0.0
comment(health_self_report)<-c("Datafield = 2178")

#number of medications (137)
medication_number <- core_vars$f.137.0.0
comment(medication_number)<-c("Datafield = 137")

#medication code (20003)
medication_code <- core_vars$f.20003.0.0
comment(medication_code)<-c("Datafield = 20003")

#attendance/disability/mobility allowance (6146)
disability_allowance <- core_vars$f.6146.0.0
comment(disability_allowance)<-c("Datafield = 6146")

#longstanding illness or disability (2188)
disability_self_report <- core_vars$f.2188.0.0
comment(disability_self_report)<-c("Datafield = 2188")

#seen psychiatrist for nerves/anxiety/depression (2100)
depress_psych <- core_vars$f.2100.0.0
comment(depress_psych)<-c("Datafield = 2100")

depress_psych <- ifelse(depress_psych == 'Prefer not to answer', NA, depress_psych)
depress_psych<- ifelse(depress_psych == 'Prefer not to answer', NA,
                              ifelse(depress_psych == "Do not know", NA,
                                     depress_psych))

#seen GP for nerves/anxiety/depression (2090)
depress_gp <- core_vars$f.2090.0.0
comment(depress_gp)<-c("Datafield = 2090")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, smoking_status, alcohol_intake, health_self_report,
                    medication_number, medication_code, disability_allowance, disability_self_report,
                    depress_psych, depress_gp)

#remove environment variables no longer needed outside of dataframe
rm(smoking_status, alcohol_intake, health_self_report, medication_number,
   medication_code, disability_allowance, disability_self_report, depress_gp, depress_psych)





## Seasonal#############################################

#time spent outdoors in summer (1050)
time_outdoors_summer <- core_vars$f.1050.0.0
comment(time_outdoors_summer)<-c("Datafield = 1050")
# clean time_outdoors_summer
# -10	Less than an hour a day
# -1	Do not know
# -3	Prefer not to answer

time_outdoors_summer <- ifelse(time_outdoors_summer == -10, .5,
                                      ifelse(time_outdoors_summer == -1, NA,
                                             ifelse(time_outdoors_summer == -3, NA,
                                                    time_outdoors_summer)))

#time spent outdoors in winter (1060)
time_outdoors_winter <- core_vars$f.1060.0.0
comment(time_outdoors_winter)<-c("Datafield = 1060")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, time_outdoors_summer, time_outdoors_winter)

#remove environment variables no longer needed outside of dataframe
rm(time_outdoors_summer, time_outdoors_winter)




## Sleep #############################################

#Sleep duration (1160)
sleep_duration<- core_vars$f.1160.0.0
comment(sleep_duration)<-c("Datafield = 1160")

#value -3 (Prefer not to answer)
#value -1 (Do not know)
sleep_duration <- ifelse(sleep_duration == -3, NA,
                                ifelse(sleep_duration == -1, NA,
                                       sleep_duration))

#Getting up in morning	(1170)
getting_up  <- core_vars$f.1170.0.0
comment(getting_up)<-c("Datafield = 1170")

#Morning/evening person (chronotype) (1180)
chronotype <- core_vars$f.1180.0.0
comment(chronotype)<-c("Datafield = 1180")

chronotype <- ifelse(chronotype == "Do not know", NA,
                            ifelse(chronotype == "Prefer not to answer", NA,
                                   chronotype))
chronotype <- factor(chronotype)

#Nap during day (1190)
day_naps <- core_vars$f.1190.0.0
comment(day_naps)<-c("Datafield = 1190")

#Sleeplessness / insomnia (1200)
insomnia <- core_vars$f.1200.0.0
comment(insomnia)<-c("Datafield = 1200")
insomnia <- ifelse(insomnia == "Prefer not to answer", NA, insomnia)

#Snoring (1210)
snoring <- core_vars$f.1210.0.0
comment(snoring)<-c("Datafield = 1210")

#Daytime dozing / sleeping (1220)
day_sleepiness <- core_vars$f.1220.0.0
comment(day_sleepiness)<-c("Datafield = 1220")

#Alcohol consumed yesterday (100580)
alcohol_yesterday <- core_vars$f.100580.0.0
comment(alcohol_yesterday)<-c("Datafield = 100580")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sleep_duration, getting_up, chronotype, day_naps, insomnia,
                    snoring, day_sleepiness, alcohol_yesterday)

#remove environment variables no longer needed outside of dataframe
rm(sleep_duration, getting_up, chronotype, day_naps, day_sleepiness, insomnia,
   snoring, alcohol_yesterday)




## Physical Measures #############

#BMI (21001)
BMI <- core_vars$f.21001.0.0
comment(BMI)<-c("Datafield = 21001.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, BMI)

#remove environment variables no longer needed outside of dataframe
rm(BMI)



### Save core variables 'UKB_master' as R datafile ######
save(UKB_master,file="UKB_master.Rda")

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))



