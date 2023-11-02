#script to extract variables of interest from R-format UK Biobank data
# script authors: Cathy Wyse & Amber Roguski
# last updated: 20.10.23 agpr141

#install libraries
library(tidyverse)
library(dplyr)
library(insol)
library(pillar)
library(readxl)
library(hms)

### R extract & data prep #########
#set cwd to folder you want to do your analysis in
setwd("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\core dataset")

#load in core Biobank dataset from folder (change path to match your own, make sure the '_core.r' file has correct path also)
source("./Core vars/ukb673864_core.r")

#rename default 'bd' dataframe as core_vars
core_vars <- bd
rm(bd)

# withdraw latest participants (download latest withdraw list from basket)
withdrawn <- read.csv("./Withdrawals/withdraw99491_10_20231013.csv")
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

## Assessment Centre ####################

#assess_date (53)
assess_date <- core_vars$f.53.0.0
comment(assess_date)<-c("Datafield = 53.0.0")

#assess_centre (54)
assess_centre <- core_vars$f.54.0.0
comment(assess_centre)<-c("Datafield = 54.0.0")

#import table of assessment centre latitudes
Table_latitude_assesment_centres <- read.csv("./Centre Latitude Longitude/Table_latitude_assesment_centres.csv")

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

## Geographic location #################

#	Home area population density - urban or rural (20118)
urban <- core_vars$f.20118.0.0
comment(urban)<-c("Datafield = 20118.0.0")

#	Index of multiple deprivation England (26410)
deprivation_index_england <- core_vars$f.26410.0.0
comment(deprivation_index_england )<-c("Datafield = 26410.0.0")

#	Index of multiple deprivation Scotland (26427)
deprivation_index_scotland <- core_vars$f.26427.0.0
comment(deprivation_index_scotland )<-c("Datafield = 26427.0.0")

#	Index of multiple deprivation Wales (26426)
deprivation_index_wales <- core_vars$f.26426.0.0
comment(deprivation_index_wales )<-c("Datafield = 26426.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, urban, deprivation_index_england, 
                    deprivation_index_scotland, deprivation_index_wales)

#remove environment variables no longer needed outside of dataframe
rm(deprivation_index_england, 
   deprivation_index_scotland, deprivation_index_wales, urban)

## Education & Employment #############################################

#	Age completed full-time education (845)
age_completed_education <- core_vars$f.845.0.0
comment(age_completed_education)<-c("Datafield = 845")

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

#add variables to master dataframe
UKB_master <- cbind(UKB_master, adopted, birth_weight, birth_weight_metric,
                    breastfed, country_birth_uk, country_birth_nonuk, handedness,
                    maternal_smoking)

#remove environment variables no longer needed outside of dataframe
rm(adopted, birth_weight, birth_weight_metric, breastfed, country_birth_uk,
   country_birth_nonuk, maternal_smoking, handedness)

## Health  #############################################

#smoking status (20116)
smoking_status <- core_vars$f.20116.0.0
comment(smoking_status)<-c("Datafield = 20116")

#alcohol intake frequency (1558)
alcohol_intake <- core_vars$f.1558.0.0
comment(alcohol_intake)<-c("Datafield = 1558")

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

#time spent outdoors in summer (1060)
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

#Getting up in morning	(1170)
getting_up  <- core_vars$f.1170.0.0
comment(getting_up)<-c("Datafield = 1170")

#Morning/evening person (chronotype) (1180)
chronotype <- core_vars$f.1180.0.0
comment(chronotype)<-c("Datafield = 1180")

#Nap during day (1190)
day_naps <- core_vars$f.1190.0.0
comment(day_naps)<-c("Datafield = 1190")

#Sleeplessness / insomnia (1200)
insomnia <- core_vars$f.1200.0.0
comment(insomnia)<-c("Datafield = 1200")

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

#acceleration average (90012)
accel_mean <- core_vars$f.90012.0.0
comment(accel_mean)<-c("Datafield = 90012.0.0")

#systolic automated reading (4080)
systolic <- core_vars$f.4080.0.0
comment(systolic)<-c("Datafield = 4080.0.0")

#diastolic automated reading (4079)
diastolic <- core_vars$f.4079.0.0
comment(diastolic)<-c("Datafield = 4079.0.0")

#pulse (102)
pulse <- core_vars$f.102.0.0
comment(pulse)<-c("Datafield = 102.0.0")

#body fat percentage from IP (23099)
body_fat <- core_vars$f.23099.0.0
comment(body_fat)<-c("Datafield = 23099.0.0")

#BMI (21001)
BMI <- core_vars$f.21001.0.0
comment(BMI)<-c("Datafield = 21001.0.0")

#hand grip left (46)
handgrip_l <- core_vars$f.46.0.0
comment(handgrip_l)<-c("Datafield = 46")

#hand grip right (47)
handgrip_r <- core_vars$f.47.0.0
comment(handgrip_r)<-c("Datafield = 47")

#FEV (3063)
FEV <- core_vars$f.3063.0.0
comment(FEV)<-c("Datafield = 3063")

#FVV (3062)
FVC <- core_vars$f.3062.0.0
comment(FVC)<-c("Datafield = 3062")

#PEF (3064)
PEF <- core_vars$f.3064.0.0
comment(PEF)<-c("Datafield = 3064")

#add variables to master dataframe
UKB_master <- cbind(UKB_master,accel_mean, systolic, diastolic, pulse, body_fat,
                    BMI, handgrip_l, handgrip_r, FEV, FVC, PEF)

#remove environment variables no longer needed outside of dataframe
rm(accel_mean, systolic, diastolic, pulse, body_fat, BMI, handgrip_l, handgrip_r,
   FEV, FVC, PEF)

## Photoperiod Vars ####

#note!!!!!!!!!!: function to calculate photoperiod variables using below forloop
#can take up to 1.5hours to run

#create table of relevant variables
ph_vars <- data.frame(UKB_master$eid) 
date <- UKB_master$assess_date
lat <- UKB_master$assess_latitude
long <- UKB_master$assess_longitude
ph_vars <- cbind(ph_vars,date,lat, long)
ph_vars$julian_day <- as.numeric(NA)
ph_vars$prev_julian_day <- as.numeric(NA)
ph_vars$daylength_min <- as.numeric(NA)
ph_vars$prev_daylength_min <- as.numeric(NA)
ph_vars$rate_of_change_min <- as.numeric(NA)
ph_vars$rate_of_change_percent <- as.numeric(NA)

#change date format to POSIX so insol functions work
ph_vars$date <- as.POSIXct(ph_vars$date,format="%Y-%m-%d")

#for loop to calculate rate of change for each date
for (i in 1:nrow(ph_vars)){
  row <- ph_vars[i,]
  
  #convert assess_date to julian day format
  julian_day <- JD(row$date, inverse=FALSE)
  #print(row$date)
  #row$julian_day <- julian_day
  #print(row)
  ph_vars$julian_day[i] <- julian_day
  
  #identify n-1 and join into table
  prev_julian_day <- JD(row$date, inverse=FALSE)-1
  ph_vars$prev_julian_day[i] <- prev_julian_day
  
  #use insol package to calculate photoperiod for assessment date and n-1
  daylength_out <- daylength(row$lat, row$long, julian_day, tmz=0)
  dl <- as.numeric(daylength_out[1,3])*60
  ph_vars$daylength_min[i] <- dl
  
  prev_daylength <- daylength(row$lat, row$long, prev_julian_day, tmz=0)
  prev_dl <- as.numeric(prev_daylength[1,3])*60
  ph_vars$prev_daylength_min[i] <- prev_dl
  
  # calculate photoperiod rate of change (minutes)
  rate_of_change_min <- dl - prev_dl
  rate_of_change_percent <- rate_of_change_min/1440*100
  ph_vars$rate_of_change_min[i] <- rate_of_change_min
  ph_vars$rate_of_change_percent[i] <- rate_of_change_percent
}

#julian day (derived)
julian_day <- ph_vars$julian_day
comment(julian_day)<-c("derived field")

#previous julian day (derived)
prev_julian_day <- ph_vars$prev_julian_day
comment(prev_julian_day)<-c("derived field")

#daylength in minutes (derived)
daylength_min <- ph_vars$daylength_min
comment(daylength_min)<-c("derived field")

#previous daylength in minutes (derived)
prev_daylength_min <- ph_vars$prev_daylength_min
comment(prev_daylength_min)<-c("derived field")

#photoperiod rate of change in minutes (derived)
photoperiod_roc_min <- ph_vars$rate_of_change_min
comment(photoperiod_roc_min)<-c("derived field")

#photoperiod rate of change in percent (derived)
photoperiod_roc_percent <- ph_vars$rate_of_change_percent
comment(photoperiod_roc_percent)<-c("derived field")

#add variables to master dataframe
UKB_master <- cbind(UKB_master,julian_day, prev_julian_day, daylength_min,
                    prev_daylength_min, photoperiod_roc_min, photoperiod_roc_percent)

#remove environment variables no longer needed outside of dataframe
rm(date, daylength_min, dl, i, julian_day, lat, long, photoperiod_roc_min, photoperiod_roc_percent,
   prev_daylength, prev_dl, prev_julian_day, prev_daylength_min, rate_of_change_min, 
   rate_of_change_percent, row, daylength_out, ph_vars)

## Data collection timing  #############################################

source("./Sample timings/ukb673864_data_timing.r")

datetime_vars <- bd
rm(bd)

# withdraw latest participants (download latest withdraw list from basket)
datetime_vars <- datetime_vars[! datetime_vars$f.eid %in% withdrawn$X1195908,]

#date of attending assessment centre (53)
assess_date <- UKB_master$assess_date

#month of attending assessment centre
assess_month <- as.integer(format(assess_date, "%m"))    # Extract month
comment(assess_month)<-c("derived from Datafield = 53")

#year of attending assessment centre 
assess_year <- as.integer(format(assess_date, "%Y"))
comment(assess_year)<-c("derived from Datafield = 53")

#Biometrics sign-off date & time (21834)
end_biometrics <- as.POSIXct(datetime_vars$f.21834.0.0)
comment(end_biometrics)<-c("Datafield = 21834")

#Cardiac monitor sign-off timestamp (21871)
end_cardiac <- as.POSIXct(datetime_vars$f.21871.2.0)
comment (end_cardiac)<-c("Datafield = 21871")

#Carotid ultrasound sign-off timestamp (21865)
end_carotid <- as.POSIXct(datetime_vars$f.21865.2.0)
comment (end_carotid)<-c("Datafield = 21865")

#Conclusion sign-off date & time (21851)
end_signoff <- as.POSIXct(datetime_vars$f.21851.0.0)
comment (end_signoff)<-c("Datafield = 21851")

#Consent sign-off date & time (21821)
end_consent <- as.POSIXct(datetime_vars$f.21821.0.0)
comment (end_consent)<-c("Datafield = 21821")

#DXA assessment sign-off date & time (21864)
end_DXA <- as.POSIXct(datetime_vars$f.21864.2.0)
comment (end_DXA)<-c("Datafield = 21864")

#ECG at rest sign-off date & time (21866)
end_ECG <- as.POSIXct(datetime_vars$f.21866.2.0)
comment (end_ECG)<-c("Datafield = 21866")

#ECG during exercise sign-off date & time (21838)
end_ECG_exercise <- as.POSIXct(datetime_vars$f.21838.1.0)
comment (end_ECG_exercise)<-c("Datafield = 21838")

#Eye measures sign-off date & time (21836)
end_eye <- as.POSIXct(datetime_vars$f.21836.0.0)
comment (end_eye)<-c("Datafield = 21836")

#Reception sign-off date & time (21811)
end_reception <- as.POSIXct(datetime_vars$f.21811.0.0)
comment (end_reception)<-c("Datafield = 21811")

#Sample collection sign-off date & time (21842)
end_sample <- as.POSIXct(datetime_vars$f.21842.0.0)
comment (end_sample)<-c("Datafield = 21842")

#Touchscreen cognitive sign-off date & time (21825)
end_touchscreen_cog <- as.POSIXct(datetime_vars$f.21825.2.0)
comment (end_touchscreen_cog)<-c("Datafield = 21825")

#Touchscreen sign-off date & time (21822)
end_touchscreen <- as.POSIXct(datetime_vars$f.21822.0.0)
comment (end_touchscreen)<-c("Datafield = 21822")

#Urine collection sign-off date & time (21841)
end_urine <- as.POSIXct(datetime_vars$f.21841.0.0)
comment (end_urine)<-c("Datafield = 21841")

#Verbal interview sign-off date & time (21831)
end_interview <- as.POSIXct(datetime_vars$f.21831.0.0)
comment (end_interview)<-c("Datafield = 21831")

# pull out blood sample timings info 
bs_vars <- readr::read_tsv("./Sample timings/blood_datetime_3166_participant.tsv")

#datetime of day of blood sampling at assessment centre (3166)
BS_date <- as.Date(bs_vars$'3166-0.0')
comment (BS_date)<-c("Datafield = 3166")

#time of blood sample
BS_time <- as.ITime(bs_vars$'3166-0.0')
comment (BS_time)<-c("derived from Datafield = 3166")

#month of blood sample
BS_month <- as.integer(format(bs_vars$'3166-0.0', "%m"))
comment (BS_month)<-c("derived from Datafield = 3166")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, assess_month, assess_year, end_biometrics, end_signoff, 
                    end_consent, end_carotid, end_cardiac, end_DXA, 
                    end_ECG, end_ECG_exercise, 
                    end_eye, end_reception, end_sample,  
                    end_touchscreen_cog, end_touchscreen, 
                    end_urine, end_interview,
                    BS_date, BS_time, BS_month)
UKB_master <- UKB_master %>% 
  rename('BS_time' = 'V1')

#remove environment variables no longer needed outside of dataframe
rm(assess_month, assess_year, assess_date, BS_date, BS_time, 
   BS_month, end_biometrics, end_signoff, end_consent, end_DXA, 
   end_ECG, end_ECG_exercise, end_eye,end_reception,end_sample,  
   end_touchscreen_cog, end_touchscreen, end_urine, end_interview, bs_vars,
   core_vars, datetime_vars, BS_time, BS_date, BS_month)



## Assessment Centre Weather #############################################

# not behaving as expected currently

# extract weather info
temp_table <- read_excel("./Weather/weather_ukb_270923.xlsx", sheet = "monthly temp")
weather_stations <- read_excel("./Weather/weather_ukb_270923.xlsx", sheet = "ukb centres - weather stations")
colnames(weather_stations) <- c("station","assess_centre")

#merge weather stations and centre
UKB_master <- merge(UKB_master,weather_stations, by="assess_centre")

#merge dataframes based on year and month and centre
UKB_master <- merge(UKB_master, temp_table, by = c("assess_year", "station", "assess_month"), all.x = TRUE)


### Save core variables 'UKB_master' as R datafile ######
save(UKB_master,file="UKB_master.Rda")

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))



