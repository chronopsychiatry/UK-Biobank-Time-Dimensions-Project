library(tidyverse)
library(dplyr)
library(readr)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\Metabolite Analyses\\analysis_met.Rda")

# get blood sample timings
bs_vars <- readr::read_tsv("C:/Users/arogusk2/OneDrive - University of Edinburgh./HELIOS-BD/Side Projects/BioBank Project/core dataset/Sample timings/blood_datetime_3166_participant.tsv")

#rename variable
bs_vars <- bs_vars %>% 
  rename('BS_datetime' = '3166-0.0')

# initialise columns
bs_vars$bs_year <- as.numeric(NA)
bs_vars$bs_month <- as.numeric(NA)
bs_vars$bs_mday <- as.numeric(NA)
bs_vars$bs_wday <- as.numeric(NA)
bs_vars$bs_time <- as.numeric(NA)

#create forloop to pull out year, month, time and day of week
for (i in 1:nrow(bs_vars)){
  row <- bs_vars[i,]
  
  #convert to POSIXlt
  lt <- as.POSIXlt(bs_vars$BS_datetime[i])
  
  #pull out year from data (lt format = # years since 1900)
  year <- 1900+lt$year
  bs_vars$bs_year[i] <- year
  
  # pull out month (zero indexed, add 1)
  month <- 1+lt$mon
  bs_vars$bs_month[i] <- month
  
  # pull out day of the month
  mday <- lt$mday
  bs_vars$bs_mday[i] <- mday
  
  # pull out day of the week
  wday <- lt$wday
  bs_vars$bs_wday[i] <- wday
  
  # pull out ToD
  time <- format(as.POSIXct(bs_vars$BS_datetime[i]), format = "%H:%M")
  bs_vars$bs_time[i] <- time
}

#merge bs_vars into main df
study_population <- merge(analysis_met,bs_vars, by="eid")

#save new df
save(study_population,file="study_population.Rda")
