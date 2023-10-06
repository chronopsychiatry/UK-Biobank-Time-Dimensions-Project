#script to extract photoperiod rate of change from UK Biobank data
# script authors: Amber Roguski

#REQUIRED data inputs: UKB_master from get_var
#query: do we need to account for daylight savings time in insol daylength function?
#query: what is the most useful way to conceptualise rate of change? minutes? min/24? percentage change?

#install libraries
library(tidyverse)
library(dplyr)
library(insol)

#create table of relevant variables
ph_vars <- data.frame(UKB_master$eid) 
date <- UKB_master$assess_date
lat <- UKB_master$lat
long <- UKB_master$long
ph_vars <- cbind(ph_vars,date,lat, long)
ph_vars$julian_day <- as.numeric(NA)
ph_vars$prev_julian_day <- as.numeric(NA)
ph_vars$daylength_min <- as.numeric(NA)
ph_vars$prev_daylength_min <- as.numeric(NA)
ph_vars$rate_of_change <- as.numeric(NA)

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
  rate_of_change <- dl - prev_dl
  ph_vars$rate_of_change[i] <- rate_of_change
}





##### test section ############
#make smaller ph_vars dataset for trialling
small_ph_vars <- ph_vars[1:10,]
small_ph_vars$julian_day <- as.numeric(NA)
small_ph_vars$prev_julian_day <- as.numeric(NA)
small_ph_vars$daylength_min <- as.numeric(NA)
small_ph_vars$prev_daylength_min <- as.numeric(NA)
small_ph_vars$rate_of_change <- as.numeric(NA)

#change date format to POSIX so insol functions work
small_ph_vars$date <- as.POSIXct(small_ph_vars$date,format="%Y-%m-%d")

#for loop to calculate rate of change for each date
for (i in 1:nrow(small_ph_vars)){
  row <- small_ph_vars[i,]
  
  #convert assess_date to julian day format
  julian_day <- JD(row$date, inverse=FALSE)
  #print(row$date)
  row$julian_day <- julian_day
  #print(row)
  small_ph_vars$julian_day[i] <- julian_day
  
  #identify n-1 and join into table
  prev_julian_day <- JD(row$date, inverse=FALSE)-1
  small_ph_vars$prev_julian_day[i] <- prev_julian_day
  
  #use insol package to calculate photoperiod for assessment date and n-1
  daylength_out <- daylength(row$lat, row$long, julian_day, tmz=0)
  dl <- as.numeric(daylength_out[1,3])*60
  small_ph_vars$daylength_min[i] <- dl
  
  prev_daylength <- daylength(row$lat, row$long, prev_julian_day, tmz=0)
  prev_dl <- as.numeric(prev_daylength[1,3])*60
  small_ph_vars$prev_daylength_min[i] <- prev_dl
  
  # calculate photoperiod rate of change (minutes) https://www.math.unl.edu/~bharbourne1/M106/projects/Proj1soln.html
  rate_of_change <- dl - prev_dl
  small_ph_vars$rate_of_change[i] <- rate_of_change
}
