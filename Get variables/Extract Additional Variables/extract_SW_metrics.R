library(dplyr)

bds<-bd[c(189,576,200),]

# Number of data frames you want to create
num_eids <- nrow(bds)

#empty list to store job dataframe for each participant
life_jobtable <- list()

# Generate and name data frames with iteration numbers
for (row in 1:num_eids) {         #this loop extracts the data for each job for one row (eid)
  
      #make timeline dataframes for results for each eid (cleared when 1-39 jobs finished)
      timeline_eid <- data.frame()
      row_data <- bds[row,]  # Extract data from the current row
  
      if (is.na(row_data[2])) {
        next  # next eid
      }
      
      # get separate jobs 1-39 by iterating over columns where only the last digit changes - this loop will exit when reach a start date              that doesn't exist
      
      for (i in 0:39) {
            
              # get all the data for each job for this eid
              
              start <- row_data[[paste("f.22602.0.", i, sep = "")]]
              if (is.na(start)){ #check if it is the last job
                  break  # This will end the loop when last job is reached
                  }
              
              finish <- row_data[[paste("f.22603.0.", i, sep = "")]]
              finish <- ifelse(finish==-313, 2015,finish)
              #value -313 (Ongoing when data entered) 2015
              
              #born <- bd[[paste("f.22200.0.", i, sep = "")]] #this doesn't work?
              born <- row_data[,2]
      
              eid <- row_data$f.eid
              
              #was it a shiftwork job YN
              SW_YN <- row_data[[paste("f.22620.0.", i, sep = "")]]
    
              #did they do dayshift and how many years within this job[i]
              day <- row_data[[paste("f.22630.0.", i, sep = "")]]
              day_yr <- row_data[[paste("f.22631.0.", i, sep = "")]]
              
              #did they do mixed-shifts and years, night shift duration and number per month for this job [i]
              mix <- row_data[[paste("f.22640.0.", i, sep = "")]]
              mix_yr <- row_data[[paste("f.22641.0.", i, sep = "")]]
              mix_NS_per_m <- row_data[[paste("f.22643.0.", i, sep = "")]]
              mix_NS_length <- row_data[[paste("f.22642.0.", i, sep = "")]]
              
              #did they do night-shifts and years, night shift duration and number per month for this job [i]
              night <- row_data[[paste("f.22650.0.", i, sep = "")]]
              night_yr <- row_data[[paste("f.22651.0.", i, sep = "")]]
              night_NS_per_m <- row_data[[paste("f.22653.0.", i, sep = "")]]
              night_NS_length <- row_data[[paste("f.226652.0.", i, sep = "")]]
           
              #replace -1001 with 0.5 years
              night_yr <- ifelse(night_yr==-1001, 0.5,night_yr)
              day_yr <- ifelse(day_yr==-1001, 0.5, day_yr)
              mix_yr <- ifelse(mix_yr==-1001, 0.5, mix_yr)
              
              #categorical variable for shiftwork type in this job - don't understand why coding doesn't total?
              SW_type <- ifelse(SW_YN == "No", "notSW",
                                ifelse(mix == "Shift pattern was worked for whole of job", "mixSW",
                                       ifelse(mix == "Shift pattern was worked for some (but not all) of job", "mixSW",
                                              ifelse(night == "Shift pattern was worked for whole of job", "nightSW",
                                                     ifelse(night == "Shift pattern was worked for some (but not all) of job", "nightSW",
                                                            ifelse(day == "Shift pattern was worked for whole of job", "daySW", 
                                                                   ifelse(day == "Shift pattern was worked for some (but not all) of job", "daySW", "NA")
                                                            )        )      )    )))
              
              #get the dose of shiftwork per year for this job
              years <- finish-start # total years in job, but not all of these were shiftwork for everyone
              
              #get the percent shiftwork per year for this job.  1 means the full year was spent in shiftwork or 0.5 less than one year.  year/night_yr is the                    percent when not all job was shiftwork. night_yr is the number of years out of total years in this job that were shiftwork
              percent_SW <- years/years
              percent_SW <- ifelse(night=="Shift pattern was worked for some (but not all) of job", night_yr/years,
                                   ifelse(day=="Shift pattern was worked for some (but not all) of job", day_yr/years,
                                            ifelse(mix=="Shift pattern was worked for some (but not all) of job", mix_yr/years,
                                                   percent_SW)))
              
              #get dose of shiftwork in hours of night shift per year for this job
              NS_hrs_yr_night <- night_NS_per_m * night_NS_length * 12 
              NS_hrs_yr_mix <- mix_NS_per_m * mix_NS_length * 12 
              dose_SW_NS_hours <- max(NS_hrs_yr_night, NS_hrs_yr_mix)
      
              #sequence of years in job
              timeline <- data.frame(seq(start,finish))
              colnames(timeline)[1] <-"year_seq"
              timeline$year_SW <- percent_SW
              timeline$type <- factor(SW_type)
              timeline$dose_NS <- dose_SW_NS_hours
      
              
              #it is not possible to know when the shiftwork happened within the years worked on a job when the total years were not all   shiftwork
              #the only way to deal with this is to use a percentage of the total time, to derive a % shiftwork per year, and number of hours of night shift per year
          
              #change year sequence to age sequence
              timeline$age <- timeline$year_seq - born
              
              #add the timeline to master timeline for each eid until all 39 jobs have been processed
              timeline_eid <- rbind(timeline_eid,timeline)
              
      }
      
      # now all job information has been recovered rename with eid
      newname <- paste("SW",eid, sep="")
      assign(newname, timeline_eid)
      
      # add each data table with a list of lifetime shiftwork to a list
      life_jobtable[[newname]] <- get(newname)
}


###  find some that were mix, day and night shift
SW = bd$f.22620.0.0[189]
days = bd$f.22630.0.0[189]
mix = bd$f.22640.0.0[576]
night = bd$f.22650.0.0[200]

#was it a shiftwork job YN
SW_YN <- row_data[["f.22620.0.1"]]


###########  table to summarise years and dose exposure to SW at each age bracket  ###################

age_brackets <- c("15-20", "21-25", "26-30","31-35","36-40", "41-45", "46-50","51-55","56-60","61-65")
i=0

#dataframe to store 
shiftwork<-data.frame()

for(t in life_jobtable) {
 #make a counter
      i=i+1
      
   for(bracket in age_brackets) {
     
      #define age bracket
      start_age <- as.numeric(substring(bracket,1,2))
      end_age <- as.numeric(substring(bracket,4,5))
      
      # make a df out of each table of job histories
      t <-data.frame(t)
      colnames(t) <- c("year_seq", "year_SW", "type", "dose_NS", "age")
      
      # Use the subset function to select rows within the specified range
      subset_df <- subset(t, t$age >= start_age & t$age <= end_age-1)
      
      # Calculate the sum of the years of shiftwork column within the specified range
      bracket_year_SW <- sum(subset_df$year_SW)
      
      # Calculate the sum of the dose of night shiftwork column within the specified range
      bracket_dose_NS <- sum(subset_df$dose_NS)
      
      # if there is a mixture of day and night shifts and non-shiftworker within age bracket, recode to mixed
      if (length(unique(subset_df$type)) > 1) {
        subset_df$type <- "mixSW" # if there is overlap between the types, default mix
         }
      
      type <- as.character(subset_df$type[1])
              
      # get data into a vector to rbind to main dataframe
      eid <- names(life_jobtable)[i]
      eid <- substr(eid, nchar(eid) - 6, nchar(eid))
      
      # add result to vector with values of SW dose and years for age bracket
      newrow <- (c(eid, bracket, bracket_dose_NS, bracket_year_SW, type)) 
      shiftwork <- rbind(shiftwork, newrow) 
      colnames(shiftwork) <- c("eid", "age_bracket", "bracket_dose_NS", "bracket_year_SW", "type")
    }  
}
