install.packages("tidyverse")
library(dplyr)

# Number of data frames you want to create
num_eids <- nrow(bd)

# Create a list to store data frames with all jobs for each eid
eid_job_list <- vector("list", num_eids)

# Generate and name data frames with iteration numbers
for (row in 1:num_eids) {         #this loop extracts the data for each job for one row (eid)

      #make timeline dataframe for results for each eid (cleared when 1-39 jobs finished)
      timeline <- data.frame()

          # get separate jobs 1-39 by iterating over columns where only the last digit changes 
          for (i in 0:39) {
  
              #get all the data for each job
              start <- paste("bd$f.22602.0.", i, sep = "")
              finish <- paste("bd$f.22603.0.", i, sep = "")
              born <- paste("bd$f.22200.0.", i, sep = "")
              eid <- bd$f.eid
              
              #was it a shiftwork job YN
              SW_YN <- paste("bd$f.22620.0.", i, sep = "")
    
              #did they do dayshift and how many years within this job[i]
              day <- paste("bd$f.22630.0.", i, sep = "")
              day_yr <- paste("bd$f.22631.0.", i, sep = "")
              
              #did they do mixed-shifts and years, night shift duration and number per month for this job [i]
              mix <- paste("bd$f.22640.0.", i, sep = "")
              mix_yr <- paste("bd$f.22641.0.", i, sep = "")
              mix_NS_per_m <- paste("bd$f.22643.0.", i, sep = "")
              mix_NS_length <- paste("bd$f.22642.0.", i, sep = "")
              
              #did they do night-shifts and years, night shift duration and number per month for this job [i]
              night <- paste("bd$f.22650.0.", i, sep = "")
              night_yr <- paste("bd$f.22651.0.", i, sep = "")
              night_NS_per_m <- paste("bd$f.22653.0.", i, sep = "")
              night_NS_length <- paste("bd$f.226652.0.", i, sep = "")
           
              #replace -1001 with 0.5 years
              which(night_yr==-1001)<-0.5
              which(day_yr==-1001)<-0.5
              which(mix_yr==-1001)<-0.5
      
              #get the dose of shiftwork per year for this job
              years <- finish-start # total years in job, but not all of these were shiftwork for everyone
              years_NSW <- min(years/years,night_yr/years) #percentage night shiftwork per year for this job
              years_DSW <- min(years/years, day_yr/years) #percentage day shiftwork per year for this job
              years_MSW <- min (years/years, mix_yr/years) #percentage mixed shiftwork per year for this job
              dose_SW_years <- max (years_NSW, years_DSW, years_MSW)
      
              #categorical variable for shiftwork type in this job - don't understand why coding doesn't total?
              SW_type <- data.frame()
              SW_type %>%
                 mutate(SW_type = case_when(
                 SW_YN == "0" ~ "notSW",
                 mix == "1" ~ "mixSW",
                 night == "1" ~ "nightSW",
                 day == "1" ~ "daySW",
                 TRUE ~ "NA"  # Default category if none of the conditions are met
                 ))
            
              #get dose of shiftwork in hours per year for this job
              NS_hrs_yr_night <- night_NS_per_m * night_NS_length * 12 
              NS_hrs_yr_mix <- mix_NS_per_m * mix_NS_length * 12 
              dose_SW_NS_hours <- max(NS_hrs_yr_night, NS_hrs_yr_mix)
      
              #sequence of years in job
              timeline <- data.frame(seq(finish,start))
              colnames(timeline)[1] <-"year_seq"
              timeline$year_SW <- dose_SW_years
              timeline$type <- SW_type
              timeline$dose <- dose_SW_NS_hours
      
              #it is not possible to know when the shiftwork happened within the years worked on a job when the total years were not all shiftwork
              #the only way to deal with this is to use a percentage of the total time, to derive a % shiftwork per year, and number of hours of night shift per year
          
              #change year sequence to age sequence
              timeline$age <- timeline$year_seq - born
      
              #add the timeline to master timeline for each eid until all 39 jobs have been processed
              timeline_eid <- rbind(timeline_eid,timeline)
              }
    
    df_list[[row]] <- timeline_eid
    names(df_list)[row] <- paste(eid "_SW", sep = "")
  }

}
  
  
  
  ###########  getting the age brackets out  ###################
  
  # Define the year range you want to sum
  start_age <- 20
  end_age <- 22

  # Use the subset function to select rows within the specified range
  subset_df <- subset(year_seq, age >= start_age & age <= end_age-1)

# Calculate the sum of the years of shiftwork column within the specified range
total_sum <- sum(subset_df$year_SW)

#add result to new dataframe with values of SW dose and years for each age bracket
shiftwork <- data.frame()
colnames(shiftwork) <- c("eid", "age_bracket", "total_NS", "years","nightSW","daySW","mixSW","not_SW")

