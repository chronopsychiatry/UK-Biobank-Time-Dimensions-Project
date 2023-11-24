library(dplyr)

# Start measuring time
#timing <- system.time({

bds<-bd[c(1:100),]
#bds<-bd[c(189),]

# Number of data frames you want to create
num_eids <- nrow(bds)

#empty list to store job dataframe for each participant
life_jobtable <- list()

#dataframe to store final results
shiftwork<-data.frame()

# Generate and name data frames with iteration numbers
for (row in 1:num_eids) {         #this loop extracts the data for each job for one row (eid)
  
      #make timeline dataframes for results for each eid (cleared when 1-39 jobs finished)
      timeline_eid <- data.frame()
      row_data <- bds[row,]  # Extract data from the current row
      
      if (is.na(row_data[2])) { #check if there is data in the born col, if not the eid was not in the work survey
        next  # next eid
      }
      
      # get separate jobs 1-39 by iterating over columns where only the last digit changes - this loop will exit when reach a start date that doesn't exist
      
      for (i in 0:39) {
            
              # get all the data for each job for this eid
              
        
              #  coding of job type 4-digit SOC2000 coding df 22617
              #   
              # Major groups used to make a factor variable
              # 
              # Level   Label         Description
              # 1       Managers      Managers and Senior Officials
              # 2       Prof          Professional Occupations
              # 3       Assoc_prof    Associate Professional and Technical Occupations
              # 4       Admin         Administrative and Secretarial Occupations
              # 5       Trades        Skilled Trades Occupations
              # 6       Service       Personal Service Occupations
              # 7       Sales         Sales and Customer Service Occupations
              # 8       Machine       Process, Plant and Machine Operatives
              # 9       Element       Elementary Occupations
              
              SOC2000_major <- row_data[[paste("f.22617.0.", i, sep = "")]]
              SOC2000_major <- substring(SOC2000_major,1,1)
        
              job_occupation <- factor(SOC2000_major,
                                       labels = c('Managers',  'Prof', 'Assoc_prof',  'Admin', 'Trades', 'Service',  'Sales', 'Machine', 'Element' ),      
                                       levels = c(1:9))
              job_number <- i
              
              start <- row_data[[paste("f.22602.0.", i, sep = "")]]
              if (is.na(start)){ #check if it is the last job
                  break  # This will end the loop when last job is reached
                  }
              
              finish <- row_data[[paste("f.22603.0.", i, sep = "")]]
              finish <- ifelse(finish==-313, 2015,finish)
              #value -313 (Ongoing when data entered) change to 2015
              
              #born <- bd[[paste("f.22200.0.", i, sep = "")]] #this doesn't work?
              born <- row_data[,2]
              eid <- row_data$f.eid
              
              years <- finish-start # total years in job, but not all of these were shiftwork for everyone but always work, not gaps
              
              #how many hours per week (categorical)
              hr_wk_cat <- row_data[[paste("f.22604.0.", i, sep = "")]]        
              # -1520	15 to less-than-20 hours
              # -2030	20 to less-than-30 hours
              # -3040	30 to 40 hours
              # 4000	Over 40 hours
              
              
              #how many hours per week (numeric)
              hr_wk_num <- row_data[[paste("f.22605.0.", i, sep = "")]]        
              
              # Replace the numeric variable based on conditions - here we are inputing missing data.  This involves assuming the midpoint of each category
              if (is.na(hr_wk_num) && !is.na(hr_wk_cat)) {
                if (hr_wk_cat == "15 to less-than-20 hours") {
                  hr_wk <- 18
                } else if (hr_wk_cat == "20 to less-than-30 hours") {
                  hr_wk <- 25
                } else if (hr_wk_cat == "30 to 40 hours") {
                  hr_wk <- 35
                } else if (hr_wk_cat == "Over 40 hours") {
                  hr_wk <- 45
                }
              }             
              
             #if both numeric and categorical hours per week are NA, then assume 35h.  All eids in this loop are jobs based on years start and finish.  Gaps are not               included so it is safe to recode all the NAs.  Only people that 
              if (is.na(hr_wk_num) && is.na(hr_wk_cat)) {
                hr_wk <- 35
              }
              
              #hours per year for this job
              hr_yr <- hr_wk * 52
              
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
              #get the percent shiftwork per year for this job.  1 means the full year was spent in shiftwork or 0.5 less than one year.  year/night_yr is the                    percent when not all job was shiftwork. night_yr is the number of years out of total years in this job that were shiftwork.  This is risky, what if                 someone worked one year of shiftwork for the first year of a 10 year post?  Safer to re-code a job with less than 40% SW as not SW and more than 40%                as SW.  
              
              
              #check percent_SW - if this is < 0.4 then recode to not SW, if > 0.4 then recode to SW.  Now there are only shiftworkers and not shiftworkers.
              percent_SW <- years/years #default is total SW
              percent_yr_SW <- ifelse(SW_type == "notSW", 100,
                                  ifelse(night=="Shift pattern was worked for some (but not all) of job", night_yr/years,
                                      ifelse(day=="Shift pattern was worked for some (but not all) of job", day_yr/years,
                                             ifelse(mix=="Shift pattern was worked for some (but not all) of job", mix_yr/years,
                                                   percent_SW))))
              SW_type <- ifelse (percent_yr_SW < 0.4,"notSW", SW_type)
              
              
              #get dose of shiftwork in hours of night shift per year for this job
              NS_hrs_yr_night <- night_NS_per_m * night_NS_length * 12 
              NS_hrs_yr_mix <- mix_NS_per_m * mix_NS_length * 12 
              dose_SW_NS_hours <- max(NS_hrs_yr_night, NS_hrs_yr_mix)
      
              #sequence of years in job
              timeline <- data.frame(seq(start,finish))
              colnames(timeline)[1] <-"year_seq"
              timeline$work_type <- factor(SW_type)
              timeline$dose_NS <- dose_SW_NS_hours
              timeline$hr_yr <- hr_yr
              timeline$job_occupation <- job_occupation
              timeline$job_number <- job_number
      
              
              #it is not possible to know when the shiftwork happened within the years worked on a job when the total years were not all   shiftwork
              #the only way to deal with this is to use a percentage of the total time, to derive a % shiftwork per year, and number of hours of night shift per                  year.  It is not possible from the percent_yr_SW column to know what type of shiftwork other than 0 is not shiftwork.  But type column contains this                information
          
              #gaps are a problem, haven't been coded - assume that if no information given in job fields then the person was not working for that year
              
              #change year sequence to age sequence
              timeline$age <- timeline$year_seq - born
              
              #add the timeline to master timeline for each eid until all 39 jobs have been processed
              timeline_eid <- rbind(timeline_eid,timeline)
      }
   
      # now all job information has been recovered rename with eid
      newname <- paste("SW",eid, sep="")
      
      # Group the data by age year and summarize the data
        # timeline_eid_sum <- timeline_eid %>%
        # group_by(age) %>%
        # summarize(
        #   work_type = paste(work_type, collapse = ", "), # Concatenate work_type values
        #   dose_NS_yr = sum(dose_NS), # Sum the NS hours for all jobs
        #   hr_yr = sum(hr_yr), # Sum the hrs per year for all jobs
        #   job_occupation = paste(job_occupation, collapse = ", "), # Concatenate work_type values
        # ) %>%
        # ungroup()
      timeline_eid <- data.frame(timeline_eid)
      assign(newname, timeline_eid)
      
      # add each data table with a list of lifetime shiftwork to a list
      life_jobtable[[newname]] <- get(newname)
      
      }

#timing check
#})
# Print the timing results
#print(timing)


#the life_jobtable is a list of dataframes of all jobs history for all eids.  The next step is to summarise this information for each age bracket and over the lifespan for each participant

###  find some that were mix, day and night shift
#SW = bd$f.22620.0.0[189]
#days = bd$f.22630.0.0[189]
#mix = bd$f.22640.0.0[576]
#night = bd$f.22650.0.0[200]



###########  table to summarise type of shiftwork, years done and dose exposure to NSW at each age bracket  ###################

age_brackets <- c("15-20", "21-25", "26-30","31-35","36-40", "41-45", "46-50","51-55","56-60","61-65")
i=0 #counter
shiftwork <- data.frame()

#this loops through all the jobs within each age bracket for each participant and extracts the job code for the shiftwork jobs 
for(t in life_jobtable) { #note t is the table of jobs for each participant
 # start a counter
      i=i+1
      
   for(agebracket in age_brackets) {
     
      #define start and end of age bracket
      start_age <- as.numeric(substring(agebracket,1,2))
      end_age <- as.numeric(substring(agebracket,4,5))
      
      # make a df out of each table of job histories for each age bracket
      t <-data.frame(t)
      colnames(t) <- c("year_seq", "work_type", "dose_NS", "hr_yr","job_occupation" ,"job_number","age")
      
      # Use the subset function to select rows within the specified range defining the brackets
      subset_df <- subset(t, t$age >= start_age & t$age <= end_age-1)
      
      # Calculate the sum of the hours worked column within the specified range.  If NA, then they were not working
      bracket_total_hr <- sum(subset_df$hr_yr,na.rm = TRUE)
      bracket_total_hr_daySW <- sum(subset(subset_df, work_type=="daySW")$hr_yr)
      bracket_total_hr_nightSW <- sum(subset(subset_df, work_type=="nightSW")$hr_yr)
      bracket_total_hr_mixSW <- sum(subset(subset_df, work_type=="mixSW")$hr_yr)
      bracket_total_hr_SW <- (bracket_total_hr_daySW + bracket_total_hr_nightSW +  bracket_total_hr_mixSW)
      
      #shiftwork per year of work in each bracket
      bracket_SW_per_work <- (bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW)/(bracket_total_hr)
      bracket_nightSW_per_work <- (bracket_total_hr_nightSW)/(bracket_total_hr)
      bracket_daySW_per_work <- (bracket_total_hr_daySW)/(bracket_total_hr)      
      bracket_mixSW_per_work <- (bracket_total_hr_mixSW)/(bracket_total_hr)      
      
      # Estimate the most freq occupation during this bracket.  There may have been two kind of jobs, and a mix of SW/NonSW
      SW_job_codes <- subset(subset_df, work_type=="daySW" | work_type=="mixSW" | work_type=="nightSW")
      occupation <- table(SW_job_codes$job_occupation)
      max_frequency <- max(occupation)

      # Find all indices with the maximum frequency
      max_indices <- which(occupation == max_frequency)

      # Use the names function to retrieve the most common values
      bracket_SW_occup <- names(occupation[max_indices])
      
      # get data into a vector to rbind to main dataframe
      eid <- names(life_jobtable)[i]
      eid <- substr(eid, nchar(eid) - 6, nchar(eid))
      
      #add the information for this age bracket to the big dataframe
      
      newdata <- data.frame(eid,agebracket, 
                            bracket_total_hr,
                            bracket_total_hr_daySW,
                            bracket_total_hr_nightSW,
                            bracket_total_hr_mixSW,
                            bracket_SW_occup, 
                            bracket_SW_per_work, 
                            bracket_nightSW_per_work, 
                            bracket_daySW_per_work,       
                            bracket_mixSW_per_work 
                            )
      
      shiftwork <- rbind(shiftwork, newdata)
      
   }
}   
      
# this creates a vector of most frequent occupation categories.  Should we restrict to shiftwork jobs only?  Not necessary non shiftwork is included? Job occupation is the most frequent category in an age bracket



