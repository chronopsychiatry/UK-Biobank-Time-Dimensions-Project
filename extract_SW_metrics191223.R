library(dplyr)
library(RColorBrewer)
library(viridis)
library(ggsci)
library(data.table)

#import shiftwork data ukb675080
bd <- read.table("C:\\Users\\Admin\\OneDrive - Maynooth University\\UK Biobank Shiftwork\\helper\\ukb675080.tab", header=TRUE, sep="\t")


### R extract & data prep #########
setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

#import the downloaded data and run encoding R code
script_path2 <- file.path(getwd(), "helper", "ukb675080.r")

# Run the script
source(script_path2)

bds <- bd[bd$f.eid %in% geteid, ]


#get data for practising
#x <-t(bd[200,])
# x <- shiftwork
# x <- t(bds)
# write.csv(x,file="SW1000326b")
# bd[595,1]

###  find some that were mix, day and night shift
# get_these <- c(189, 576, 200, 595) 
#one random 130

#bds<-bd[c(1:393760),]
#bds<-bd[c(1:50000),]

#SW = bd$f.22620.0.0[189]
#days = bd$f.22630.0.0[189]
#mix = bd$f.22640.0.0[576]
#night = bd$f.22650.0.0[200]

# Number of data frames you want to create
num_eids <- nrow(bds)

#empty list to store job dataframe for each participant
life_jobtable <- list()

#empty list to store job dataframe for each age bracket for each participant
life_jobtable_agebrackets <- list()

#dataframe to store final results
shiftwork<-data.frame()

setwd("C:/Users/Admin/Documents/jobtable_docu") # for local storage of csv
setwd("C:/Users/Admin/Documents/test") # for local storage of csv

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
              
        
              SOC2000_major <- row_data[[paste("f.22617.0.", i, sep = "")]] #Job code - historical
              SOC2000_major <- substring(SOC2000_major,1,1)
        
              job_occupation <- factor(SOC2000_major,
                                       labels = c('Managers',  'Prof', 'Assoc_prof',  'Admin', 'Trades', 'Service',  'Sales', 'Machine', 'Element' ),      
                                       levels = c(1:9))
              job_number <- i
              
              start <- row_data[[paste("f.22602.0.", i, sep = "")]] #Year job started
              if (is.na(start)){ #check if it is the last job
                  break  # This will end the loop when last job is reached
                  }
              
              finish <- row_data[[paste("f.22603.0.", i, sep = "")]] #Year job finished
              finish <- ifelse(finish==-313, 2015,finish)
              #value -313 (Ongoing when data entered) change to 2015
              
              born <- row_data[,2]
              eid <- row_data$f.eid
              
              years <- finish-start+1 # total years in job, but not all of these were shiftwork for everyone but always work, not gaps
              
              #how many hours per week (categorical)
              hr_wk_cat <- row_data[[paste("f.22604.0.", i, sep = "")]]        
              # -1520	15 to less-than-20 hours
              # -2030	20 to less-than-30 hours
              # -3040	30 to 40 hours
              # 4000	Over 40 hours
              
              #how many hours per week (numeric)
              hr_wk_num <- row_data[[paste("f.22605.0.", i, sep = "")]]        
              if (is.null(hr_wk_num)) {
                hr_wk_num <- NA
              }
              
              # Replace the numeric variable based on conditions - here we are inputing missing data.  This involves assuming the midpoint of each category
              if (is.na(hr_wk_num) && !is.na(hr_wk_cat)) {
                if (hr_wk_cat == "15 to less-than-20 hours") {
                  hr_wk_num  <- 18
                } else if (hr_wk_cat == "20 to less-than-30 hours") {
                  hr_wk_num  <- 25
                } else if (hr_wk_cat == "30 to 40 hours") {
                  hr_wk_num  <- 35
                } else if (hr_wk_cat == "Over 40 hours") {
                  hr_wk_num  <- 45
                }
              }             
              
             #if both numeric and categorical hours per week are NA, then assume 35h.  All eids in this loop are jobs based on years start and finish.  Gaps are not included so it is safe to recode all the NAs.  
              if (is.na(hr_wk_num) && is.na(hr_wk_cat)) {
                hr_wk_num <- 35
              }
               
              #hours per year for this job
              hr_yr <- hr_wk_num * 52
              
              #was it a shiftwork job YN
              SW_YN <- row_data[[paste("f.22620.0.", i, sep = "")]]  	#Job involved shift work
             
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
              night_NS_length <- row_data[[paste("f.22652.0.", i, sep = "")]]
           
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
              dose_SW_NS_hours <- max(NS_hrs_yr_night, NS_hrs_yr_mix, na.rm = TRUE)
      
              #sequence of years in job
              timeline <- data.frame(seq(start,finish))
              colnames(timeline)[1] <-"year_seq"
              timeline$work_type <- factor(SW_type)
              timeline$dose_NS <- dose_SW_NS_hours
              timeline$hr_yr <- hr_yr
              timeline$job_occupation <- job_occupation
              timeline$job_number <- job_number
              timeline$SW_YN <- SW_YN
      
              
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
      
      timeline_eid <- data.frame(timeline_eid)
      assign(newname, timeline_eid)
      
      # Save as CSV file
      fwrite(timeline_eid, file = paste0(newname, ".csv"))
      
      print(eid)
      
      rm(list = ls()[grepl("SW", ls())])
      
      }

#The next step is to summarise this information for each age bracket and over the lifespan for each participant



###########  table to summarise type of shiftwork, years done and dose exposure to NSW at each age bracket  ###################

age_brackets <- c("15-20", "21-25", "26-30","31-35","36-40", "41-45", "46-50","51-55","56-60","61-65")
i=100000 #counter

shiftwork <- data.frame()

#this loops through all the jobs within each age bracket for each participant and extracts the job code for the shiftwork jobs 

filelist <- list.files(path = "C:/Users/Admin/Documents/jobtable_docu", pattern = "\\.csv$", full.names = TRUE)

filelist <- filelist[c(100001:120271)]

#quick open each csv
for(file in filelist) { #note filelist of ts, where t is the table of jobs for each participant
 # start a counter
      i=i+1
      t <- fread(file)
  #if end loop
        if (all(is.na(t))) {
        print("Dataframe is empty. goto next")
        next  # This will go to next file
      }
      
  # print debugging information
  #print(paste("Processing participant:", "in age bracket:", agebracket))
    
   for(agebracket in age_brackets) {
     
      #define start and end of age bracket
      start_age <- as.numeric(substring(agebracket,1,2))
      end_age <- as.numeric(substring(agebracket,4,5))
      
      # #exit the loop if the participant has not yet reached the age bracket
        #if t$age < start_age
       # next  # end the loop next participant
      #}
      
      # make a df out of each table of job histories for each age bracket
      t <-data.frame(t)
      colnames(t) <- c("year_seq", "work_type", "dose_NS", "hr_yr","job_occupation" ,"job_number","SW_YN","age")
      
      # Use the subset function to select rows within the specified range defining the brackets
      subset_df <- subset(t, t$age >= start_age & t$age <= end_age)
    
      # Calculate the sum of the hours worked column within the specified range.  If NA, then they were not working
      bracket_total_hr <- sum(as.numeric((subset_df$hr_yr),na.rm = TRUE))
      bracket_total_hr_daySW <- sum(as.numeric(subset(subset_df, work_type=="daySW")$hr_yr))
      bracket_total_hr_nightSW <- sum(as.numeric(subset(subset_df, work_type=="nightSW")$hr_yr))
      bracket_total_hr_mixSW <- sum(as.numeric(subset(subset_df, work_type=="mixSW")$hr_yr))
      bracket_total_hr_SW <- (bracket_total_hr_daySW + bracket_total_hr_nightSW +  bracket_total_hr_mixSW)
      
      #shiftwork per year of work in each bracket
      bracket_SW_per_work <- (bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW)/(bracket_total_hr)
      bracket_nightSW_per_work <- (bracket_total_hr_nightSW)/(bracket_total_hr)
      bracket_daySW_per_work <- (bracket_total_hr_daySW)/(bracket_total_hr)      
      bracket_mixSW_per_work <- (bracket_total_hr_mixSW)/(bracket_total_hr)      
      
      #bracket SW_YN = any SW in that bracket
      bracket_SW_YN <- ifelse(any(subset_df$SW_YN == "yes", na.rm = TRUE), 1, 0)
      
      # find the type of shiftwork done in the age bracket
      SW_job_codes <- subset(subset_df, work_type=="daySW" | work_type=="mixSW" | work_type=="nightSW")
      unique_values <- unique(SW_job_codes$work_type)
      
        if (length(unique_values) > 0) {  # Check if unique_values is not empty
          bracket_SW_type <- sort(paste(unique_values, collapse = ", "))
            } else {
              bracket_SW_type <- NA
            }
      
      # find the occupations that were shiftwork during this bracket.  There may have been two kinds of jobs, and a mix of SW/NonSW
      unique_values <- unique(SW_job_codes$job_occupation)
      bracket_SW_occupation <- paste(unique_values, collapse = ", ")
      
        if (length(unique_values) > 0) {# Check if unique_values is not empty
          bracket_SW_occupation <- sort(paste(unique_values, collapse = ", "))
          } else {
            bracket_SW_type <- NA
            }
      
      # get data into a vector to rbind to main dataframe
      eid <- as.numeric(gsub("\\D", "", file))
      
      #add the information for this age bracket to the big dataframe
      
      newdata <- data.frame(eid,agebracket, 
                            bracket_total_hr,
                            bracket_total_hr_daySW,
                            bracket_total_hr_nightSW,
                            bracket_total_hr_mixSW,
                            bracket_SW_type,
                            bracket_SW_occupation, 
                            bracket_SW_per_work, 
                            bracket_nightSW_per_work, 
                            bracket_daySW_per_work,       
                            bracket_mixSW_per_work,
                            bracket_SW_YN
                            )
      
      shiftwork <- rbind(shiftwork, newdata)
      
   }
}    
   
      
shiftwork <- rbind(shiftwork230224, shiftwork250224, shiftwork260225)
shiftwork230224 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork230224.csv")
shiftwork250224 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork250224.csv")
shiftwork260225 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork260225.csv")
write.csv(shiftwork, file="shiftwork250225.csv")


# the only thing of interest is the type of shiftwork at different age brackets.  The actual type of shiftwork at an individual level is probably not useful - would be under powered to detect any association with MS.  In the shiftwork dataframe, bracket_SW_type records the job type of each SW job for that age bracket for that person. Next loop though the shiftwork data frames to summarise the types of shiftwork done at each age bracket
 
# The variables could be:
#   (1) Ever a shiftworker
#   (2) Lifetime years of shiftwork
#   (3) Severity of shiftwork over lifetime = hours of nightshift
#   (4) Hours of nightshift 15-20
#   (5) Was a shiftworker 15-20
#   (6) Years of shiftwork per year of work 15-20


#######################################################################################################
#
# Barplots of changes in shlftwork over the lifetime
#
#######################################################################################################

#   Need to know the frequency of the shiftwork categories at each age bracket 
#   There may have been two kinds of jobs, and a mix of SW/NonSW.  
#   Use table to get frequencies of shiftwork occupations by age bracket


#  Barplot 1 Occupation of shiftworkers at each age bracket
###############################################################################################

SW_type_by_agebracket <- (prop.table(table(shiftwork$bracket_SW_type, shiftwork$agebracket), margin=2))

SW_type_by_agebracket <- replace(SW_type_by_agebracket, is.na(SW_type_by_agebracket), 0)

col <- viridis(length(labels), option = "D", begin = 0, end = 1, direction = 1 )

# tab:blue : #1f77b4
#   tab:orange : #ff7f0e
#   tab:green : #2ca02c
#   tab:red : #d62728
#   tab:purple : #9467bd
#   tab:brown : #8c564b
#   tab:pink : #e377c2
#   tab:gray : #7f7f7f
#   tab:olive : #bcbd22
#   tab:cyan : #17becf

col<-pal_jco("default")(9)

col <-c('#1f77b4', #tableau matlab
'#ff7f0e',
'#2ca02c',
'#d62728',
'#9467bd',
'#8c564b',
'#e377c2',
'#7f7f7f',
'#bcbd22')
'#17becf')

shiftwork$bracket_SW_type <- factor(shiftwork$bracket_SW_type, levels = c("Assoc_prof","Prof","Machine","Managers","Trades","Service","Element","Admin","Sales"))

labels <- c("Assoc Professional","Professional","Machine","Managers","Trades","Service","Element","Admin","Sales")

svg("occupation_barplot1.svg", width = 4, height = 3, pointsize = 8, onefile = TRUE)
par(mar = c(5, 4, 4, 12))
barplot((SW_type_by_agebracket), col = col,border = NA, space = .1, yaxt = "n")
title(main = "Occuption of Shiftworkers by Age Bracket", col.main = "black", font.main = 2, cex=.6)
title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
legend("right", x= 11, y = 1, bty = "n", xpd = TRUE,  border = NA, legend = labels, fill = col)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = 1, labels = c("0","20%","40%","60%", "80%", "100%"))  
dev.off()
par(mar = par("mar"))


##  Barplot 3  Shiftwork by type night day and mix across the ages
###############################################################################################

# Create a new categorical variable based on the three SW variables
shiftwork <- shiftwork %>%
  mutate(
   SW_summary = case_when(
      bracket_total_hr_daySW >1 ~ "Night",
      bracket_total_hr_nightSW >1 ~ "Day",
      bracket_total_hr_mixSW > 1 ~ "Mixed",
      TRUE ~ NA  
    ))

shiftwork$SW_summary <- factor(shiftwork$SW_summary, levels = c("Mixed", "Night", "Day"))
labels2 <- c("Mixed", "Night", "Day")

#col<-pal_jco("default")(9)
#"#0073C2FF" "#EFC000FF" "#868686FF" "#CD534CFF" "#7AA6DCFF" "#003C67FF" "#8F7700FF" "#3B3B3BFF" "#A73030FF"

col <- c("#0073C2FF","#3B3B3BFF","#EFC000FF")
all_SW_agebracket <- prop.table(table(factor(shiftwork$SW_summary),factor(shiftwork$agebracket)), margin=2)

   svg("SW_type_barplot.svg", width = 7, height = 3, pointsize = 8, onefile = TRUE)
   par(mar = c(5, 4, 4, 6))
   barplot((all_SW_agebracket), col = col,border = NA, space = .1, yaxt = "n")
   title(main = "Type of Shift Work by Age Bracket", col.main = "black", font.main = 2, cex=.6)
   title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
   legend("right", x= 11, y = 1, bty = "n", xpd = TRUE,  border = NA, legend = labels2, fill = col)
   axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = 1, labels = c("0","20%","40%","60%", "80%", "100%"))  
   dev.off()
   par(mar = par("mar"))


##  Barplot 3  Shiftwork or not across the ages
###############################################################################################

#use table to get the frequencies of all shiftwork by age
shiftwork$allSW <- shiftwork$bracket_total_hr_daySW + shiftwork$bracket_total_hr_night + shiftwork$bracket_total_hr_mixSW

# Creating the new categorical variable
shiftwork <- shiftwork %>%
  mutate(
    SW_YN = case_when(
      allSW == 0 & bracket_total_hr > 0 ~ "Not a Shiftworker",
      allSW > 0 & bracket_total_hr > 0 ~ "Shiftworker",
      bracket_total_hr == 0 ~ "Not working",
      TRUE ~ NA_character_
    )
  )

SW_agebracket <- prop.table(table(shiftwork$SW_YN,shiftwork$agebracket), margin=2)
labels3 <- c("Non Shift Worker" , "Not Working", "Shift Worker" )
labels4 <- c('15-20',      '21-25', '26-30',   '31-35', '36-40',  '41-45', '46-50','51-55', '56-60','61-65')
col <- c("#CD534CFF","#EFC000FF", "#003C67FF")

svg("SW_YN_barplot.svg", width = 4, height = 3, pointsize = 8, onefile = TRUE)
par(mar = c(5, 4, 4, 12))
barplot(SW_agebracket, col = col, space = .1, border = NA, yaxt = "n")
title(main = "Work Status by Age Bracket", col.main = "black", font.main = 2, cex=.8)
title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
legend("right", x= 11, y = .5, bty = "n", xpd = TRUE,  border = NA, legend = labels3, fill = col)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = .8, labels = c("0","20%","40%","60%", "80%", "100%"))  

par(mar = par("mar"))
dev.off()

###--------------------------  find missing  ---------------------------------------------------------------
      


#find missing eids       should be 121,248 participants that had data in born col
eid_true <- bd$f.eid[!is.na(bd$f.22200.0.0)] #121248 this is correct but 977 have missing data only true value is 120271 

check_filelist <- list.files(getwd()) #120271
eid_files <- as.integer(substr(check_filelist, 3, 9)) #121271
# 121248-120271 977 files missing from filelist

eid_true <- as.integer(eid_true)
eid_files <- as.integer(eid_files)

# Values in eid_true that are not in eid_files
values_not_in_eid_files <- as.integer(setdiff(eid_true, eid_files))

#get rows of bd where eid == values not in eid files
filtered_rows <- bd[bd$f.eid %in% values_not_in_eid_files, ]

