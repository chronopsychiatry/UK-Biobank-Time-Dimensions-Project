# Data Analysis of UKB Lifetime SW and NDD
library(lme4)
library(table1)
library(tableone)
library(finalfit)
library(dplyr)
library(knitr)
library(sjPlot)
library(stringr)
library(glmnet)
library(psych)
library(performance)
library(see)
library(sjmisc)
library(sjlabelled)

setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

shiftwork <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork.csv")
UKB_master <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/UKB_master150224.csv")

# add sunburn data
sunburn <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/sunburn.tsv")
names(sunburn) <- c("eid", "sunburn")
sunburn[sunburn$sunburn == -3] <- NA #-3	Prefer not to answer
UKB_master <- merge(UKB_master,sunburn,by="eid")
# unit are occasions
# -1	Do not know
# -3	Prefer not to answer

#adding centre to UKB_master
centre <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/centre.tsv")
names(centre) <- c("eid", "centre")
UKB_master <- merge(UKB_master,centre,by="eid")

#adding maternal smoking to UKB_master Data-Field 1787
maternal_smoke <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/maternal_smoke.tsv")
names(maternal_smoke) <- c("eid", "maternal_smoke")
maternal_smoke$maternal_smoke[maternal_smoke$maternal_smoke == ""] <- NA
maternal_smoke$maternal_smoke[maternal_smoke$maternal_smoke == "Prefer not to answer"] <- NA# -3	Prefer not to answer
maternal_smoke$maternal_smoke <- factor(maternal_smoke$maternal_smoke)
UKB_master <- merge(UKB_master,maternal_smoke,by="eid")


# goto get_NDD_var_040124.R to get NDD data into UKB_master

##########################################################################################################################
#
#  1.  Format shiftwork table to get cumulative hours or dose SW
#
##########################################################################################################################

shiftwork_cum <- shiftwork

# ever did early night shiftwork 15-20
shiftwork_cum$early_NSW_YN <- 0
shiftwork_cum$early_NSW_YN <- ifelse (shiftwork_cum$agebracket=="15-20" &
                                        shiftwork_cum$bracket_total_hr_nightSW > 0 , 1, 
                                      shiftwork_cum$early_NSW_YN)

shiftwork_cum$early_NSW_YN <- ifelse (shiftwork_cum$agebracket=="15-20" &
                                        shiftwork_cum$bracket_total_hr_mixSW > 0 , 1, 
                                      shiftwork_cum$early_NSW_YN)
shiftwork_cum$early_NSW_YN <- factor(shiftwork_cum$early_NSW_YN)

# ever did early any shiftwork 15-20
shiftwork_cum$early_SW_YN <- 0
shiftwork_cum$early_SW_YN <- ifelse(shiftwork_cum$agebracket == "15-20" & 
                     (shiftwork_cum$bracket_total_hr_nightSW > 0 | shiftwork_cum$bracket_total_hr_mixSW > 0 |   
                        shiftwork_cum$bracket_total_hr_daySW > 0 ), 1, shiftwork_cum$early_SW_YN)
shiftwork_cum$early_SW_YN <- factor(shiftwork_cum$early_SW_YN)

shiftwork_cum <- shiftwork_cum [,c(
                                  "eid",              
                                  "agebracket",               
                                  "bracket_total_hr",        
                                  "bracket_total_hr_daySW",   
                                  "bracket_total_hr_nightSW", 
                                  "bracket_total_hr_mixSW",   
                                  "bracket_SW_type",          
                                  "bracket_SW_occupation",   
                                  "bracket_SW_per_work",      
                                  "bracket_nightSW_per_work", 
                                  "bracket_daySW_per_work",   
                                  "bracket_mixSW_per_work", 
                                  "early_NSW_YN",
                                  "early_SW_YN",
                                  "bracket_SW_YN")]  

# Summarize continuous variables by eid
shiftwork_cum <- shiftwork_cum %>%
  group_by(eid) %>%
  summarise(total_hr = sum(bracket_total_hr),
            total_hr_daySW = sum(bracket_total_hr_daySW),
            total_hr_nightSW = sum(bracket_total_hr_nightSW),
            total_hr_mixSW = sum(bracket_total_hr_mixSW),
            early_NSW_YN = max(as.numeric(early_NSW_YN)),
            early_SW_YN = max(as.numeric(early_SW_YN)),
            SW_type = paste(bracket_SW_type, collapse = " "),          
            SW_occupation = paste(bracket_SW_occupation, collapse = " ")
    )

# Remove rows with NA or zero values in 'total_hours' column
shiftwork_cum <- shiftwork_cum %>%
  filter(!is.na(total_hr) & total_hr != 0)

UKB_masterSW_cum <- merge(shiftwork_cum, UKB_master, by="eid")
UKB_masterSW_cum$SW <- UKB_masterSW_cum$total_hr_nightSW + UKB_masterSW_cum$total_hr_mixSW + UKB_masterSW_cum$total_hr_daySW
UKB_masterSW_cum$NSW <- UKB_masterSW_cum$total_hr_nightSW + UKB_masterSW_cum$total_hr_mixSW

UKB_masterSW_cum$NDD <- UKB_masterSW_cum$NDD <- 0
UKB_masterSW_cum$NDD <- ifelse(UKB_masterSW_cum$MS_YN == 1, "MS", 
                               ifelse(UKB_masterSW_cum$PD_YN == 1, "PD",
                                      ifelse(UKB_masterSW_cum$dementia_YN == 1, "Dementia"
                                             ,0)))


##########################################################################################################################
#
#  2.  Merge shiftwork and core variables 
#
##########################################################################################################################

#merge shiftowrk and core variables
UKB_masterSW <- merge(shiftwork, UKB_master, by="eid")

# get dataset of healthy only participants (other than MS)
# Coding	Meaning
# 1	Excellent
# 2	Good
# 3	Fair
# 4	Poor
# -1	Do not know
# -3	Prefer not to answer
UKB_masterSW <- UKB_masterSW %>%
  mutate(healthy = case_when(
    health_self_report == "Prefer not to answer" ~ NA,
    health_self_report == "Excellent" ~ "Healthy",
    health_self_report == "Good" ~ "Healthy",
    health_self_report == "Fair" ~ "Not Healthy",
    health_self_report == "Poor" ~ "Not Healthy",
    health_self_report == "Do not know" ~ "Not Healthy",
    health_self_report == "Prefer not to answer" ~ NA,
    TRUE ~ NA
  ))
UKB_masterSW$healthy <- factor(UKB_masterSW$healthy)


# define the participants that worked but not SW
UKB_masterSW$bracket_SW_type <- ifelse(is.na(UKB_masterSW$bracket_SW_type) & UKB_masterSW$bracket_total_hr > 0, "notSW",UKB_masterSW$bracket_SW_type)

# bracket SW_YN = participant did any type of SW in that bracket
UKB_masterSW$bracket_SW_YN <- ifelse(is.na(UKB_masterSW$bracket_SW_type), NA, 
                                ifelse(UKB_masterSW$bracket_SW_type %in% c("daySW", "mix_SW", "nightSW"), 1, 0))
  

# bracket NSW_YN = participant did any NSW in that bracket
UKB_masterSW$bracket_NSW_YN <- ifelse(is.na(UKB_masterSW$bracket_SW_type), NA, 
                                     ifelse(UKB_masterSW$bracket_SW_type %in% c("mixSW", "nightSW"), 1, 0))

# ever did early shiftwork 15-20
UKB_masterSW$early_SW_YN <- 0  #any SW
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting NSW
                                             UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)

UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & #getting daySW
                                             UKB_masterSW$bracket_total_hr_daySW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)
                                    
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & #getting mixSW
                                             UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                               UKB_masterSW$early_SW_YN)

UKB_masterSW$early_NSW_YN <- 0  #any NSW
UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting NSW
                                            UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                               UKB_masterSW$early_NSW_YN)

UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" & # getting mixSW
                                            UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                               UKB_masterSW$early_NSW_YN)

UKB_masterSW$early_SW_YN <- factor(UKB_masterSW$early_SW_YN)
UKB_masterSW$early_NSW_YN <- factor(UKB_masterSW$early_NSW_YN)

# make percentage in work variable - note that NA or 0 hours are gaps, not possible to have NA, either work or gap
UKB_masterSW$workingYN <- ifelse(is.na(UKB_masterSW$bracket_total_hr), 0,1)
 
# make variable to stratify by NDD
UKB_masterSW$NDD <- 0
UKB_masterSW$NDD <- ifelse(UKB_masterSW$MS_YN == 1, "MS", 
                            ifelse(UKB_masterSW$PD_YN == 1, "PD",
                                   ifelse(UKB_masterSW$dementia_YN == 1, "Dementia"
                                          ,0)))
 
#########################################################################################################################
# 
# 3,  Format shiftwork table to get years of shiftwork variables
#   
#########################################################################################################################

shiftwork_yr140324 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork_yr140324.csv") # this has only participants in follow up study and only SW categories

shiftwork_years <- shiftwork_yr140324

# Replace NA with 0
shiftwork_years[is.na(shiftwork_years)] <- 0

# get years of SW for each type
shiftwork_years$yrs_SW  <- rowSums(shiftwork_years[,c("mixSW","nightSW","daySW")]) # any SW
shiftwork_years$yrs_NSW <- rowSums(shiftwork_years[,c("mixSW","nightSW")])   # NSW

# Define the breakpoints for intervals
breaks <- c(-Inf, 0, 4.99, 9.99, Inf)

# Define labels for the intervals
labels <- c("0", "1-5", "6-10", ">10")

# Recode the continuous variable into categories
shiftwork_years$SW_cat <- cut(shiftwork_years$yrs_SW, breaks = breaks, labels = labels, include.lowest = TRUE)
shiftwork_years$NSW_cat <- cut(shiftwork_years$yrs_NSW, breaks = breaks, labels = labels, include.lowest = TRUE)

shiftwork_years <- merge(UKB_masterSW,shiftwork_years, by = "eid")  # merge the new SW categories into main table



##############################################################################################################
#
# 4.  SW in early life no age bracket data
#
################################################################################################################

#make a dataset to test early life shiftwork regardless of age bracket
x<-shiftwork_years %>%
  select(eid, sex, age, ethnicity_5, smoking_status, centre, alcohol_intake, time_outdoors_summer, sleep_duration,                chronotype,  BMI_cat, townsend, maternal_smoke, sunburn,  child_obesity, smoker, smoke_start,  smoke_20yr, birth_latitude,mnd_YN, PD_YN,dementia_YN, MS_YN,NDD, early_SW_YN, early_NSW_YN, bracket_SW_YN, bracket_NSW_YN, NDD) 

# Define a function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by 'eid' and summarize
summary_data <- x %>%
  group_by(eid) %>%
  summarise(
    sex = get_mode(sex),
    age = max(age),
    ethnicity_5 = get_mode(ethnicity_5),
    smoking_status = get_mode(smoking_status),
    alcohol_intake = get_mode(alcohol_intake),
    time_outdoors_summer = max(time_outdoors_summer, na.rm=TRUE),
    sleep_duration = max(sleep_duration, na.rm=TRUE),
    chronotype = get_mode(chronotype),
    BMI_cat = get_mode(BMI_cat),
    townsend = max(townsend, na.rm=TRUE),
    child_obesity = get_mode(child_obesity),
    maternal_smoke = get_mode(maternal_smoke),
    sunburn = max(sunburn, na.rm=TRUE),
    smoker = get_mode(smoker),
    smoke_start = max(smoke_start, na.rm=TRUE),
    smoke_20yr = max(smoke_20yr, na.rm=TRUE),
    birth_latitude = max(birth_latitude, na.rm=TRUE),
    mnd_YN = get_mode(mnd_YN),
    PD_YN = get_mode(PD_YN),
    dementia_YN = get_mode(dementia_YN),
    MS_YN = get_mode(MS_YN),
    early_SW_YN = max(early_SW_YN,na.rm = TRUE),
    early_NSW_YN = max(early_NSW_YN,na.rm = TRUE),
    #bracket_SW_YN = max(bracket_SW_YN, na.rm=TRUE),
    #bracket_NSW_YN = max(as.numeric(bracket_NSW_YN),na.rm = TRUE),
    NDD = get_mode(NDD, na.rm=TRUE)
  )

summary_data$early_SW_YN = factor(summary_data$early_SW_YN)
summary_data$early_NSW_YN = factor(summary_data$early_NSW_YN)
summary_data$bracket_SW_YN = factor(summary_data$bracket_SW_YN)
summary_data$bracket_NSW_YN = factor(summary_data$bracket_NSW_YN)

max(x$bracket_SW_YN, na.rm=TRUE)
any(x$bracket_NSW_YN == -Inf)
any(x$bracket_SW_YN == -Inf)
 max(x$bracket_SW_YN, na.rm=TRUE)
max(x$bracket_SW_YN==-Inf, na.rm=TRUE)

# Remove rows where bracket_SW_YN is equal to -Inf
summary_data[summary_data$bracket_SW_YN != -Inf, ]
summary_data$bracket_SW_YN <- droplevels(summary_data$bracket_SW_YN)
table(summary_data$bracket_NSW_YN, useNA = "always")
summary_data <- summary_data[summary_data$bracket_NSW_YN != -Inf, ]
summary_data$bracket_NSW_YN <- droplevels(summary_data$bracket_NSW_YN)



 #########################################################################################################################
 # 
 # 5.  Make tables of descriptive data
 #   
 #########################################################################################################################
 
#  
# #get variables for tables
# vars <- UKB_masterSW %>%
#   select(where(is.numeric)) %>%
#   colnames() %>%
#   str_c('"', ., '"') %>% 
#   str_c(collapse = " + ") %>% 
#   cat()
# 
# vars <- paste(x, collapse = " + ")
# vars <- noquote(vars)

#make tables
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}

my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}

my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

#template table <- table1(~  age + sex | eventname ,  data=data, overall=TRUE, render.continuous=my.render.cont, render.categorical=my.render.cat)

# format continuous vars with 95% CI
my.render.cont = function(x) {c("", "mean (95% CI)"=sprintf("%s (%s, %s)",
              round(stats.default(x)$MEAN,2),
              round(stats.default(x)$MEAN-qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2),
              round(stats.default(x)$MEAN+qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2)
              ))
            }

#check 95% CI
t.test(tabledata[tabledata$MS==1,]$age)$conf.int
sample_mean <- mean(tabledata[tabledata$MS==1,]$age)
sample_sd <- sd(tabledata[tabledata$MS==1,]$age)
sample_size <- length(tabledata[tabledata$MS==1,]$age) 
df <- sample_size - 1
se <- sample_sd / sqrt(sample_size)
margin_of_error <- qt(0.975, df) * se
lower_ci <- sample_mean - margin_of_error
upper_ci <- sample_mean + margin_of_error


# Table 1   Demography stratify by NDD
---------------------------------------------------------------------------------------------------------------------

table1(~ sex + age + townsend + birth_latitude + #demography
         BMI_cat +   chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestyle
         smoker + smoke_start + smoke_20yr  + #smoking
         early_NSW_YN + early_SW_YN + factor(bracket_NSW_YN) + bracket_SW_YN # shiftwork 15-20
         | NDD, data=summary_data, overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                      'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                      'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                      'smoker' ,
                                    'smoke_start' , 'smoke_20yr'  ,
                                      'early_NSW_YN' , 'early_SW_YN' , 'bracket_NSW_YN' , 'bracket_SW_YN' ),
                           strata = c("PD_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())



# Table 2   Demography stratify by SW
---------------------------------------------------------------------------------------------------------------------
  
table1(~ sex + age + townsend + birth_latitude + #demography
           BMI_cat +   chronotype + child_obesity + #physiology
           alcohol_intake + time_outdoors_summer +  #lifestyle
           smoker + smoke_start + smoke_20yr  + #smoking
           early_NSW_YN + NDD + early_SW_YN   # shiftwork 15-20
         | bracket_SW_YN, data=summary_data, overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' ,
                                    'smoke_start' , 'smoke_20yr'  ,
                                    'early_NSW_YN' , 'early_SW_YN' ),
                           strata = c("bracket_SW_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)

table1(~ sex + age + townsend + birth_latitude + #demography
         BMI_cat +   chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestyle
         smoker + smoke_start + smoke_20yr  + #smoking
         early_NSW_YN + NDD + early_SW_YN   # shiftwork 15-20
       | bracket_NSW_YN, data=summary_data, overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' ,
                                    'smoke_start' , 'smoke_20yr'  ,
                                    'early_NSW_YN' , 'early_SW_YN' ),
                           strata = c("bracket_NSW_YN"), 
                           data = summary_data, 
                           #test = FALSE, 
                           factorVars = c())
print(tableOne)


# Table 3   Shiftwork exposure data 
---------------------------------------------------------------------------------------------------------------------

#table to summarise years of exposure and dose of shiftwork

#need shiftwork_cum to get dose and need shiftwork_years to get years, merge these two
for_merge <- shiftwork_cum[,c("eid","total_hr", "total_hr_daySW", "total_hr_nightSW", "total_hr_mixSW")]
shiftwork_merged <- merge(shiftwork_years, for_merge, by="eid")
shiftwork_merged <- merge(shiftwork_merged, summary_data[,c("NDD", "eid")], by="eid")

table1(~ total_hr + total_hr_daySW + total_hr_nightSW + total_hr_mixSW + SW_cat + NSW_cat + yrs_SW + yrs_NSW
       | NDD, data=shiftwork_merged, overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)


  
################################################################################################################
#
# 6.  Regression models
# 
##############################################################################################################


# MODELS FOR PROB OF BEING A SW OR EARLY SW AND MS
---------------------------------------------------------------------------------------------------------------------
  
#remove post 2015 diagnosis Filter out rows where eid is present in diag2015
summary_data_filter <- subset(summary_data, !(eid %in% diag_2015))


# Specify the reference level using the relevel function
summary_data$chronotype <- factor(summary_data$chronotype)
summary_data$chronotype <- relevel(summary_data$chronotype, ref = "Morning")
table(summary_data$chronotype)
str(summary_data)

summary_data$smoker <- factor(summary_data$smoker)
summary_data$smoker <- relevel(summary_data$smoker, ref = "Never smoked")
table(summary_data$smoker)

summary_data$child_obesity <- factor(summary_data$child_obesity)
summary_data$child_obesity <- relevel(summary_data$child_obesity, ref = "Thinner")
table(summary_data$child_obesity)

summary_data$alcohol_intake <- factor(summary_data$alcohol_intake)
summary_data$alcohol_intake <- relevel(summary_data$alcohol_intake, ref = "Never")
table(summary_data$alcohol_intake)


# PD final model looking at numbers that did early SW and any SW
PD_model1 <- glm(PD_YN ~ age + sex + townsend + centre,
                 data = summary_data, family = "binomial")
PD_model2 <- glm(PD_YN ~ age + sex + townsend + centre 
              + child_obesity + early_SW_YN + chronotype,
              data = summary_data, family = "binomial")
PD_model3 <- glm(PD_YN ~ age + sex + townsend + centre 
              + child_obesity + early_SW_YN +chronotype 
               + alcohol_intake + smoker +  time_outdoors_summer   + bracket_SW_YN,
              data = summary_data_filter, family = "binomial")
summary(PD_model3)

tab_model(PD_model1, PD_model2, PD_model3 ,show.intercept=FALSE)

# MS           final model looking at numbers that did early SW and any SW
MS_model1 <- glm(MS_YN ~ age + sex + townsend + centre,
                 data = summary_data, family = "binomial")
MS_model2 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype,
                 data = summary_data, family = "binomial")
MS_model3 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype
                 + alcohol_intake + smoker + time_outdoors_summer  + bracket_SW_YN,
                 data = summary_data, family = "binomial")
summary(MS_model3)
tab_model(MS_model1, MS_model2, MS_model3 ,show.intercept=FALSE)


# dementia     final model looking at numbers that did early SW and any SW
dementia_model1 <- glm(dementia_YN ~ age + sex + townsend + centre,
                 data = summary_data, family = "binomial")
dementia_model2 <- glm(dementia_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype,
                 data = summary_data, family = "binomial")
dementia_model3 <- glm(dementia_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype
                 + alcohol_intake + smoker + time_outdoors_summer +   bracket_SW_YN,
                 data = summary_data, family = "binomial")
tab_model(dementia_model1, dementia_model2, dementia_model3 ,show.intercept=FALSE)
summary(dementia_model3)

#check VIF for early life and ever sw
library (car)
vif_values <- car::vif(PD_model3)
print(vif_values)



#  MODELS FOR EXPOSURE TO SW 
---------------------------------------------------------------------------------------------------------------------
  

# Specify the reference level using the relevel function
UKB_masterSW_cum$chronotype <- factor(UKB_masterSW_cum$chronotype)
UKB_masterSW_cum$chronotype <- relevel(UKB_masterSW_cum$chronotype, ref = "Morning")
table(UKB_masterSW_cum$chronotype)

UKB_masterSW_cum$smoker <- factor(UKB_masterSW_cum$smoker)
UKB_masterSW_cum$smoker <- relevel(UKB_masterSW_cum$smoker, ref = "Never smoked")
table(UKB_masterSW_cum$smoker)

UKB_masterSW_cum$child_obesity <- factor(UKB_masterSW_cum$child_obesity)
UKB_masterSW_cum$child_obesity <- relevel(UKB_masterSW_cum$child_obesity, ref = "Thinner")
table(UKB_masterSW_cum$child_obesity)

UKB_masterSW_cum$alcohol_intake <- factor(UKB_masterSW_cum$alcohol_intake)
UKB_masterSW_cum$alcohol_intake <- relevel(UKB_masterSW_cum$alcohol_intake, ref = "Never")
table(UKB_masterSW_cum$alcohol_intake)

UKB_masterSW_cum$NSW_normal <- UKB_masterSW_cum$NSW/UKB_masterSW_cum$total_hr

# PD           
PDx_model1 <- glm(PD_YN ~ age + sex + townsend + centre,
                 data = UKB_masterSW_cum, family = "binomial")
PDx_model2 <- glm(PD_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype,
                 data = UKB_masterSW_cum, family = "binomial")
PDx_model3 <- glm(PD_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype
                 + alcohol_intake + smoker + time_outdoors_summer + SW,
                 data = UKB_masterSW_cum, family = "binomial")
tab_model(PDx_model1, PDx_model2, PDx_model3 ,show.intercept=FALSE)
summary(PDx_model3)

# MS           
MSx_model1 <- glm(MS_YN ~ age + sex + townsend + centre,
                 data = UKB_masterSW_cum, family = "binomial")
MSx_model2 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype,
                 data = UKB_masterSW_cum, family = "binomial")
MSx_model3 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype
                 + alcohol_intake + smoker + time_outdoors_summer + SW,
                 data = UKB_masterSW_cum, family = "binomial")
tab_model(MSx_model1, MSx_model2, MSx_model3 ,show.intercept=FALSE)
summary(MSx_model3 )

# dementia     
dementiax_model1 <- glm(dementia_YN ~ age + sex + townsend + centre,
                       data = UKB_masterSW_cum, family = "binomial")
dementiax_model2 <- glm(dementia_YN ~ age + sex + townsend + centre 
                       + child_obesity + early_SW_YN + chronotype,
                       data = UKB_masterSW_cum, family = "binomial")
dementiax_model3 <- glm(dementia_YN ~ age + sex + townsend + centre 
                       + child_obesity + early_SW_YN + chronotype
                       + alcohol_intake + smoker + time_outdoors_summer + SW,
                       data = UKB_masterSW_cum, family = "binomial")
tab_model(dementiax_model1, dementiax_model2, dementiax_model3 ,show.intercept=FALSE)
summary(dementiax_model3)




#  MODELS FOR years of SW multiplied by total hours of shiftwork in career
---------------------------------------------------------------------------------------------------------------------
  
#remove post 2015 diagnosis Filter out rows where eid is present in diag2015
shiftwork_cum_pre2015 <- subset(shiftwork_years, !(eid %in% diag_2015))

  
  
# Specify the reference level using the relevel function
shiftwork_years$chronotype <- factor(shiftwork_years$chronotype)
shiftwork_years$chronotype <- relevel(shiftwork_years$chronotype, ref = "Morning")
table(shiftwork_years$chronotype)

shiftwork_years$smoker <- factor(shiftwork_years$smoker)
shiftwork_years$smoker <- relevel(shiftwork_years$smoker, ref = "Never smoked")
table(shiftwork_years$smoker)

shiftwork_years$child_obesity <- factor(shiftwork_years$child_obesity)
shiftwork_years$child_obesity <- relevel(shiftwork_years$child_obesity, ref = "Thinner")
table(shiftwork_years$child_obesity)

shiftwork_years$alcohol_intake <- factor(shiftwork_years$alcohol_intake)
shiftwork_years$alcohol_intake <- relevel(shiftwork_years$alcohol_intake, ref = "Never")
table(shiftwork_years$alcohol_intake)

# make categorical variables
x <- UKB_masterSW_cum[,c("eid","early_NSW_YN", "early_SW_YN" )]
shiftwork_years <- merge(shiftwork_years,x, by="eid")
      
# PD           
PDyr_model1 <- glm(PD_YN ~ age + sex + townsend + centre,
                  data = shiftwork_years, family = "binomial")
PDyr_model2 <- glm(PD_YN ~ age + sex + townsend + centre 
                  + child_obesity  + early_NSW_YN + chronotype,
                  data = shiftwork_years, family = "binomial")
PDyr_model3 <- glm(PD_YN ~ age + sex + townsend + centre 
                  + child_obesity  + early_SW_YN + chronotype
                  + alcohol_intake + smoker + time_outdoors_summer + SW_dose_yr,
                  data = shiftwork_cum_pre2015 , family = "binomial")
tab_model(PDyr_model1, PDyr_model2, PDyr_model3 ,show.intercept=FALSE)
summary(PDyr_model3)

# MS           
MSyr_model1 <- glm(MS_YN ~ age + sex + townsend + centre,
                  data = shiftwork_years, family = "binomial")
MSyr_model2 <- glm(MS_YN ~ age + sex + townsend + centre 
                  + child_obesity + early_NSW_YN + chronotype,
                  data = shiftwork_years, family = "binomial")
MSyr_model3 <- glm(MS_YN ~ age + sex + townsend + centre 
                  + child_obesity  +early_SW_YN+ chronotype
                  + alcohol_intake + smoker + time_outdoors_summer + SW_dose_yr,
                  data = shiftwork_cum_pre2015, family = "binomial")
tab_model(MSyr_model1, MSyr_model2, MSyr_model3 ,show.intercept=FALSE)
summary(MSyr_model3 )

# dementia     
dementiayr_model1 <- glm(dementia_YN ~ age + sex + townsend + centre,
                        data = shiftwork_years, family = "binomial")
dementiayr_model2 <- glm(dementia_YN ~ age + sex + townsend + centre 
                        + child_obesity + early_NSW_YN + chronotype,
                        data = shiftwork_years, family = "binomial")
dementiayr_model3 <- glm(dementia_YN ~ age + sex + townsend + centre 
                        + child_obesity +  early_SW_YN + chronotype
                        + alcohol_intake + smoker + time_outdoors_summer + SW_dose_yr,
                        data = shiftwork_cum_pre2015, family = "binomial")
tab_model(dementiayr_model1, dementiayr_model2, dementiayr_model3 ,show.intercept=FALSE)
summary(dementiayr_model3)

shiftwork_years <- merge(shiftwork_years, UKB_masterSW_cum[,c("SW","eid")], by="eid") 
shiftwork_years$SW_dose_yr <- shiftwork_years$yrs_SW * shiftwork_years$SW






#~~~~~~~~~~~~~~~~~~~~~~~~~    2015

PDyr_model3 <- glm(PD_YN ~ age + sex 
                 + alcohol_intake + smoker + time_outdoors_summer + yrs_SW + nightSW,
                   data = shiftwork_cum_pre2015 , family = "binomial")
summary(PDyr_model3)

MSyr_model3 <- glm(MS_YN ~ age + sex 
                   + child_obesity + alcohol_intake + smoker + time_outdoors_summer + yrs_SW + nightSW,
                   data = shiftwork_cum_pre2015, family = "binomial")
summary(MSyr_model3 )


dementiayr_model3 <- glm(dementia_YN ~ age + sex 
                         + child_obesity + alcohol_intake + smoker + time_outdoors_summer + yrs_SW + nightSW,
                         data = shiftwork_cum_pre2015, family = "binomial")
summary(dementiayr_model3)


# PD final model looking at numbers that did early SW and any SW
PD_model3 <- glm(PD_YN ~ age + sex + child_obesity + early_SW_YN 
                 ,                 data = summary_data, family = "binomial")
summary(PD_model3)


# MS           final model looking at numbers that did early SW and any SW
MS_model1 <- glm(MS_YN ~ age + sex + townsend + centre,
                 data = summary_data, family = "binomial")
MS_model2 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype,
                 data = summary_data, family = "binomial")



MS_model3 <- glm(MS_YN ~ age + sex + townsend + centre 
                 + child_obesity + early_SW_YN + chronotype
                 + alcohol_intake + smoker + time_outdoors_summer  + bracket_SW_YN,
                 data = summary_data, family = "binomial")
summary(MS_model3)


# dementia     final model looking at numbers that did early SW and any SW
dementia_model1 <- glm(dementia_YN ~ age + sex + townsend + centre,
                       data = summary_data, family = "binomial")
dementia_model2 <- glm(dementia_YN ~ age + sex + townsend + centre 
                       + child_obesity + early_SW_YN + chronotype,
                       data = summary_data, family = "binomial")
dementia_model3 <- glm(dementia_YN ~ age + sex + townsend + centre 
                       + child_obesity + early_SW_YN + chronotype
                       + alcohol_intake + smoker + time_outdoors_summer +   bracket_SW_YN,
                       data = summary_data, family = "binomial")
tab_model(dementia_model1, dementia_model2, dementia_model3 ,show.intercept=FALSE)
summary(dementia_model3)
