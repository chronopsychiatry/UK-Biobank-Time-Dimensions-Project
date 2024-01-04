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

setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

#merge shiftowrk and core variables
UKB_masterSW <- merge(shiftwork, UKB_master, by="eid")

#make an early life shiftwork variable
UKB_masterSW$early_life_SW <- ifelse(as.character(UKB_masterSW$agebracket) == "15-20" & as.character(UKB_masterSW$SW_YN) == "Shiftworker",1,0)

# check if smoked for that year in prior smokers

#age range
UKB_masterSW$start_age <- substr(UKB_masterSW$agebracket,1,2)
UKB_masterSW$stop_age <-  substr(UKB_masterSW$agebracket,4,5)

#prior smokers range
UKB_masterSW$stop_smoke <- UKB_masterSW$age_stop_smoke
UKB_masterSW$start_smoke <- UKB_masterSW$start_smoke_P

#current smoker range
UKB_masterSW$start_smoke_C <- UKB_masterSW$start_smoke_current # all 1 after started

#function to check if smoked during age bracket in prior smokers
check_overlap <- function(start_age, stop_age, start_smoke, stop_smoke) {
  as.integer(start_age <= stop_smoke & stop_age >= start_smoke)
}

# Create a new variable 'smoked_year' based on overlapping intervals
UKB_masterSW$smoked_year <- check_overlap(UKB_masterSW$start_age, UKB_masterSW$stop_age, UKB_masterSW$start_smoke, UKB_masterSW$stop_smoke)

#add 0 if never smoked
UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smoking_status == "Never", 0, UKB_masterSW$smoked_year)

#add 1 to all years after started smoking in current smokers
UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smoking_status == "Previous" & UKB_masterSW$start_smoke > UKB_masterSW$start_age, 1, UKB_masterSW$smoked_year)

#data for 15-20, used for general factors (ignore age bracket)
tabledata <- UKB_masterSW[UKB_mast$agebracket=="15-20",]

SW_vars <- c( 
"agebracket" ,                
"bracket_total_hr" ,         
"bracket_total_hr_daySW" ,    
"bracket_total_hr_nightSW",   
"bracket_total_hr_mixSW" ,    
"bracket_SW_type"   ,        
"bracket_SW_per_work" ,       
"bracket_nightSW_per_work" ,  
"bracket_daySW_per_work"  ,   
"bracket_mixSW_per_work" ,   
"allSW"     ,                 
"SW_YN" ,                     
"SW_summary" )                                      

#demographic
"sex"                        
"age"                        
"year_born"                  
"month_born"                
"ethnicity_5"                  
"townsend" 
"age_completed_education"
"child_obesity"
"latitude_birth"
"smoking_status"
"alcohol_intake"
"depress_psych"             
"time_outdoors_summer"
"sleep_duration"            
"chronotype"
"BMI"                        

"MS_source"                  
"MS_year"                    
"MS_age" 
"MS_YN"

"PD_source"
"PD_age"
"PD_year"
"PD_YN"

"dementia_source"
"dementia_age"
"dementia_year"
"dementia_YN"
)


#get variables for tables
vars <- mydata %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  str_c('"', ., '"') %>% 
  str_c(collapse = " + ") %>% 
  cat()

x <- names(mydata)
vars <- paste(x, collapse = " + ")
vars <- noquote(vars)

#make tables
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}

my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}

my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

table <- table1(~  age + sex | eventname ,  data=data, overall=TRUE, render.continuous=my.render.cont, render.categorical=my.render.cat)

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

############################################################################################################### 
# Table 1   Demography stratify by MS
# 
##############################################################################################################


# this is nicely formated table1, add pvalues from tableone function below
table1(~  sex + age + ethnicity_5+ townsend + age_completed_education + smoking_status + alcohol_intake + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI|MS, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

#check that some MS did shiftwork
result <- UKB_masterSW %>%
  group_by(eid) %>%
  summarise(SW = ifelse(any(SW_YN == "Shiftworker") & any(MS == 1), 1, 0))

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c("age","sex", "ethnicity_5", "townsend" + "age_completed_education" , "smoking_status" , "alcohol_intake" , "depress_psych" , "allSW","SW_YN",  "SW_summary", "time_outdoors_summer" ,  "sleep_duration" , "chronotype" , "BMI"), strata = c("MS"), data = tabledata, factorVars = c())
IPAQ early_life_SW early_life_smoke
print(tableOne)

##############################################################################################################
# 
# Table 2   Descriptive Statistics of Shiftwork stratified by 15-20 and then mean of all other brackets and MS
# 
##############################################################################################################
IPAQ early_life_SW early_life_smoke
# this is only 15-20
table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work + allSW + SW_summary + night_shift + shift_work|agebracket + MS, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work + allSW + SW_summary + night_shift + shift_work|MS, data=mydata[mydata$agebracket != "15-20",], overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c("bracket_total_hr" , "bracket_total_hr_daySW" , "bracket_total_hr_nightSW" , "bracket_total_hr_mixSW" , "bracket_SW_type" , "bracket_SW_per_work" , "bracket_nightSW_per_work" , "bracket_daySW_per_work" , "bracket_mixSW_per_work" , "allSW" , "SW_summary" , "night_shift" , "shift_work"), strata = c("MS"), data = tabledata, factorVars = c())

print(tableOne)

tableOne <- CreateTableOne(vars = c("bracket_total_hr" , "bracket_total_hr_daySW" , "bracket_total_hr_nightSW" , "bracket_total_hr_mixSW" , "bracket_SW_type" , "bracket_SW_per_work" , "bracket_nightSW_per_work" , "bracket_daySW_per_work" , "bracket_mixSW_per_work" , "allSW" , "SW_summary" , "night_shift" , "shift_work"), strata = c("MS"), data = mydata[mydata$agebracket != "15-20",], factorVars = c())
print(tableOne)


##############################################################################################################
#
# Model - SW in early life no age bracket data
#
################################################################################################################
# IPAQ early_life_SW early_life_smoke
#M1 - demography
#
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude + early_life_SW ,
                data = tabledata, family = "binomial")
summary(model)  

#M2 - physical mental health and lifestyle
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude +  time_outdoors_summer + shift_work +
              child_obesity + chronotype + BMI + bracket_total_hr_nightSW + bracket_SW_per_work        ,
             data = tabledata, family = "binomial")
summary(model)  
table(tabledata$shift_work, useNA = "always")

#M3 - work and environment
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude + 
               child_obesity + sleep_duration + chronotype + BMI + smoking_status + alcohol_intake  + depress_psych + 
              +  + early_life_SW,
               
             data = tabledata, family = "binomial")
summary(model)  



# Fit logistic regression model for early life shiftwork
model <- glm(MS ~  + smoking_status + alcohol_intake + townsend + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI + bracket_total_hr_nightSW + age + sex + bracket_SW_per_work, data = tabledata, family = "binomial")

# Display the summary of the model
summary(model)

# Fit logistic regression model for total life shiftwork
model <- glm(MS ~ ethnicity_5 + early_life_SW + smoking_status + alcohol_intake + townsend + depress_psych + agebracket + time_outdoors_summer +  sleep_duration + chronotype + BMI + bracket_total_hr_nightSW + age + sex + bracket_SW_per_work, data = mydata, family = "binomial")

# Display the summary of the model
summary(model)

##############################################################################################################
#
# Model - SW in early life - cross sectional
#
##############################################################################################################
+ alcohol_intake birth_latitude  +  chronotypeethnicity_5 + sex + age + townsend + time_outdoors_summer + child_obesity + bracket_total_hr_nightSW + 
# Logistic regression with age bracket as a random effect
model <- glmer(MS ~ ethnicity_5 + sex + age +  bracket_total_hr_nightSW + bracket_SW_per_work + early_life_smoke + (1 | agebracket), data = mydata, family = binomial)


model <- glm(MS ~ bracket_SW_per_work + townsend + time_outdoors_summer + child_obesity + smoked_year +  bracket_total_hr_nightSW  + bracket_SW_per_work, data = mydata, family = binomial)



# Summary of the mixed-effects logistic regression model
summary(model)
mydata$early_life_smoke

##############################################################################################################
#
# Model 3 - SW in early life - prospective
#
##############################################################################################################

