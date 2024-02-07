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

shiftwork <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork.csv")
UKB_master <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/UKB_master.csv")

# goto get_NDD_var_040124.R to get NDD data into UKB_master

#merge shiftowrk and core variables
UKB_masterSW <- merge(shiftwork, UKB_master, by="eid")

#make an early life shiftwork variable
#UKB_masterSW$early_life_SW <- ifelse(as.character(UKB_masterSW$agebracket) == "15-20" & as.character(UKB_masterSW$SW_YN) == "Shiftworker",1,0)

# check if smoked for that year in prior smokers

#age range
UKB_masterSW$start_age <- substr(UKB_masterSW$agebracket,1,2)
UKB_masterSW$stop_age <-  substr(UKB_masterSW$agebracket,4,5)


#===       get started smoking var    not enough participants had this var  ======================
#smokers range
UKB_masterSW$stop_smoke <- UKB_masterSW$smoke_stopped
UKB_masterSW$start_smoke <- UKB_masterSW$smoke_start

#function to check if smoked during age bracket in prior smokers
check_overlap <- function(start_age, stop_age, start_smoke, stop_smoke) {
  as.integer(start_age <= stop_smoke & stop_age >= start_smoke)
}

# Create a new variable 'smoked_bracket' based on overlapping intervals
UKB_masterSW$smoked_bracket <- check_overlap(UKB_masterSW$start_age, UKB_masterSW$stop_age, UKB_masterSW$start_smoke, UKB_masterSW$stop_smoke)

#add 0 if never smoked
UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smoking_status == "Never", 0, UKB_masterSW$smoked_year)

table(UKB_masterSW$smoker)
table(UKB_masterSW$smokerYN)

#add 1 to all years after started smoking in current smokers
UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smokerYN == "Non-Smoker" & UKB_masterSW$start_smoke > UKB_masterSW$start_age, 1, UKB_masterSW$smoked_year)

UKB_masterSW$smoke_20yr <- factor(UKB_masterSW$smoke_20yr)

#=================================================================================================================

#get missing MS var - change MS NA to MS zero
UKB_masterSW$MS <- ifelse(UKB_masterSW$MS_year > 1, 1,0)
table(UKB_masterSW$MS, useNA = "always")
UKB_masterSW$MS <- ifelse(is.na(UKB_masterSW$MS_year), 0, UKB_masterSW$MS)
table(UKB_masterSW$MS)

# get dataset of healthy only participants (other than MS)

#make health screening variable
table(UKB_masterSW$health_self_report, useNA="always")
# 
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
table(UKB_masterSW$healthy)

#take out the unhealthy, leave MS and healthy
UKB_masterSW_healthy <- UKB_masterSW[UKB_masterSW$healthy == "Healthy" | UKB_masterSW$MS == 1, ]

#change missing MS to didn't have MS, year = 0
UKB_masterSW_healthy$MS <- ifelse(is.na(UKB_masterSW_healthy$MS_year), 0, UKB_masterSW_healthy$MS)

#make a MS_YN variable
UKB_masterSW_healthy$MS_YN <- 0
UKB_masterSW_healthy$MS_YN[UKB_masterSW_healthy$MS == 1] <- 1

table(UKB_masterSW_healthy$MS, useNA="always")
table(UKB_masterSW_healthy$MS_YN, useNA="always")

UKB_masterSW_healthy$MS[is.na(UKB_masterSW_healthy$MS)] <- 0
UKB_masterSW_healthy$MS <- factor(UKB_masterSW_healthy$MS)
UKB_masterSW_healthy$MS <- droplevels(UKB_masterSW_healthy$MS)
table(UKB_masterSW_healthy$MS, useNA = "always")


# ==========================          make SW vars ==================================================
UKB_masterSW_healthy$SW_YN <- 0 # ever did SW
UKB_masterSW_healthy$SW_YN <- ifelse (UKB_masterSW_healthy$bracket_total_hr_nightSW > 0 | UKB_masterSW_healthy$bracket_total_hr_mixSW >0 | UKB_masterSW_healthy$bracket_total_hr_daySW > 0,1,UKB_masterSW_healthy$SW_YN)
UKB_masterSW_healthy$SW_YN <- factor(UKB_masterSW_healthy$SW_YN)

UKB_masterSW_healthy$NSW_YN <- 0  #ever did NSW
UKB_masterSW_healthy$NSW_YN <- ifelse (UKB_masterSW_healthy$bracket_total_hr_nightSW > 0 | UKB_masterSW_healthy$bracket_total_hr_mixSW >0,1,UKB_masterSW_healthy$NSW_YN)
UKB_masterSW_healthy$NSW_YN <- factor(UKB_masterSW_healthy$NSW_YN)

# ================           set all values for shiftwork after diagnosis to NA for sensitivity analysis only    ========
if (UKB_masterSW_healthy$dementia_year | UKB_masterSW_healthy$PD_year | UKB_masterSW_healthy$MS_year)

UKB_masterSW_healthy$bracket_total_hr_daySW
UKB_masterSW_healthy$bracket_total_hr_nightSW
UKB_masterSW_healthy$bracket_total_hr_mixSW
UKB_masterSW_healthy$bracket_SW_per_work
UKB_masterSW_healthy$bracket_nightSW_per_work
UKB_masterSW_healthy$bracket_daySW_per_work
UKB_masterSW_healthy$bracket_mixSW_per_work

#data for 15-20, used for general factors (ignore age bracket)
tabledata <- UKB_masterSW_healthy[UKB_masterSW_healthy$agebracket=="15-20",]
tabledata$MS[is.na(tabledata$MS)] <- 0
table(tabledata$MS, useNA = "always")
tabledata$MS[is.na(tabledata$MS)] <- 0

#process PD, MND and dementia data to get YN variables and NAs to zero
tabledata$PD_YN[is.na(tabledata$PD_YN)] <- 0
tabledata$dementia_YN[is.na(tabledata$dementia_YN)] <- 0
tabledata$mnd_YN[is.na(tabledata$dementia_YN)] <- 0

table(tabledata$mnd_YN, useNA = "always")
tabledata$mnd <- 0
tabledata$mnd[tabledata$mnd_YN==1] <- 1
table(tabledata$MS, useNA = "always")

"agebracket"              

"bracket_total_hr"        
"bracket_total_hr_daySW"   
"bracket_total_hr_nightSW" 
"bracket_total_hr_mixSW"   
"bracket_SW_type"         
"bracket_SW_per_work"      
"bracket_nightSW_per_work" 
"bracket_daySW_per_work"   
"bracket_mixSW_per_work"  

"sex"                      
"age"
"ethnicity_5"
"townsend"

"alcohol_intake"           
"time_outdoors_summer"
"chronotype"  
"BMI"                     
"smoker"

"child_obesity"
"smoke_start"
"smoke_20yr"
"birth_latitude"
"smoked_bracket"

#outcome
"mnd_YN"
"PD_YN"                   
"dementia_YN"

#get variables for tables
vars <- UKB_masterSW_healthy %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  str_c('"', ., '"') %>% 
  str_c(collapse = " + ") %>% 
  cat()

x <- names(UKB_masterSW_healthy)
vars <- paste(x, collapse = " + ")
vars <- noquote(vars)

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

############################################################################################################### 
# Table 1   Demography stratify by MS
# 
##############################################################################################################

chronotype <- factor(chronotype, levels = c(3:6), labels = c("Morning","More morning than evening","More evening than morning","Evening"))

# covariables
sex + age + ethnicity_5 + townsend + alcohol_intake + time_outdoors_summer + chronotype + BMI + smoker + child_obesity
+ smoke_start + smoke_20yr + birth_latitude + sleep_duration

#outcome
"mnd_YN"
"PD_YN" 
"MS_YN"
"dementia_YN"

# this is nicely formated table1, add pvalues from tableone function below
table1(~ sex + age + townsend + + birth_latitude + #demography
         alcohol_intake + time_outdoors_summer +  #lifestype
         chronotype + sleep_duration + BMI + child_obesity + #physiology
         smoker + smoke_start + smoke_20yr  + #smoking
         SW_YN + NSW_YN + bracket_SW_per_work + bracket_nightSW_per_work # shiftwork 15-20
         | dementia_YN, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

#check that some MS did shiftwork
result <- UKB_masterSW %>%
  group_by(eid) %>%
  summarise(SW = ifelse(any(SW_YN == "Shiftworker") & any(MS == 1), 1, 0))

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                      'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                      'chronotype' , 'sleep_duration' , 'BMI' , 'child_obesity' , #physiology
                                      'smoker' , 'smoke_start' , 'smoke_20yr'  , #smoking
                                      'SW_YN' , 'NSW_YN' , 'bracket_SW_per_work' , 'bracket_nightSW_per_work'),
                           strata = c("dementia_YN"), 
                           data = tabledata, 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)

##############################################################################################################
# 
# Table 2   Descriptive Statistics of Shiftwork stratified by brackets and MS
# 
##############################################################################################################

# Remove rows with missing values in the agebracket
UKB_masterSW_healthy <- UKB_masterSW_healthy[complete.cases(UKB_masterSW_healthy$agebracket), ]


UKB_masterSW_healthy <- UKB_masterSW_healthy[complete.cases(UKB_masterSW_healthy$agebracket),]
describe(UKB_masterSW_healthy$bracket_nightSW_per_work)

#percentage in work
#percentage in shiftwork
UKB_masterSW_healthy$bracket_total_hr_daySW

# this is only 15-20
table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work | agebracket + MS, data=UKB_masterSW_healthy, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

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

