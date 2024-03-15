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


setwd("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork")

shiftwork1 <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/shiftwork.csv")
UKB_master <- read.csv("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/UKB_master150224.csv")

# goto get_NDD_var_040124.R to get NDD data into UKB_master

#merge shiftowrk and core variables
UKB_masterSW <- merge(shiftwork1, UKB_master, by="eid")

# #===       get started smoking var    not enough participants had this var  ======================
# #smokers range
# UKB_masterSW$stop_smoke <- UKB_masterSW$smoke_stopped
# UKB_masterSW$start_smoke <- UKB_masterSW$smoke_start
# 
# #function to check if smoked during age bracket in prior smokers
# check_overlap <- function(start_age, stop_age, start_smoke, stop_smoke) {
#   as.integer(start_age <= stop_smoke & stop_age >= start_smoke)
# }
# 
# # Create a new variable 'smoked_bracket' based on overlapping intervals
# UKB_masterSW$smoked_bracket <- check_overlap(UKB_masterSW$start_age, UKB_masterSW$stop_age, UKB_masterSW$start_smoke, UKB_masterSW$stop_smoke)
# 
# #add 0 if never smoked
# UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smoking_status == "Never", 0, UKB_masterSW$smoked_year)
# 
# table(UKB_masterSW$smoker)
# table(UKB_masterSW$smokerYN)
# 
# #add 1 to all years after started smoking in current smokers
# UKB_masterSW$smoked_year <- ifelse(UKB_masterSW$smokerYN == "Non-Smoker" & UKB_masterSW$start_smoke > UKB_masterSW$start_age, 1, UKB_masterSW$smoked_year)
# 
# UKB_masterSW$smoke_20yr <- factor(UKB_masterSW$smoke_20yr)

#=================================================================================================================

#get missing MS var - change MS NA to MS zero
# UKB_masterSW$MS <- ifelse(UKB_masterSW$MS_year > 1, 1,0)
# table(UKB_masterSW$MS, useNA = "always")
# UKB_masterSW$MS <- ifelse(is.na(UKB_masterSW$MS_year), 0, 1)
# table(UKB_masterSW$MS)

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
table(UKB_masterSW$healthy, useNA = "always")

#take out the unhealthy, leave MS and healthy
UKB_masterSW_healthy <- UKB_masterSW[UKB_masterSW$healthy == "Healthy" | UKB_masterSW$MS_YN == 1, ]

#change missing MS to didn't have MS, year = 0
#UKB_masterSW_healthy$MS <- ifelse(is.na(UKB_masterSW_healthy$MS_year), 0, UKB_masterSW_healthy$MS)

#make a MS_YN variable
# UKB_masterSW$MS_YN <- 0
# UKB_masterSW$MS_YN[UKB_masterSW$MS_YN == 1] <- 1
# table(UKB_masterSW$MS_YN, useNA="always")
# 
# UKB_masterSW$MS[is.na(UKB_masterSW$MS_YN)] <- 0
# UKB_masterSW$MS_YN <- factor(UKB_masterSW$MS_YN)
# UKB_masterSW$MS_YN <- droplevels(UKB_masterSW$MS_YN)
# str(UKB_masterSW$MS_YN)

# ==========================          make SW vars ==================================================

table(UKB_masterSW$bracket_SW_type, useNA="always")

UKB_masterSW$SW_YN <- 0 # ever did SW
UKB_masterSW$SW_YN <- ifelse (UKB_masterSW$bracket_total_hr_nightSW > 0 |
                                        UKB_masterSW$bracket_total_hr_mixSW > 0 |
                                        UKB_masterSW$bracket_total_hr_daySW > 0, 1, UKB_masterSW$SW_YN)
                                        
UKB_masterSW$SW_YN <- ifelse(!is.na(UKB_masterSW$current_shift_work) &
                                       (UKB_masterSW$current_shift_work == "Always" |
                                          UKB_masterSW$current_shift_work == "Sometimes" |
                                          UKB_masterSW$current_shift_work == "Usually"),
                                     1, UKB_masterSW$SW_YN)
table(UKB_masterSW$SW_YN, useNA = "always")      # add current shiftwork
UKB_masterSW$SW_YN <- factor(UKB_masterSW$SW_YN)



UKB_masterSW$NSW_YN <- 0  # ever did NSW
UKB_masterSW$NSW_YN <- ifelse (UKB_masterSW$bracket_total_hr_nightSW > 0 |
                                        UKB_masterSW$bracket_total_hr_mixSW > 0 
                                        , 1, UKB_masterSW$NSW_YN)

UKB_masterSW$NSW_YN <- ifelse(!is.na(UKB_masterSW$current_night_shift) &
                                       (UKB_masterSW$current_night_shift == "Always" |
                                          UKB_masterSW$current_night_shift == "Sometimes" |
                                          UKB_masterSW$current_night_shift == "Usually"),
                                     1, UKB_masterSW$NSW_YN)
table(UKB_masterSW$NSW_YN, useNA = "always")      # add current shiftwork
UKB_masterSW$NSW_YN <- factor(UKB_masterSW$NSW_YN)

# ever did early shiftwork 15-20
UKB_masterSW$early_SW_YN <- 0
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" &
                                            UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                            UKB_masterSW$early_SW_YN)
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" &
                                              UKB_masterSW$bracket_total_hr_daySW > 0 , 1, 
                                            UKB_masterSW$early_SW_YN)
UKB_masterSW$early_SW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" &
                                              UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                            UKB_masterSW$early_SW_YN)
table(UKB_masterSW$early_SW_YN)
UKB_masterSW$early_SW_YN 

x<-UKB_masterSW[UKB_masterSW$agebracket=="15-20",] 
table(x$early_SW_YN)

UKB_masterSW$early_SW_YN <- factor(UKB_masterSW$early_SW_YN)
                                       
# ever did early night shiftwork 15-20
UKB_masterSW$early_NSW_YN <- 0
UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" &
                                              UKB_masterSW$bracket_total_hr_nightSW > 0 , 1, 
                                            UKB_masterSW$early_NSW_YN)
UKB_masterSW$early_NSW_YN <- ifelse (UKB_masterSW$agebracket=="15-20" &
                                              UKB_masterSW$bracket_total_hr_mixSW > 0 , 1, 
                                            UKB_masterSW$early_NSW_YN)
table(UKB_masterSW$bracket_total_hr_nightSW, useNA = "always")
UKB_masterSW$early_NSW_YN <- factor(UKB_masterSW$early_NSW_YN)

UKB_masterSW <- UKB_masterSW[!is.na(UKB_masterSW$eid), ]

# missing values in job category not allowed.  All participants were working or on a gap, and no question omitted


##----------------   correct quantitative shiftwork parameters    ----------------------------------
UKB_masterSW$bracket_total_hr[UKB_masterSW$bracket_total_hr==0] <- NA
UKB_masterSW$bracket_total_hr_daySW[UKB_masterSW$bracket_total_hr_daySW==0] <- NA
UKB_masterSW$bracket_total_hr_nightSW[UKB_masterSW$bracket_total_hr_nightSW==0] <- NA
UKB_masterSW$bracket_total_hr_mixSW[UKB_masterSW$bracket_total_hr_mixSW==0] <- NA

UKB_masterSW$bracket_SW_per_work[UKB_masterSW$bracket_SW_per_work==0] <- NA
UKB_masterSW$bracket_nightSW_per_work[UKB_masterSW$bracket_nightSW_per_work ==0] <- NA
UKB_masterSW$bracket_mixSW_per_work[UKB_masterSW$bracket_mixSW_per_work==0] <- NA

summary(UKB_masterSW$bracket_total_hr_daySW, useNA="always")
summary(UKB_masterSW$bracket_total_hr_daySW, useNA="always")

prop.table(table(UKB_masterSW$bracket_SW_type,UKB_masterSW$agebracket), margin=2)

#make percentage in work variable - note that NA or 0 hours are gaps, not possible to have NA, either work or gap
UKB_masterSW$workingYN <- ifelse(is.na(UKB_masterSW$bracket_total_hr), 0,1)
table(UKB_masterSW$workingYN)
summary(UKB_masterSW$bracket_total_hr)

#age range
UKB_masterSW$start_age <- substr(UKB_masterSW$agebracket,1,2)
UKB_masterSW$stop_age <-  substr(UKB_masterSW$agebracket,4,5)




# # ================           set all values for shiftwork after diagnosis to NA for sensitivity analysis only    ========
# if (UKB_masterSW$dementia_year | UKB_masterSW$PD_year | UKB_masterSW$MS_year)
# 
# UKB_masterSW$bracket_total_hr_daySW
# UKB_masterSW$bracket_total_hr_nightSW
# UKB_masterSW$bracket_total_hr_mixSW
# UKB_masterSW$bracket_SW_per_work
# UKB_masterSW$bracket_nightSW_per_work
# UKB_masterSW$bracket_daySW_per_work
# UKB_masterSW$bracket_mixSW_per_work
# # ================      ========
 

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
"early_NSW_YN"
"early_SW_YN"
"SW_YN"
"NSW_YN"
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
vars <- UKB_masterSW %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  str_c('"', ., '"') %>% 
  str_c(collapse = " + ") %>% 
  cat()

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


#copy values for early life shiftwork to all other levels of agebracket, not just 15-20
table(UKB_masterSW_updated$early_SW_YN)
df <- UKB_masterSW_updated
# Find the value of SW_YN for rows where both eid and agebracket match the condition
sw_15_20 <- df$SW_YN[df$agebracket == "15-20" & !is.na(df$SW_YN)]

# Find the eids where agebracket is not "15-20"
eids_not_15_20 <- df$eid[df$agebracket != "15-20"]

# Update the SW_YN for other rows with matching eid
for (eid in eids_not_15_20) {
  df$SW_YN[df$eid == eid] <- sw_15_20[df$eid[df$agebracket == "15-20"] == eid]
}


############################################################################################################### 
# Table 1   Demography stratify by MS
################################################################################################################

#make variable to stratify by NDD
UKB_masterSW$NDD <- 0
UKB_masterSW$NDD <- ifelse(UKB_masterSW$MS_YN == 1, "MS", 
                 ifelse(UKB_masterSW$PD_YN == 1, "PD",
                        ifelse(UKB_masterSW$dementia_YN == 1, "Dementia"
                              ,0)))

table(UKB_masterSW$NDD,useNA="always")

#outcome
"PD_YN" 
"MS_YN"
"dementia_YN"

# table (as.numeric(UKB_master$MS_YN, useNA = "always"))
# na_index <- is.na(UKB_masterSW_healthy$MS_YN)
# UKB_masterSW_healthy$MS_YN[na_index] <- 0
# UKB_masterSW_healthy$MS_YN <- droplevels(factor(UKB_masterSW_healthy$MS_YN))
# table(UKB_masterSW_healthy$PD_YN, useNA = "always")

UKB_masterSW$smoke_20yr <- factor(UKB_masterSW$smoke_20yr)

# this is nicely formated table1, add pvalues from tableone function below
table1(~ sex + age + townsend + birth_latitude + #demography
         BMI_cat +  healthy + sleep_cat + chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestype
         smoker + smoke_start + smoke_20yr  + #smoking
         early_NSW_YN + early_SW_YN + NSW_YN + SW_YN # shiftwork 15-20
         | NDD, data=UKB_masterSW[UKB_masterSW$agebracket=="15-20",], overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)

#check that some MS did shiftwork
result <- UKB_masterSW %>%
  group_by(eid) %>%
  summarise(SW = ifelse(any(SW_YN == "Shiftworker") & any(MS == 1), 1, 0))

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                      'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                      'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                      'smoker' , 'smoke_start' , 'smoke_20yr'  , #smoking
                                      'early_NSW_YN' , 'early_SW_YN' , 'NSW_YN' , 'SW_YN' ),
                           strata = c("MS_YN"), 
                           data = UKB_masterSW[UKB_masterSW$agebracket=="15-20",], 
                           #test = FALSE, 
                           factorVars = c())

print(tableOne)

##############################################################################################################
# 
# Table 2   Descriptive Statistics of Shiftwork stratified by brackets 
# 
##############################################################################################################


table1(~  workingYN + bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work | agebracket, data=UKB_masterSW, overall=FALSE, render.continuous=my.render.cont, render.missing = NULL,render.categorical=my.render.cat)

# this is only 15-20
table1(~ workingYN + bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work  | NDD, data = UKB_masterSW[UKB_masterSW$agebracket == "15-20",], render.missing = NULL, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

filtered_rows <- UKB_masterSW[UKB_masterSW$agebracket == "15-20" & UKB_masterSW$MS_YN == 1, ]



tableOne <- CreateTableOne(vars = c('workingYN' , 'bracket_total_hr' , 'bracket_total_hr_daySW' , 'bracket_total_hr_nightSW' , 'bracket_total_hr_mixSW' , 'bracket_SW_type' , 'bracket_SW_per_work' , 'bracket_nightSW_per_work' , 'bracket_daySW_per_work' , 'bracket_mixSW_per_work' ), strata = c("NDD"), data = UKB_masterSW[UKB_masterSW$agebracket == "15-20",], factorVars = c())

prprprint(tableOne)

tableOne <- CreateTableOne(vars = c("bracket_total_hr" , "bracket_total_hr_daySW" , "bracket_total_hr_nightSW" , "bracket_total_hr_mixSW" , "bracket_SW_type" , "bracket_SW_per_work" , "bracket_nightSW_per_work" , "bracket_daySW_per_work" , "bracket_mixSW_per_work" , "allSW" , "SW_summary" , "night_shift" , "shift_work"), strata = c("MS"), data = mydata[mydata$agebracket != "15-20",], factorVars = c())
print(tableOne)

x <- UKB_masterSW[UKB_masterSW$agebracket == "15-20",]
x$bracket_total_hr_daySW

##############################################################################################################
#
# Model - SW in early life no age bracket data
#
################################################################################################################

sex + age + townsend + birth_latitude + #demography
         BMI_cat +  healthy + sleep_cat + chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestype
         smoker + smoke_start + smoke_20yr  + #smoking
         early_NSW_YN + early_SW_YN + NSW_YN + SW_YN # shiftwork 15-20
       | NDD, data=UKB_masterSW[UKB_masterSW$agebracket=="15-20",], overall=FALSE

# Check model assumptions
check_model(model3)
+ early_SW_YN ++ NSW_YN
table(UKB_masterSW$agebracket)
+ early_SW_YN + NSW_YN ++ early_SW_YN 
#MS
model_MS <- glm(MS_YN ~ age + sex + ethnicity_5 + townsend + birth_latitude + time_outdoors_summer +
             child_obesity  + SW_YN  ,
               data = UKB_masterSW[UKB_masterSW$agebracket=="21-25",], family = "binomial")
summary(model_MS)  
+ early_SW_YN 
#PD
model_PD <- glm(PD_YN ~ age + sex + ethnicity_5 + townsend + birth_latitude + time_outdoors_summer +
                  child_obesity + SW_YN   ,
                data = UKB_masterSW[UKB_masterSW$agebracket=="21-25",], family = "binomial")
summary(model_PD)  

#MS
model_dementia <- glm(dementia_YN ~ age + sex + ethnicity_5 + townsend + birth_latitude + time_outdoors_summer +
                  child_obesity  + SW_YN  ,
                data = UKB_masterSW[UKB_masterSW$agebracket=="21-25",], family = "binomial")
summary(model_dementia)  

+  NSW_YN 
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

