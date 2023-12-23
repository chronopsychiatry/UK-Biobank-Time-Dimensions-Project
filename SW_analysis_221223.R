# Data Analysis of UKB Lifetime SW and NDD

library(table1)
library(tableone)
library(finalfit)
library(dplyr)
library(knitr)
library(sjPlot)
library(stringr)

mydata <- merge(shiftwork, UKB_master, by="eid")
names(mydata)

#variables to include

#hist SW 
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
"allSW"                      
"SW_YN"                      
"SW_summary"                                       

#demographic
"sex"                        
"age"                        
"year_born"                  
"month_born"                
"ethnicity"                  
"assess_latitude"            
"deprivation_index_england" 
"deprivation_index_scotland" 
"deprivation_index_wales"    
"age_completed_education"
"qualifications"            

"country_birth_uk"
"country_birth_nonuk"        
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
"MS"

#make MS YN variable
mydata$MS <- factor(ifelse(!is.na(mydata$MS_year), 1, 0) )  
UKB_master$MS <- factor(ifelse(!is.na(UKB_master$MS_year), 1, 0) )     

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
c(
  "", 
  "geoMean" = format(round(EnvStats::geoMean(x), 3), nsmall = 3), 
  "geoSD"   = format(round(EnvStats::geoSD(x),   3), nsmall = 3)
)
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

######################################################################################################################
# 
# Table 1   Demography stratify by MS
# 
######################################################################################################################
# to do - find townsend, birthplace latitude, short ethnicity var, PA self report

# this is nicely formated table1, add pvalues from tableone function below
table1(~  sex + age + ethnicity + age_completed_education + smoking_status + alcohol_intake + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI|MS, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

#data for 15-20, used for general factors (ignore age bracket)
tabledata <- mydata[mydata$agebracket=="15-20",]

#check that some MS did shiftwork
result <- mydata %>%
  group_by(eid) %>%
  summarise(SW = ifelse(any(SW_YN == "Shiftworker") & any(MS == 1), 1, 0))

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c("age","sex", "ethnicity", "age_completed_education" , "smoking_status" , "alcohol_intake" , "depress_psych" , "allSW","SW_YN",  "SW_summary", "time_outdoors_summer" ,  "sleep_duration" , "chronotype" , "BMI"), strata = c("MS"), data = tabledata, factorVars = c())

print(tableOne)

######################################################################################################################
# 
# Table 2   Descriptive Statistics of Shiftwork stratified by age bracket and MS
# 
######################################################################################################################

vars

# this is nicely formated table1, add pvalues from tableone function below
table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work + allSW + SW_summary + night_shift + shift_work|agebracket + MS, data=mydata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)


