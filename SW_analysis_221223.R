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

#merge shiftowrk and core variables
mydata <- merge(shiftwork, UKB_master, by="eid")

delete <- c("X131042.0.0.x","X131043.0.0.x" ,"MS_source.x", "MS_year.x", "X.y",   "X131042.0.0.y" ,  "X131043.0.0.y", "MS_source.y",  "MS_year.y", "X.y" ,"X131042.0.0" , "X131043.0.0" )

# Delete columns using negative indexing
mydata <- mydata[, -which(names(mydata) %in% delete)]













#             move to getvar script        


mydata <- mydata %>%
  mutate(ethnicity_5 = case_when(
    
    ethnicity == "Prefer not to answer" ~ NA,
    ethnicity == "Do not know" ~ NA,
    ethnicity == "White" ~ "White",
    ethnicity == "Mixed" ~ "Mixed",
    ethnicity == "Asian or Asian British" ~ "Asian",
    ethnicity == "Black or Black British" ~ "Black",
    ethnicity == "Chinese" ~ "Chinese",
    ethnicity == "Other ethnic group" ~ "Other",
    ethnicity == "British" ~ "White",
    ethnicity == "Irish" ~ "White",
    ethnicity == "Any other white background" ~ "White",
    ethnicity == "White and Black Caribbean" ~ "Mixed",
    ethnicity == "White and Black African" ~ "Mixed",
    ethnicity == "White and Asian" ~ "Mixed",
    ethnicity == "Any other mixed background" ~ "Mixed",
    ethnicity == "Indian" ~ "Asian",
    ethnicity == "Pakistani" ~ "Asian",
    ethnicity == "Bangladeshi" ~ "Asian",
    ethnicity == "Any other Asian background" ~ "Asian",
    ethnicity == "Caribbean" ~ "Black",
    ethnicity == "African" ~ "Black",
    ethnicity == "Any other Black background" ~ "Black",
     TRUE ~ NA
  )
  )     

#childhood obesity
childhood_obesity <- read.delim("./childhood_obesity.tsv")
names (childhood_obesity) <- c("eid", "child_obesity")
comment(childhood_obesity$child_obesity)<-c("Data field = 1687.0.0")

levels_CO <- c(1, 2, 3, NA, NA)
labels_CO <- c("Thinner", "Plumper", "About average","NA",	"NA")
childhood_obesity$child_obesity <- factor(childhood_obesity$child_obesity, labels = lablels_CO, levels = levels_CO)
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Do not know")] <- NA
childhood_obesity$child_obesity[which(childhood_obesity$child_obesity == "Prefer not to answer")] <- NA
childhood_obesity$child_obesity<-droplevels(childhood_obesity$child_obesity)
mydata <- merge (mydata, childhood_obesity, by = "eid")

#get townsend index
townsend <- read.delim("C:/Users/Admin/OneDrive - Maynooth University/UK Biobank Shiftwork/townsend.tsv")
names(townsend) <- c("eid", "townsend")
comment(townsend$townsend)<-c("Data field = 22189.0.0")
mydata <- merge(mydata, townsend, by="eid")

# add latitude of birth
latitude_birth_lookup <- read.csv("./latitude_birth_lookup.csv")
mydata <- merge(mydata,latitude_birth_lookup[,c(1:2)], all.x = TRUE, by = "country_birth_nonuk")
mydata <- mydata[order(mydata$eid, decreasing = FALSE), ]

#add UK latitude
mydata$birth_latitude <- ifelse(mydata$country_birth_uk == "England", 52,
                          ifelse(mydata$country_birth_uk == "Scotland", 56,
                                 ifelse(mydata$country_birth_uk == "Wales", 52,
                                        mydata$latitude)))


#make MS YN variable
mydata$MS <- factor(ifelse(!is.na(mydata$MS_year), 1, 0) )  
UKB_master$MS <- factor(ifelse(!is.na(UKB_master$MS_year), 1, 0) ) 


#make an early life shiftwork variable
mydata$early_life_SW <- ifelse(as.character(mydata$agebracket) == "15-20" & as.character(mydata$SW_YN) == "Shiftworker",1,0)

#clean chronotype
mydata$chronotype <- ifelse(mydata$chronotype == "Do not know", NA,
                                ifelse(mydata$chronotype == "Prefer not to answer", NA,
                                       mydata$chronotype))
table(mydata$chronotype, useNA = "always")
mydata$chronotype <- factor(mydata$chronotype)


#clean  sex
mydata$sex <- factor(mydata$sex)
table(mydata$sex, useNA= "always")

#clean age
describe(mydata$age)

#clean ethnicity_5
str(mydata$ethnicity_5)
mydata$ethnicity_5 <- factor(mydata$ethnicity_5)
table(mydata$ethnicity_5, useNA = "always")

#clean mydata$age_completed_education
str(mydata$age_completed_education)
describe(mydata$age_completed_education) 
mydata$age_completed_education <- ifelse(mydata$age_completed_education == -2, 0,
                            ifelse(mydata$age_completed_education == -1, NA,
                                   ifelse(mydata$age_completed_education == -3, NA,
                                          mydata$age_completed_education)))
#-2 represents "Never went to school"
#-1 represents "Do not know"
#-3 represents "Prefer not to answer"


# clean mydata$smoking_status ---         need to get orginal variable from UKB Master again
mydata$smoking_status <- factor(mydata$smoking_status)
table (mydata$smoking_status, useNA="always")
mydata$smoking_status <- ifelse(mydata$smoking_status == 'Prefer not to answer', NA, mydata$smoking_status)

# Clean alcohol_intake
str(mydata$alcohol_intake)
table(mydata$alcohol_intake, useNA="always") 
mydata$alcohol_intake <- ifelse(mydata$alcohol_intake == 'Prefer not to answer', NA, mydata$alcohol_intake)

# clean depress_psych 
str(mydata$depress_psych)
table(mydata$depress_psych, useNA="always") 
mydata$depress_psych <- ifelse(mydata$depress_psych == 'Prefer not to answer', NA, mydata$depress_psych)
mydata$depress_psych<- ifelse(mydata$depress_psych == 'Prefer not to answer', NA,
                                         ifelse(mydata$depress_psych == "Do not know", NA,
                                                mydata$depress_psych))
# clean time_outdoors_summer
hist(mydata$time_outdoors_summer)
describe(mydata$time_outdoors_summer)
# -10	Less than an hour a day
# -1	Do not know
# -3	Prefer not to answer

mydata$time_outdoors_summer <- ifelse(mydata$time_outdoors_summer == -10, .5,
                                      ifelse(mydata$time_outdoors_summer == -1, NA,
                                          ifelse(mydata$time_outdoors_summer == -3, NA,
                                            mydata$time_outdoors_summer)))



#clean sleep_duration 
#
str(mydata$sleep_duration)
describe(mydata$sleep_duration)
#value -3 (Prefer not to answer)
#value -1 (Do not know)
mydata$sleep_duration <- ifelse(mydata$sleep_duration == -3, NA,
                                      ifelse(mydata$sleep_duration == -1, NA,
                                            mydata$sleep_duration))

# clean BMI
describe(mydata$BMI) 
summary(mydata$BMI) 

#clean SW variables - careful with missing values and assumming NA is not working
describe(mydata$allSW)
table(mydata$SW_YN, useNA = "always")
table(mydata$SW_summary, useNA = "always")

describe(mydata$bracket_total_hr)

#data for 15-20, used for general factors (ignore age bracket)
tabledata <- mydata[mydata$agebracket=="15-20",]

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

describe(mydata)

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
"child_obesity"
"townsend"
"latitude_birth"
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

######################################################################################################################
# 
# Table 1   Demography stratify by MS
# 
######################################################################################################################
# to do - find townsend, birthplace latitude, short ethnicity var, PA self report

# this is nicely formated table1, add pvalues from tableone function below
table1(~  sex + age + ethnicity_5+ townsend + age_completed_education + smoking_status + alcohol_intake + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI|MS, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)


#check that some MS did shiftwork
result <- mydata %>%
  group_by(eid) %>%
  summarise(SW = ifelse(any(SW_YN == "Shiftworker") & any(MS == 1), 1, 0))

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c("age","sex", "ethnicity_5", "townsend" + "age_completed_education" , "smoking_status" , "alcohol_intake" , "depress_psych" , "allSW","SW_YN",  "SW_summary", "time_outdoors_summer" ,  "sleep_duration" , "chronotype" , "BMI"), strata = c("MS"), data = tabledata, factorVars = c())

print(tableOne)

######################################################################################################################
# 
# Table 2   Descriptive Statistics of Shiftwork stratified by 15-20 and then mean of all other brackets and MS
# 
######################################################################################################################

# this is only 15-20
table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work + allSW + SW_summary + night_shift + shift_work|agebracket + MS, data=tabledata, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

table1(~  bracket_total_hr + bracket_total_hr_daySW + bracket_total_hr_nightSW + bracket_total_hr_mixSW + bracket_SW_type + bracket_SW_per_work + bracket_nightSW_per_work + bracket_daySW_per_work + bracket_mixSW_per_work + allSW + SW_summary + night_shift + shift_work|MS, data=mydata[mydata$agebracket != "15-20",], overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

## Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c("bracket_total_hr" , "bracket_total_hr_daySW" , "bracket_total_hr_nightSW" , "bracket_total_hr_mixSW" , "bracket_SW_type" , "bracket_SW_per_work" , "bracket_nightSW_per_work" , "bracket_daySW_per_work" , "bracket_mixSW_per_work" , "allSW" , "SW_summary" , "night_shift" , "shift_work"), strata = c("MS"), data = tabledata, factorVars = c())

print(tableOne)

tableOne <- CreateTableOne(vars = c("bracket_total_hr" , "bracket_total_hr_daySW" , "bracket_total_hr_nightSW" , "bracket_total_hr_mixSW" , "bracket_SW_type" , "bracket_SW_per_work" , "bracket_nightSW_per_work" , "bracket_daySW_per_work" , "bracket_mixSW_per_work" , "allSW" , "SW_summary" , "night_shift" , "shift_work"), strata = c("MS"), data = mydata[mydata$agebracket != "15-20",], factorVars = c())
print(tableOne)


#######################################################################################################################
#
# Model 
#
#######################################################################################################################

#M1 - demography
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude ,
                data = tabledata, family = "binomial")
summary(model)  

#M2 - physical mental health and lifestyle
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude + 
              child_obesity + sleep_duration + chronotype + BMI + smoking_status + alcohol_intake  + depress_psych,              
             data = tabledata, family = "binomial")
summary(model)  
table(tabledata$shift_work, useNA = "always")

#M3 - work and environment
model <- glm(MS ~ age + sex + ethnicity_5 + townsend + birth_latitude + 
               child_obesity + sleep_duration + chronotype + BMI + smoking_status + alcohol_intake  + depress_psych + 
               time_outdoors_summer + shift_work + early_life_SW,
               
             data = tabledata, family = "binomial")
summary(model)  






, data = tabledata, family = "binomial")





# Fit logistic regression model for early life shiftwork
model <- glm(MS ~  + smoking_status + alcohol_intake + townsend + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI + bracket_total_hr_nightSW + age + sex + bracket_SW_per_work, data = tabledata, family = "binomial")

# Display the summary of the model
summary(model)

# Fit logistic regression model for total life shiftwork
model <- glm(MS ~ ethnicity_5 + early_life_SW + smoking_status + alcohol_intake + townsend + depress_psych + agebracket + time_outdoors_summer +  sleep_duration + chronotype + BMI + bracket_total_hr_nightSW + age + sex + bracket_SW_per_work, data = mydata, family = "binomial")

# Display the summary of the model
summary(model)



# Logistic regression with age bracket as a random effect
model <- glmer(MS ~ ethnicity_5 + sex + age + early_life_SW + smoking_status + townsend + alcohol_intake + depress_psych + time_outdoors_summer +  sleep_duration + chronotype + BMI + bracket_total_hr_nightSW + bracket_SW_per_work + (1 | agebracket), data = mydata, family = binomial)

# Summary of the mixed-effects logistic regression model
summary(model)
