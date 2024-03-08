# script to analyse biobank data for metabolomic seasonality trend

#load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(season)
library(ggplot2)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\Metabolite Analyses\\analysis_met.Rda")

# initial descriptive analyses
alanine_df <- analysis_met %>%
  group_by(Group) %>%
  summarise(mean = mean(alanine.x, na.rm=TRUE), SD = sd(alanine.x, na.rm=TRUE), N = length(alanine.x))


# ANALYSE: AMINO ACIDS ####
#amino acid df
aa_df <- data.frame(as.numeric(NA))
 
aa_df$aa <- as.numeric(NA)
aa_df$group <- as.numeric(NA)
aa_df$month <- as.numeric(NA)
aa_df$mean <- as.numeric(NA)
aa_df$sd <- as.numeric(NA)
aa_df <- aa_df[-c(1)]

#alanine ####
alanine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(alanine.x, na.rm=TRUE), SD = sd(alanine.x, na.rm=TRUE), N = length(alanine.x))
#add new column to both with study group
alanine_df <- alanine_df %>%
  mutate(metabolite = 'alanine')

#creatinine ####
creatinine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(creatinine.x, na.rm=TRUE), SD = sd(creatinine.x, na.rm=TRUE), N = length(creatinine.x))
#add new column to both with study group
creatinine_df <- creatinine_df %>%
  mutate(metabolite = 'creatinine')

#glutamine ####
glutamine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(glutamine, na.rm=TRUE), SD = sd(glutamine, na.rm=TRUE), N = length(glutamine))
#add new column to both with study group
glutamine_df <- glutamine_df %>%
  mutate(metabolite = 'glutamine')

#glycine ####
glycine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(glycine, na.rm=TRUE), SD = sd(glycine, na.rm=TRUE), N = length(glycine))
#add new column to both with study group
glycine_df <- glycine_df %>%
  mutate(metabolite = 'glycine')

#histidine ####
histidine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(histidine, na.rm=TRUE), SD = sd(histidine, na.rm=TRUE), N = length(histidine))
#add new column to both with study group
histidine_df <- histidine_df %>%
  mutate(metabolite = 'histidine')

#isoleucine ####
isoleucine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(isoleucine, na.rm=TRUE), SD = sd(isoleucine, na.rm=TRUE), N = length(isoleucine))
#add new column to both with study group
isoleucine_df <- isoleucine_df %>%
  mutate(metabolite = 'isoleucine')

#leucine ####
leucine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(leucine, na.rm=TRUE), SD = sd(leucine, na.rm=TRUE), N = length(leucine))
#add new column to both with study group
leucine_df <- leucine_df %>%
  mutate(metabolite = 'leucine')

#phenylalanine ####
phenylalanine_df <- analysis_met %>%
    group_by(assess_month, Group) %>%
  summarise(mean = mean(phenylalanine, na.rm=TRUE), SD = sd(phenylalanine, na.rm=TRUE), N = length(phenylalanine))
#add new column to both with study group
phenylalanine_df <- phenylalanine_df %>%
  mutate(metabolite = 'phenylalanine')

#tyrosine ####
tyrosine_df <- analysis_met %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(tyrosine, na.rm=TRUE), SD = sd(tyrosine, na.rm=TRUE), N = length(tyrosine))
#add new column to both with study group
tyrosine_df <- tyrosine_df %>%
  mutate(metabolite = 'tyrosine')

#join into 1 aa_df - visualise without creatinine
aa <- rbind(alanine_df, glutamine_df, glycine_df, histidine_df, isoleucine_df, leucine_df, phenylalanine_df, tyrosine_df)

#line plot only controls
ggplot(tyrosine_df, aes(assess_month, mean))+
  geom_line(data=tyrosine_df[tyrosine_df$Group=="control", ])+
  ggtitle("tyrosine Annual Profile")+
  geom_point()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                position=position_dodge(0.05))+
  scale_x_continuous("assess_month", labels = as.character(creatinine_df$assess_month), breaks = creatinine_df$assess_month)

#line plot both groups  
ggplot(tyrosine_df, aes(assess_month, mean, color=Group))+
  geom_line()+ggtitle("tyrosine Annual Profile")+
  geom_point()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                position=position_dodge(0.05))+
  scale_x_continuous("assess_month", labels = as.character(creatinine_df$assess_month), breaks = creatinine_df$assess_month)

#box plot all participants
ggplot(analysis_met, aes(x=factor(assess_month), y=tyrosine))+
  geom_boxplot()+
  ggtitle("Tyrosine Annual Profile")+
  scale_x_continuous("assess_month", labels = as.character(creatinine_df$assess_month), breaks = creatinine_df$assess_month)


# analyses for g_haem hypothesis ####
ghaem_df <- analysis_met %>%  filter(!is.na(g_haem))
ghaem_df<- ghaem_df %>%  filter(!g_haem>200)


# summary statistics - mean&sd for continuous variables:
demo_mean_df <- ghaem_df %>%
  group_by(Group) %>%
  summarise_at(c('age', 'BMI', 'age_completed_education', 'deprivation_index_england', 'deprivation_index_scotland', 'deprivation_index_wales'),
               mean, na.rm=TRUE)

demo_sd_df <- ghaem_df %>%
  group_by(Group) %>%
  summarise_at(c('age', 'BMI', 'age_completed_education', 'deprivation_index_england', 'deprivation_index_scotland', 'deprivation_index_wales'),
               sd, na.rm=TRUE)

#counts for discrete variables:
demo_sex_df <- ghaem_df %>%
  group_by(Group) %>%
  #mutate(sex = factor(sex)) %>% 
  count(sex)
demo_ethnicity_df <- ghaem_df %>%
  group_by(Group) %>%
  #mutate(sex = factor(sex)) %>% 
  count(ethnicity)
demo_disabled_df <- ghaem_df %>%
  group_by(Group) %>%
  #mutate(sex = factor(sex)) %>% 
  count(disability_self_report)                


#cosinor analysis mof glycated haemoglobin in all participants ####
#remove nas from g_haem column
ghaem_df <- analysis_met %>%  filter(!is.na(g_haem))
ghaem_df<- ghaem_df %>%  filter(!g_haem>200)

# do cosinor on daily data  
ghaem_daily_cosinor = cosinor(g_haem~1, date='assess_date', data=ghaem_df, type='daily')
summary(ghaem_daily_cosinor)
seasrescheck(ghaem_daily_cosinor$residuals) # check the residuals
plot(ghaem_daily_cosinor)

# do cosinor on monthly data
ghaem_df$assess_month <- as.numeric(as.character(ghaem_df$assess_month))
ghaem_monthly_cosinor = cosinor(g_haem~1, date='assess_month', data=ghaem_df, type='monthly')
summary(ghaem_monthly_cosinor)
seasrescheck(ghaem_monthly_cosinor$residuals) # check the residuals
plot(ghaem_monthly_cosinor)

# explore gly.haem differences between groups ####
# calculate mean & plot glycated haem
g_haem_summary <- ghaem_df %>%
  group_by(assess_month, Group) %>%
  summarise(mean = mean(g_haem, na.rm=TRUE), SD = sd(g_haem, na.rm=TRUE), N = length(g_haem))
#plot
ggplot(g_haem_summary, aes(assess_month, mean, color=Group))+
  geom_line()+ggtitle("glyc.haem Annual Profile")+
  geom_point()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                position=position_dodge(0.05))+
  scale_x_continuous("assess_month", labels = as.character(ghaem_df$assess_month), breaks = ghaem_df$assess_month)

# complete cosinor for group control
ghaem_control <- ghaem_df %>% filter(Group!='bipolar')
ghaem_control_monthly_cosinor <- cosinor(g_haem~1, date='assess_month', data=ghaem_control, type='monthly')
summary(ghaem_control_monthly_cosinor)
seasrescheck(ghaem_control_monthly_cosinor$residuals) # check the residuals
plot(ghaem_control_monthly_cosinor)

# complete cosinor for group bipolar
ghaem_bipolar <- ghaem_df %>% filter(Group!='control')
ghaem_bipolar_monthly_cosinor <- cosinor(g_haem~1, date='assess_month', data=ghaem_bipolar, type='monthly')
summary(ghaem_bipolar_monthly_cosinor)
seasrescheck(ghaem_bipolar_monthly_cosinor$residuals) # check the residuals
plot(ghaem_bipolar_monthly_cosinor)

  