# script to analyse biobank data for metabolomic seasonality trend

#load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(season)
library(ggplot2)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\Metabolite Analyses\\analysis_met.Rda")

# ANALYSE: AMINO ACIDS ####
#amino acid df
aa_df <- data.frame(as.numeric(NA))
#Schizophrenia, schizotypal and delusional disorders
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
  geom_boxplot()+is 
  ggtitle("Tyrosine Annual Profile")+
  scale_x_continuous("assess_month", labels = as.character(creatinine_df$assess_month), breaks = creatinine_df$assess_month)