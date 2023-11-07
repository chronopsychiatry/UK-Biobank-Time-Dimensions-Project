# script to get healthy control participants from biobank data
# to analyse normative metabolic profiles across seasons

#load libraries
library(tidyverse)
library(dplyr)
library(readr)

# set cwd
setwd("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics")

# load in UKB_master
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\core dataset\\UKB_master.Rda")

#make new df which i will now work with
bdmet_df <- UKB_master
rm(UKB_master)

#load control exclusion vars
control_excl_vars <- readr::read_tsv("./normative mets/control_exclusion_participant.tsv")
control_excl_vars <- control_excl_vars %>% 
  rename('eid' = 'Participant ID')

gp_codes <- readr::read_tsv("./normative mets/data_gp_clinical.tsv")

#merge new vars with bdmet_df -- 3 people get removed
bdmet_df_excl <- merge(bdmet_df,control_excl_vars, by="eid")

#remove people who shift/night shift work
no_shift <- bdmet_df_excl[!grepl("Prefer not to answer|Do not know|Sometimes|Usually|Always", bdmet_df_excl$shift_work),]
no_shift_2 <- no_shift[!grepl("Prefer not to answer|Do not know|Sometimes|Usually|Always", no_shift$night_shift),]

#remove people with diabetes dx
unique(bdmet_df$`Diabetes diagnosed by doctor | Instance 0`)
no_dm <- no_shift_2[!grepl("Yes|Do not know|NA|Prefer not to answer", no_shift_2$`Diabetes diagnosed by doctor | Instance 0`),]
unique(bdmet_df$`Gestational diabetes only | Instance 0`)
no_dm_2 <- no_dm[!grepl("Yes|Do not know|Not applicable|NA|Prefer not to answer", no_dm$`Gestational diabetes only | Instance 0`),]

#remove people with self reported bipolar
str_count(bdmet_df_excl$'Diagnoses-ICD10', "Varicose")


