#20.02.24 @agpr141

# script to get relevant study groups from biobank dataset:
# -> bipolar disorder group
# -> control group

# to analyse normative metabolic profiles across seasons
# control exclusion criteria as follows:
# - have metabolic dx (including diabetes); previous hx severe mental illness; eating disorder dx;
# - sleep/wake disorder dx; shift work hx;

# bipolar exclusion criteria as follows:
# - have metabolic dx (including diabetes); previous hx severe mental illness (excluding bipolar); eating disorder dx;
# - sleep/wake disorder dx; shift work hx;

### First-time set up ####

#load libraries
library(tidyverse)
library(dplyr)
library(readr)

# set cwd
setwd("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data")

# load in UKB_master
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\ukb_master.Rda")

# make new df which i will now work with
bdmet_df <- ukb_master
rm(ukb_master)

#  control exclusion vars
control_excl_vars <- readr::read_tsv("./normative mets/control_exclusion_participant.tsv")
control_excl_vars <- control_excl_vars %>% 
  rename('eid' = 'Participant ID')

# merge above 2 datasets into 1
bdmet_df_excl <- merge(bdmet_df,control_excl_vars, by="eid")

# remove bdmet_df and control_excl_vars datasets (no longer needed)
rm(bdmet_df, control_excl_vars)

### remove people who shift/night shift work ####
no_shift <- bdmet_df_excl[!grepl("Prefer not to answer|Do not know|Sometimes|Usually|Always", bdmet_df_excl$shift_work),] 
control_excl_vars <- no_shift[!grepl("Prefer not to answer|Do not know|Sometimes|Usually|Always", no_shift$night_shift),]

### Create icd10 exclusion criteria variables ####

excluded_numbers <- data.frame(as.numeric(NA))
#Schizophrenia, schizotypal and delusional disorders
excluded_numbers$F20 <- as.numeric(NA)
excluded_numbers$F21 <- as.numeric(NA)
excluded_numbers$F22 <- as.numeric(NA)
excluded_numbers$F23 <- as.numeric(NA)
excluded_numbers$F24 <- as.numeric(NA)
excluded_numbers$F25 <- as.numeric(NA)
excluded_numbers$F27 <- as.numeric(NA)
excluded_numbers$F28 <- as.numeric(NA)
#Mood (affective) disorders
excluded_numbers$F30 <- as.numeric(NA)
excluded_numbers$F31 <- as.numeric(NA)
excluded_numbers$F32 <- as.numeric(NA)
excluded_numbers$F33 <- as.numeric(NA)
excluded_numbers$F34 <- as.numeric(NA)
excluded_numbers$F38.1 <- as.numeric(NA)
excluded_numbers$F39 <- as.numeric(NA)
#Eating disorders
excluded_numbers$F50 <- as.numeric(NA)
#Diabetes
excluded_numbers$E10 <- as.numeric(NA)
excluded_numbers$E11 <- as.numeric(NA)
excluded_numbers$E12 <- as.numeric(NA)
excluded_numbers$E13 <- as.numeric(NA)
excluded_numbers$E14 <- as.numeric(NA)
#Metabolic disorders
excluded_numbers$E70_E72 <- as.numeric(NA)
excluded_numbers$E74 <- as.numeric(NA)
excluded_numbers$E75 <- as.numeric(NA)
excluded_numbers$E76 <- as.numeric(NA)
excluded_numbers$E77 <- as.numeric(NA)
excluded_numbers$E78 <- as.numeric(NA)
excluded_numbers$E79 <- as.numeric(NA)
excluded_numbers$E80 <- as.numeric(NA)
#Sleep-wake cycle disorders
excluded_numbers$G47.2 <- as.numeric(NA)
excluded_numbers$F51.2 <- as.numeric(NA)
#delete first blank column
excluded_numbers <- excluded_numbers[-c(1)]

# Search for & store exclusions instances####
# DX: Schizophrenia, schizotypal and delusional disorders ####
#F20 - Schizophrenia ####
exclusions_icd <- data.frame(control_excl_vars$eid)
exclusions_icd$F20 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F20[i] <- grepl("F20", row)}

#count how many true cases there are
excluded_numbers$F20  =  sum(exclusions_icd$F20, na.rm=TRUE)

#F21 - Schizotypal disorder ####
exclusions_icd$F21 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F21[i] <- grepl("F21", row)}

#count how many true cases there are
excluded_numbers$F21  =  sum(exclusions_icd$F21, na.rm=TRUE)

#F22 - Persistent delusional disorders ####
exclusions_icd$F22 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F22[i] <- grepl("F22", row)}

#count how many true cases there are
excluded_numbers$F22  =  sum(exclusions_icd$F22, na.rm=TRUE)

#F23 - acute & transient psychotic disorders####
exclusions_icd$F23 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F23[i] <- grepl("F23", row)}

#count how many true cases there are
excluded_numbers$F23  =  sum(exclusions_icd$F23, na.rm=TRUE)

#F24 - induced delusional disorder####
exclusions_icd$F24 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F24[i] <- grepl("F24", row)}

#count how many true cases there are
excluded_numbers$F24  =  sum(exclusions_icd$F24, na.rm=TRUE)

#F25 - schizoaffective disorders ####
exclusions_icd$F25 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F25[i] <- grepl("F25", row)}

#count how many true cases there are
excluded_numbers$F25  =  sum(exclusions_icd$F25, na.rm=TRUE)

#F27 - other nonorganic psychotic disorders ####
exclusions_icd$F27 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F27[i] <- grepl("F27", row)}

#count how many true cases there are
excluded_numbers$F27  =  sum(exclusions_icd$F27, na.rm=TRUE)

#F28 - unspecified nonorganic psychosis ####
exclusions_icd$F28 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F28[i] <- grepl("F28", row)}

#count how many true cases there are
excluded_numbers$F28  =  sum(exclusions_icd$F28, na.rm=TRUE)



# DX: Mood (affective) disorders ####
#F30 - manic episode ####
exclusions_icd$F30 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F30[i] <- grepl("F30", row)}

#count how many true cases there are
excluded_numbers$F30  =  sum(exclusions_icd$F30, na.rm=TRUE)

#F31 - bipolar affective disorder ####
exclusions_icd$F31 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F31[i] <- grepl("F31", row)}

#count how many true cases there are
excluded_numbers$F31  =  sum(exclusions_icd$F31, na.rm=TRUE)

#F32 - depressive episode ####
exclusions_icd$F32 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F32[i] <- grepl("F32", row)}

#count how many true cases there are
excluded_numbers$F32  =  sum(exclusions_icd$F32, na.rm=TRUE)

#F33 - recurrent depressive episode ####
exclusions_icd$F33 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F33[i] <- grepl("F33", row)}

#count how many true cases there are
excluded_numbers$F33  =  sum(exclusions_icd$F33, na.rm=TRUE)

#F34 - peristent mood (affective) disorders ####
exclusions_icd$F34 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F34[i] <- grepl("F34", row)}

#count how many true cases there are
excluded_numbers$F34  =  sum(exclusions_icd$F34, na.rm=TRUE)

#F38.1 - other mood (affective) disorders ####
exclusions_icd$F38.1 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F38.1[i] <- grepl("F38.1", row)}

#count how many true cases there are
excluded_numbers$F38.1  =  sum(exclusions_icd$F38.1, na.rm=TRUE)

#F39 - unspecified mood (affective) disorder ####
exclusions_icd$F39 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F39[i] <- grepl("F39", row)}

#count how many true cases there are
excluded_numbers$F39  =  sum(exclusions_icd$F39, na.rm=TRUE)

# DX: eating disorders (any) ####
#F50 - eating disorders ####
exclusions_icd$F50 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F50[i] <- grepl("F50", row)}

#count how many true cases there are
excluded_numbers$F50  =  sum(exclusions_icd$F50, na.rm=TRUE)

# DX: Diabetes (Any) ####
#E10 - type 1 diabetes ####
exclusions_icd$E10 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E10[i] <- grepl("E10", row)}

#count how many true cases there are
excluded_numbers$E10  =  sum(exclusions_icd$E10, na.rm=TRUE)

#E11 - type 2 diabetes ####
exclusions_icd$E11 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E11[i] <- grepl("E11", row)}

#count how many true cases there are
excluded_numbers$E11  =  sum(exclusions_icd$E11, na.rm=TRUE)

#E12 - malnutrition-related diabetes ####
exclusions_icd$E12 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E12[i] <- grepl("E12", row)}

#count how many true cases there are
excluded_numbers$E12  =  sum(exclusions_icd$E12, na.rm=TRUE)

#E13 - other specified diabetes ####
exclusions_icd$E13 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E13[i] <- grepl("E13", row)}

#count how many true cases there are
excluded_numbers$E13  =  sum(exclusions_icd$E13, na.rm=TRUE)

#E14 - unspecified diabetes ####
exclusions_icd$E14 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E14[i] <- grepl("E14", row)}

#count how many true cases there are
excluded_numbers$E14  =  sum(exclusions_icd$E14, na.rm=TRUE)



# DX: Metabolic disorders (any) ####
#E70 - E72 - disorders of amino-acid & fatty-acid metabolism ####
exclusions_icd$E70_E72 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E70_E72[i] <- grepl("E70 | E71| E72", row)}

#count how many true cases there are
excluded_numbers$E70_E72  =  sum(exclusions_icd$E70_E72, na.rm=TRUE)

#E74 - disorders of carbohydrate metabolism ####
exclusions_icd$E74 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E74[i] <- grepl("E74", row)}

#count how many true cases there are
excluded_numbers$E74  =  sum(exclusions_icd$E74, na.rm=TRUE)


#E75 - disorders of sphingolipid metabolism ####
exclusions_icd$E75 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E75[i] <- grepl("E75", row)}

#count how many true cases there are
excluded_numbers$E75  =  sum(exclusions_icd$E75, na.rm=TRUE)


#E76 - disorders of glycosaminoglycan metabolism ####
exclusions_icd$E76 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E76[i] <- grepl("E76", row)}

#count how many true cases there are
excluded_numbers$E76  =  sum(exclusions_icd$E76, na.rm=TRUE)

#E77 - disorders of glycoprotein metabolism ####
exclusions_icd$E77 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E77[i] <- grepl("E77", row)}

#count how many true cases there are
excluded_numbers$E77  =  sum(exclusions_icd$E77, na.rm=TRUE)

#E78 - disorders of lipoprotein metabolism ####
exclusions_icd$E78 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E78[i] <- grepl("E78", row)}

#count how many true cases there are
excluded_numbers$E78  =  sum(exclusions_icd$E78, na.rm=TRUE)

#E79 - disorders of purine and pyramidine metabolism ####
exclusions_icd$E79 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E79[i] <- grepl("E79", row)}

#count how many true cases there are
excluded_numbers$E79  =  sum(exclusions_icd$E79, na.rm=TRUE)

#E80 - disorders of porphyrin and bilirubin metabolism ####
exclusions_icd$E80 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$E80[i] <- grepl("E80", row)}

#count how many true cases there are
excluded_numbers$E80  =  sum(exclusions_icd$E80, na.rm=TRUE)


# DX: Disorders of sleep-wake schedule ####
#G47.2 - Disorders of the sleep-wake schedule  (organic) ####
exclusions_icd$G47.2 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$G47.2[i] <- grepl("G47.2", row)}

#count how many true cases there are
excluded_numbers$G47.2  =  sum(exclusions_icd$G47.2, na.rm=TRUE)

#F51.2 - Disorders of the sleep-wake schedule  (non-organic) ####
exclusions_icd$F51.2 <- as.numeric(NA)

for (i in 1:nrow(control_excl_vars)){
  row <- control_excl_vars$'Diagnoses - ICD10'[i]
  exclusions_icd$F51.2[i] <- grepl("F51.2", row)}

#count how many true cases there are
excluded_numbers$F51.2  =  sum(exclusions_icd$F51.2, na.rm=TRUE)


# save files as r data files ####
#rename exclusions_icd 1st column as eid 
exclusions_icd <- exclusions_icd %>% 
  rename('eid' = 'control_excl_vars.eid')
save(excluded_numbers,file="excluded_numbers.Rda")
save(exclusions_icd,file="excluded_icd.Rda")

### remove people with self-reported diabetes dx ####
#unique(no_shift_2$`Diabetes diagnosed by doctor | Instance 0`)
no_dm <- control_excl_vars[!grepl("Yes", control_excl_vars$'Diabetes diagnosed by doctor | Instance 0'),]
#unique(bdmet_df$`Gestational diabetes only | Instance 0`)
no_dm_2 <- no_dm[!grepl("Yes", no_dm$'Gestational diabetes only | Instance 0'),]

### remove people with probable single/recurrent major depression ####
no_md <- no_dm_2[!grepl("Probable", no_dm_2$`Bipolar and major depression status | Instance 0`),]

#merge icd search output variables with control_excl_vars ####
study_population <- merge(no_md,exclusions_icd, by="eid")

### CONTROL group: remove people who meet any of the exclusion criteria ####
ctrl_group <- study_population %>%
  filter(F20!='1' & F21!='1'& F22!='1' & F23!='1' & F24!='1' & F25!='1'& F27!='1' & F28!='1' &
           F30!='1' & F31!='1'& F32!='1'& F33!='1' & F34!='1' & F38.1!='1' & F39!='1' & 
           E10!='1'& E11!='1' & E12!='1' & E13!='1' & E14!='1' &
           F50!='1'&
           E70_E72!='1'& E74!='1' & E75!='1' & E76!='1' & E77!='1' & E78!='1' & E79!='1' & E80!='1' &
           G47.2!='1'& F51.2!='1')

#### CONTROL group: remove people with probable bipolar disorder
control_group <- ctrl_group[!grepl("Disorder", ctrl_group$'Bipolar and major depression status | Instance 0'),]

### BIPOLAR group: remove people who meet any of the exclusion criteria ####
bplr_group <- study_population %>%
  filter(F20!='1' & F21!='1'& F22!='1' & F23!='1' & F24!='1' & F25!='1'& F27!='1' & F28!='1' &
           F32!='1'& F33!='1' & F34!='1' & F38.1!='1' & F39!='1' & 
           E10!='1'& E11!='1' & E12!='1' & E13!='1' & E14!='1' &
           F50!='1'&
           E70_E72!='1'& E74!='1' & E75!='1' & E76!='1' & E77!='1' & E78!='1' & E79!='1' & E80!='1' &
           G47.2!='1'& F51.2!='1')

# create new bipolar group of people who have either: ICD10 bipolar disorder (F31) or have probable bipolar (Pell & Smith criteria)
bipolar_group <- dplyr::filter(bplr_group, grepl('Bipolar I Disorder|Bipolar II Disorder', `Bipolar and major depression status | Instance 0`)|bplr_group$F31!='0')


# SAVE GROUP DATAFRAMES ####
save(control_group,file="control_group_bdmet.Rda")
save(bipolar_group,file="bipolar_group_bdmet.Rda")

# Combine both group dataframes into one for future analysis purposes ####
#add new column to both with study group
control_nc <- control_group %>%
  mutate(Group = 'control')
bipolar_nc <- bipolar_group %>%
  mutate(Group = 'bipolar')

#join 2 dataframes
bdmet_df <- rbind(control_nc,bipolar_nc)
#save
save(bdmet_df,file="bdmet_df.Rda")

### Code for amendments/data revisiting to the above data ####
# load in existing R data sets linked to this work
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\excluded_icd.Rda")
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\excluded_numbers.Rda")

### random other bits of code ####
#remove people with self-reported bipolar/major dep?
#unique(no_dm_2$`Bipolar and major depression status | Instance 0`)
#no_bd_md <- no_dm_2[!grepl("Probable Recurrent major depression (moderate)|
#                           Probable Recurrent major depression (severe)|NA|Bipolar II Disorder|
#                           Bipolar I Disorder", no_dm_2$`Bipolar and major depression status | Instance 0`),]

# check overlap between self-reported bipolar and ICD-10 bipolar
#bipolar_returned <- as.numeric(NA)

#for (i in 1:nrow(control_excl)){
#  row <- control_excl$'Bipolar and major depression status | Instance 0'[i]
#  bipolar_returned[i] <- grepl("Bipolar II Disorder|Bipolar I Disorder", row)
#}

#bipolar_ret_cases = sum(bipolar_returned, na.rm=TRUE)

# load in clinical/medical history vars (GP data, V2/V3 read codes)
#clinical_vars <- readr::read_tsv("./normative mets/data_clinical_vars.tsv")

### Exclude based on BMI #
#control_no_ob  <-  no_dm_2[no_dm_2$BMI<30,]  #exclude people with BMI>30 (obese)
#control_no_uw <- control_no_ob[control_no_ob$BMI>18.5,]  #exclude people with BMI<18.5 (underweight)

### Exclude based on blood biochem #

#BIPOLAR GROUP: remove people with 'no bipolar or depression' (Smith/Pell criteria)
#bplr_group_2 <- bplr_group[!grepl("No Bipolar or Depression", bplr_group$'Bipolar and major depression status | Instance 0'),]

#BIPOLAR GROUP: remove people who do not have 
#bipolar_group <- bplr_group_2 %>%
#  filter(F30!='0'| F31!='0')



#for (i in 1:nrow(bplr_group)){
#  if bplr_group$'Bipolar and major depression status | Instance 0'[i] ==
