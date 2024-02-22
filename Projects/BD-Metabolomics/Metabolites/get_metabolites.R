# script to load blood biochemistry data

#load libraries
library(readr)
library(tidyverse)

#load in tsv (edit for own path)
blood_biochem_df <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\blood_biochemistry.tsv")
blood_datetime <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\blood_datetime_3166_participant.tsv")

#rename tsv variable columns
blood_biochem_df <- blood_biochem_df %>% 
  rename('alanine_at' = '30620-0.0',
         'albumin' = '30600-0.0',
         'alkaline_phos' = '30610-0.0',
         'apolipo_a' = '30630-0.0',
         'apolipo_b' = '30640-0.0',
         'aspartate_at' = '30650-0.0',
         'crp' = '30710-0.0',
         'calcium' = '30680-0.0',
         'cholesterol' = '30690-0.0',
         'creatinine' = '30700-0.0',
         'cystatin_c' = '30720-0.0',
         'dir_bilirubin' = '30660-0.0',
         'gamma_gmt' = '30730-0.0',
         'glucose' = '30740-0.0',
         'g_haem' = '30750-0.0',
         'hdl_chol' = '30760-0.0',
         'igf_1' = '30770-0.0',
         'ldl_direct' = '30780-0.0',
         'lipoprotein_a' = '30790-0.0',
         'oestradiol' = '30800-0.0',
         'phosphate' = '30810-0.0',
         'rheum_factor' = '30820-0.0',
         'shbg' = '30830-0.0',
         'testosterone' = '30850-0.0',
         'tot_bilirubin' = '30840-0.0',
         'tot_protein' = '30860-0.0',
         'triglycerides' = '30870-0.0',
         'urate' = '30880-0.0',
         'urea' = '30670-0.0',
         'vit_d' = '30890-0.0',
         )

#add comments to variable to link with biobank datafield id

comment(blood_biochem_df$alanine_at) <- c("Datafield = 30620-0.0")
comment(blood_biochem_df$albumin) <- c("Datafield = 30600-0.0")
comment(blood_biochem_df$alkaline_phos) <- c("Datafield = 30610-0.0")
comment(blood_biochem_df$apolipo_a) <- c("Datafield = 30630-0.0")
comment(blood_biochem_df$apolipo_b) <- c("Datafield = 30640-0.0")
comment(blood_biochem_df$aspartate_at) <- c("Datafield = 30650-0.0")
comment(blood_biochem_df$crp) <- c("Datafield = 30710-0.0")
comment(blood_biochem_df$calcium) <- c("Datafield = 30680-0.0")
comment(blood_biochem_df$cholesterol) <- c("Datafield = 30690-0.0")
comment(blood_biochem_df$creatinine) <- c("Datafield = 30700-0.0")
comment(blood_biochem_df$cystatin_c) <- c("Datafield = 30720-0.0")
comment(blood_biochem_df$dir_bilirubin) <- c("Datafield = 30660-0.0")
comment(blood_biochem_df$gamma_gmt) <- c("Datafield = 30730-0.0")
comment(blood_biochem_df$glucose) <- c("Datafield = 30740-0.0")
comment(blood_biochem_df$g_haem) <- c("Datafield = 30750-0.0")
comment(blood_biochem_df$hdl_chol) <- c("Datafield = 30760-0.0")
comment(blood_biochem_df$igf_1) <- c("Datafield = 30770-0.0")
comment(blood_biochem_df$ldl_direct) <- c("Datafield = 30780-0.0")
comment(blood_biochem_df$lipoprotein_a) <- c("Datafield = 30790-0.0")
comment(blood_biochem_df$oestradiol) <- c("Datafield = 30800-0.0")
comment(blood_biochem_df$phosphate) <- c("Datafield = 30810-0.0")
comment(blood_biochem_df$rheum_factor) <- c("Datafield = 30820-0.0")
comment(blood_biochem_df$shbg) <- c("Datafield = 30830-0.0")
comment(blood_biochem_df$testosterone) <- c("Datafield = 30850-0.0")
comment(blood_biochem_df$tot_bilirubin) <- c("Datafield = 30840-0.0")
comment(blood_biochem_df$tot_protein) <- c("Datafield = 30860-0.0")
comment(blood_biochem_df$triglycerides) <- c("Datafield = 30870-0.0")
comment(blood_biochem_df$urate) <- c("Datafield = 30880-0.0")
comment(blood_biochem_df$urea) <- c("Datafield = 30670-0.0")
comment(blood_biochem_df$vit_d) <- c("Datafield = 30890-0.0")


# exclude markers that are not of interest to us
metabolites <- blood_biochem_df[c("eid", "alanine_at","albumin","apolipo_a", "apolipo_b", "crp", "cholesterol",
                  "creatinine", "cystatin_c", "dir_bilirubin", "gamma_gmt", "glucose",
                  "g_haem", "hdl_chol", "ldl_direct", "lipoprotein_a", "phosphate", "rheum_factor",
                  "tot_bilirubin", "triglycerides", "vit_d")]


# merge with existing bdmet_df
bdmet_blood <- merge(bdmet_df,metabolites, by="eid")
bdmet_blood_dt <- merge(bdmet_blood,blood_datetime, by="eid")

#save as new dataframe
analysis_df <- bdmet_blood_dt
save(analysis_df,file="analysis_df.Rda")


