# script to load blood biochemistry data
# output file: analysis_df

#load libraries
library(readr)
library(tidyverse)

# set cwd
setwd("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data")

# load in existing participant df
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\bdmet_df.Rda")

#extract blood biochemistry variables

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

bdmet_blood_dt <- bdmet_blood_dt %>% 
  rename('blood sample datetime' = '3166-0.0')

#save as new dataframe
analysis_df <- bdmet_blood_dt
save(analysis_df,file="analysis_df.Rda")


#extract NMR variables
#load libraries
library(readr)
library(tidyverse)
library(dplyr)

#load in tsv (edit for own path) - all are instance 0
metabolomics_df_A <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_A.tsv")
metabolomics_df_C <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_C.tsv")
metabolomics_df_D_F <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_D-F.tsv")
metabolomics_df_G_O <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_G-O.tsv")
metabolomics_df_P <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_P.tsv")
metabolomics_df_R_V <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_R-V.tsv")

#merge into one dataframe
metdf_1 <- merge(metabolomics_df_A,metabolomics_df_C, by="eid")
metdf_2 <- merge(metdf_1,metabolomics_df_D_F, by="eid")
metdf_3 <- merge(metdf_2,metabolomics_df_G_O, by="eid")
metdf_4 <- merge(metdf_3,metabolomics_df_P, by="eid")
metdf_5 <- merge(metdf_4,metabolomics_df_R_V, by="eid")

#rename variables (currently only renaming variables of interest (i.e. not every kind fo cholesterol!))

#### 0-A ####
metdf_names <- metdf_5 %>% 
  rename('hydroxybut' = '23474-0.0',
         'acetate' = '23475-0.0',
         'acetoacetate' = '23476-0.0',
         'acetone' = '23477-0.0',
         'alanine' = '23460-0.0',
         'albumin' = '23479-0.0',
         'apolipo_A' = '23440-0.0',
         'apolipo_B' = '23439-0.0',
         'apolipo_AB' = '23441-0.0',
         'avg_hdl' = '23433-0.0',
         'avg_ldl' = '23432-0.0',
         'avg_vldl' = '23431-0.0',
         'citrate' = '23473-0.0',
         'creatinine'= '23478-0.0',
         'docosahex_acid' = '23450-0.0',
         #'glucose' = '23470-0.0', (already in blood biochem)
         #'glucose-lactate' = '20280-0.0',
         'glutamine' = '23461-0.0',
         'glycine' = '23462-0.0',
         'glycoprotein_acetyls' = '23480-0.0',
         'histidine' = '23463-0.0',
         'isoleucine' = '23465-0.0',
         'lactate' = '23471-0.0',
         'leucine' = '23466-0.0',
         'linoleic_acid' = '23449-0.0',
         'monounsat_fatty_acid' = '23447-0.0',
         'omega_3' = '23444-0.0',
         'omega_6' = '23445-0.0',
         'phenylalanine' = '23468-0.0',
         'phos_t_cholines' = '23437-0.0',
         'phosphoglycerides' = '23434-0.0',
         'polyunsat_fatty_acid' = '23446-0.0',
         'pyruvate' = '23472-0.0',
         'sat_fatty_acid' = '23448-0.0',
         #'spect_corr_alanine' = '20281-0.0',
         'sphyngomyelins' = '23438-0.0',
         'tot_chol' = '23400-0.0',
         'tot_cholines' = '23436-0.0',
         'tot_branchedchain_aa' = '23464-0.0',
         'tot_lipoprotein' = '23427-0.0',
         'tot_fatty_acid' = '23442-0.0',
         'tot_triglycerides' = '23407-0.0',
         'tyrosine' = '23469-0.0',
         #'valine' - '23467-0.0'
  )

#reduce to a df with only the above named vars
met_reduce_df <- metdf_names[c("eid", "hydroxybut","acetate","acetoacetate","acetone", "alanine","albumin",
                               "apolipo_A","apolipo_B","apolipo_AB","avg_hdl","avg_ldl","avg_vldl","citrate",
                               "creatinine","docosahex_acid","glutamine","glycine","glycoprotein_acetyls",
                               "histidine", "isoleucine","lactate","leucine","linoleic_acid","monounsat_fatty_acid",
                               "omega_3","omega_6","phenylalanine","phos_t_cholines","phosphoglycerides",
                               "polyunsat_fatty_acid","pyruvate", "sat_fatty_acid","sphyngomyelins",
                               "tot_chol","tot_cholines", "tot_branchedchain_aa","tot_lipoprotein",
                               "tot_fatty_acid","tot_triglycerides","tyrosine")]

#merge with analysis_df
analysis_met <- merge(analysis_df,met_reduce_df, by="eid")

#save
save(analysis_met,file="analysis_met.Rda")
