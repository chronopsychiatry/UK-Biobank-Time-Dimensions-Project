# script to load metabolomics data

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