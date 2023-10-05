<<<<<<< HEAD
# script to load blood biochemistry data

#load libraries
library(readr)
library(tidyverse)

#load in tsv (edit for own path)
metabolomics_df <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_A.tsv")

#rename variables (segmented alphabeticaly for readability)

#### 0-A ####
metabolomics_df <- metabolomics_df %>% 
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
  )

#add comments to variable to link with biobank datafield id
comment(metabolomics_df$hydroxybut) <- c("Datafield = 23474-0.0")
comment(metabolomics_df$acetate) <- c("Datafield = 23475-0.0")
comment(metabolomics_df$acetoacetate) <- c("Datafield = 23476-0.0")
comment(metabolomics_df$acetone) <- c("Datafield = 23477-0.0")
comment(metabolomics_df$alanine) <- c("Datafield = 23460-0.0")
comment(metabolomics_df$albumin) <- c("Datafield = 23479-0.0")
comment(metabolomics_df$apolipo_A) <- c("Datafield = 23440-0.0")
comment(metabolomics_df$apolipo_B) <- c("Datafield = 23439-0.0")
comment(metabolomics_df$apolipo_AB) <- c("Datafield = 23441-0.0")
comment(metabolomics_df$avg_hdl) <- c("Datafield = 23433-0.0")
comment(metabolomics_df$avg_ldl) <- c("Datafield = 23432-0.0")
comment(metabolomics_df$avg_vldl) <- c("Datafield = 23431-0.0")


#### C ####
=======
# script to load blood biochemistry data

#load libraries
library(readr)
library(tidyverse)

#load in tsv (edit for own path)
metabolomics_df <- readr::read_tsv("C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\Metabolomics\\metabolomics_A.tsv")

#rename variables (segmented alphabeticaly for readability)

#### 0-A ####
metabolomics_df <- metabolomics_df %>% 
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
  )

#add comments to variable to link with biobank datafield id
comment(metabolomics_df$hydroxybut) <- c("Datafield = 23474-0.0")
comment(metabolomics_df$acetate) <- c("Datafield = 23475-0.0")
comment(metabolomics_df$acetoacetate) <- c("Datafield = 23476-0.0")
comment(metabolomics_df$acetone) <- c("Datafield = 23477-0.0")
comment(metabolomics_df$alanine) <- c("Datafield = 23460-0.0")
comment(metabolomics_df$albumin) <- c("Datafield = 23479-0.0")
comment(metabolomics_df$apolipo_A) <- c("Datafield = 23440-0.0")
comment(metabolomics_df$apolipo_B) <- c("Datafield = 23439-0.0")
comment(metabolomics_df$apolipo_AB) <- c("Datafield = 23441-0.0")
comment(metabolomics_df$avg_hdl) <- c("Datafield = 23433-0.0")
comment(metabolomics_df$avg_ldl) <- c("Datafield = 23432-0.0")
comment(metabolomics_df$avg_vldl) <- c("Datafield = 23431-0.0")


#### C ####
>>>>>>> 1894a05745a874e5e7085782aa36782299e80ab7
