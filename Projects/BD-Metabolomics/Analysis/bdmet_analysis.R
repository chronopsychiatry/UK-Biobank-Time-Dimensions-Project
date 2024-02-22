# script to analyse biobank data for metabolomic seasonality trend

#load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(season)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\analysis_df.Rda")

# 