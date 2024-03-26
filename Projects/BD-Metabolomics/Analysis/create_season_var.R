#load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(ExcelFunctionsR)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\Final study participant dataframe\\analysis_df.Rda")

# create new season df
season_df <- data.frame(analysis_df$eid)
season_df <- season_df %>% 
  rename('eid' = 'analysis_df.eid')
season_df$bs_month <- analysis_df$bs_month
season_df$season <- as.numeric(NA)

#remove NAs
season_df<- season_df %>%  filter(!is.na(bs_month))

# pull out month and assign season
for (i in 1:nrow(season_df)){
  if (season_df$bs_month[i] == "3" | season_df$bs_month[i] =="4" | season_df$bs_month[i] == "5") {
    season_df$season[i]= '1'
  } else if (season_df$bs_month[i] == "6" |season_df$bs_month[i] =="7" |season_df$bs_month[i] == "8") {
    season_df$season[i]= '2'
  } else if (season_df$bs_month[i] == "9" |season_df$bs_month[i] =="10" |season_df$bs_month[i] == "11") {
    season_df$season[i]= '3'
  } else if (season_df$bs_month[i] == "12" |season_df$bs_month[i] =="1" |season_df$bs_month[i] == "2") {
    season_df$season[i]= '4'
  }}
 
#rename column as bs_season
season_df <- season_df %>% 
  rename('bs_season' = 'season')

# merge season_df with analysis_df
analysis_df <- merge(analysis_df,season_df, by = c("eid", "bs_month"), all.x = TRUE)
save(analysis_df,file="analysis_df.Rda")
