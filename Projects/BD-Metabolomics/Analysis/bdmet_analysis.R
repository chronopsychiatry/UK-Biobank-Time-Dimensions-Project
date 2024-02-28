# script to analyse biobank data for metabolomic seasonality trend

#load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(season)
library(ggplot2)

# load participant dataframe for analysis
load(file="C:\\Users\\arogusk2\\OneDrive - University of Edinburgh\\HELIOS-BD\\Side Projects\\BioBank Project\\BD-Metabolomics data\\analysis_df.Rda")

# calculate mean value for each time point
alanine_df <- analysis_df %>%
  group_by(assess_month, Group) %>%
  summarise(mean_alanine = mean(alanine, na.rm=TRUE), SD_alanine = sd(alanine, na.rm=TRUE), N = length(alanine))

# first plot data --> choose one metabolite to get started with. VitD!
ggplot(acetate_df, aes(x=assess_month, y=mean_acetate, color=Group)) + 
  geom_point()

don <- xts(x = analysis_df$vit_d, order.by = analysis_df$assess_date)

ggplot(analysis_df, aes(x = acetate, y = assess_month, colour=Group)) +
  xlab("Date") +
  ylab("acetate level") +
  stat_smooth(method = 'nls', formula = 'y~a*exp(b*x)',
              method.args = list(start=c(a=0.1646, b=9.5e-8)), se=FALSE) +
  stat_smooth(color = 1, method = 'nls', formula = 'y~a*exp(b*x)',
              method.args = list(start=c(a=0.1646, b=9.5e-8)), se=FALSE) #+
  #geom_point(size=4, pch=21,color = "black", stroke=1.5, aes(fill=Group))

analysis_df %>%
  ggplot( aes(x=assess_month, y=acetate, group=Group, color=Group)) +
  geom_line()

#plot
p <- ggplot(analysis_df, aes(x=assess_month, y=alanine, colour=Group)) +
#  geom_point() +
  geom_smooth()+
  scale_x_continuous(labels=as.character(analysis_df$assess_month),breaks=analysis_df$assess_month)

p + scale_x_continuous(labels=c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9"="September",
                              "10"= "October", "11"="November", "12"="December"))

  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("assess_month", labels = as.character(analysis_df$assess_month), breaks = analysis_df$assess_month)

p + scale_x_discrete(labels=c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9"="September",
                              "10"= "October", "11"="November", "12"="December"))

#plot
ggplot(acetate_df, aes(x=assess_month, y=mean_acetate, colour=Group)) +
  geom_point() +
  geom_smooth()


ggplot(subset(analysis_df,Group %in% c("bipolar"))) + 
         geom_boxplot(aes(x=assess_month, y=alanine))
