#need to exclude cases after online work study?

#need to exclude data after diagnosis of MS
data$Measurement_Date <= diagnosis_date #rows of data collected after diagnosis are disregarded

#this is the model
model <- glmer(MS_Risk ~ work_type + age_of_exposure + dose_of_night_shift + (1|Individual_ID), 
               data = data, 
               family = binomial(link = "logit"))

#Table one stratified by MS and night, day and mix and non-shift
#table two regression table SW and MS risk.