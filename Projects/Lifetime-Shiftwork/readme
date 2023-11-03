# Introduction

Data are taken from UKB Category 130

Details of lifetime employment history collected at online follow-up. Participants were asked to enumerate their work history. Periods of 6 months or longer during which they were in paid full time employment for at least 15 hours each week have been labelled "jobs", with all other periods labelled "gaps".

Data are processed to give parameters used to compare the effects of shiftwork at different ages.  There are sometimes more than one job per year of age, and these could be shiftwork or not, and could be different occupations.  

https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=130


The R code is here [  ]
Two for loops iterate over each job for each eid to make a table that summarises job history for each person.  These tables were stored in a list (life_jobtable).  Because some people had two jobs per year, the data are expressed as hours per year.  No person had more than 39 jobs in their lifetime.

## 1.  Occupations             
Occupation coding of job type 4-digit SOC2000 coding UKB df22617.  This coding was used to create a factor variable.  Because some people had more than one job at each year, the final variable (per age bracket) table stores job occupation as a vector (            
              
Major groups used to make a factor variable (job_occupation)
               
Level   Label         Description
1       Managers      Managers and Senior Officials
2       Prof          Professional Occupations
3       Assoc_prof    Associate Professional and Technical Occupations
4       Admin         Administrative and Secretarial Occupations
5       Trades        Skilled Trades Occupations
6       Service       Personal Service Occupations
7       Sales         Sales and Customer Service Occupations
8       Machine       Process, Plant and Machine Operatives
9       Element       Elementary Occupations


## 2. Work Duration
Some people had more than one job per year, so not possible to assume a shiftwork job was totally shiftwork, although all jobs were more than 15 hours per week.  For this reason, the number of hours per job was calculated and used to express job duration as hours per year.
This was calculated using UKB df22605 (number of hours per week, numeric) but in the case of this being a missing value, hours per week was inputted from UKB df22604 (hours per week, categorical) assuming the midpoint of each category

Level  Code          Label
1      -1520	       15 to less-than-20 hours
2      -2030         20 to less-than-30 hours
3      -3040         30 to 40 hours
4      -4000         Over 40 hours
              
When the categorical value for work hours was also missing, the data for hours per week was inputted as 35 hours.

## Work Type
Work type was categorised as non-shiftwork, day shift, night shift and mix shift in a variable called "SW_type".  If this variable has a missing value, the person was taking a gap year from work or did not enter data.  Either way, a missing value is taken as not working.  This variable records whether a person did shiftwork during this job, but not the amount.  For example, someone might do shiftwork only for the first year of a ten year job.  The proportion of shiftwork was assessed using UKB df 22651.0, 22641.0, 22631.0.
