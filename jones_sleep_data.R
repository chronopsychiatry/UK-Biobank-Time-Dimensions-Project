## Jones sleep returned data  #############################################

jones_sleepdata <- read.csv("./jones_sleepdata.csv")
ukb99491bridge9072 <- read.table("./ukb99491bridge9072.txt", quote="\"", comment.char="")

#rename the bridging file var
names(ukb99491bridge9072) <- c("eid","n_eid")

#merge jones data with bridging file to get right eid 
jones_sleepdata_recode <- merge(jones_sleepdata,ukb99491bridge9072, by="n_eid")

#delete bridging eid
jones_sleepdata_recode$n_eid <- NULL

#recode season to factor
jones_sleepdata_recode$acc_season_worn <- factor(jones_sleepdata_recode$acc_season_worn)

UKB_master <- merge(UKB_master,jones_sleepdata_recode, by="eid")
