########################################################################################################################
#INSTALL LIBRARY PACKAGES
library(ggplot2)
library(summarytools)
library(corrplot)
library(tidyverse)
library(lubridate)
library(questionr)
library(FactoMineR)
library(factoextra)
library(survey)
library(forcats)
library(finalfit)
library(ggExtra)
library(ggpubr)
library(viridis)
library(jtools)
library(survey)
library(tidyr)
library(dplyr)
library(foreign)
install.packages("gnm")
no
library(gnm)
install.packages("devtools")
no
devtools::install_github("hturner/gnm")
library(devtools)
library(tsModel)

.libPaths()
lapply(.libPaths(), dir)
options(scipen = 999)

#LOAD FILES TO ENV
setwd("/Volumes/hallahk/Analysis/Data")
ont.temp.data <- readRDS("ontario_Daymet_weather_byFSA_2005_2015.RDS")
ont.admissions.data <- readRDS("ontario_admissions_byFSA_2005_2015_update_age_sex_UPDATE_v2.RDS") 
ont.ed.data <- readRDS("EDvisits_byFSA_Jan2005_Dec2015_ON_Hallah_UPDATE_v2.RDS")

########################################################################################################################
#BASIC DATA EXPLORATION
# ##Ontario temperature data
# names(ont.temp.data)
# names(ont.admissions.data)
# str(ont.temp.data)

ont.temp.data$Tmax_avg<-ont.temp.data$tmax
ont.temp.data$Tmin_avg<-ont.temp.data$tmin
ont.temp.data$Year<-ont.temp.data$year
# 
# summary(ont.temp.data$Tmax_avg) 
# str(ont.temp.data$Tmax_avg)
# descr(ont.temp.data$Tmax_avg) 
# hist(ont.temp.data$Tmax_avg) 
# 
# summary(ont.temp.data$Tmin_avg) 
# str(ont.temp.data$Tmin_avg)
# descr(ont.temp.data$Tmin_avg) 
# hist(ont.temp.data$Tmin_avg) 

#No PCP measure in Daymet data
#summary(ont.temp.data$PCP_avg) 
#str(ont.temp.data$PCP_avg)
#descr(ont.temp.data$PCP_avg) 
#hist(ont.temp.data$PCP_avg) 

##Hospital admissison data
# names(ont.admissions.data)
# str(ont.admissions.data)

########################################################################################################################
########################################################################################################################
########################################################################################################################
#CREATING HELPFUL VARIABLES

#Reading the date columns as year-month-day dates
ont.temp.data$date<-ymd(ont.temp.data$date)
ont.admissions.data$date<-ymd(ont.admissions.data$date)
ont.ed.data$date<-ymd(ont.ed.data$date)
#ont.admissions.data$date %>% separate(date, sep="-", into = c("year", "month", "day"))

#Creating variable for population density (ie. rural vs urban)
ont.admissions.data$density.a <- ifelse(grepl("0",ont.admissions.data$PATIENT_POSTAL_CODE_TRUNCATED),'rural','urban')
ont.ed.data$density.e <- ifelse(grepl("0",ont.ed.data$PATIENT_POSTAL_CODE_TRUNCATED),'rural','urban')
ont.temp.data$density.t <- ifelse(grepl("0",ont.temp.data$fsa),'rural','urban')

ont.admissions.data$fsa<-ont.admissions.data$PATIENT_POSTAL_CODE_TRUNCATED
ont.ed.data$fsa<-ont.ed.data$PATIENT_POSTAL_CODE_TRUNCATED


#Merging dataset 
df.ad<- merge(ont.admissions.data,ont.temp.data,by=c("date","fsa"), all.x = T)
df.ed<- merge(ont.ed.data,ont.temp.data,by=c("date","fsa"), all.x = T)
#Checking how many were lost by merging
nrow(ont.admissions.data)
nrow(df.ad)
summary(unique(ont.admissions.data$fsa))
summary(unique(df.ad$fsa))


nrow(ont.ed.data)
nrow(df.ed)
summary(unique(ont.ed.data$fsa))
summary(unique(df.ed$fsa))

#checking to see any chnages in FSA's included in data. result: 518 in each year
# FSA05 <- subset(df.ad, year == "2005")
# length(unique(FSA05$fsa))
# 
# FSA06 <- subset(df.ad, year == "2006")
# length(unique(FSA06$fsa))
# 
# FSA07 <- subset(df.ad, year == "2007")
# length(unique(FSA07$fsa))
# 
# FSA08 <- subset(df.ad, year == "2008")
# length(unique(FSA08$fsa))
# 
# FSA09 <- subset(df.ad, year == "2009")
# length(unique(FSA09$fsa))
# 
# FSA10 <- subset(df.ad, year == "2010")
# length(unique(FSA10$fsa))
# 
# FSA11 <- subset(df.ad, year == "2011")
# length(unique(FSA11$fsa))
# 
# FSA12 <- subset(df.ad, year == "2012")
# length(unique(FSA12$fsa))
# 
# FSA13 <- subset(df.ad, year == "2013")
# length(unique(FSA13$fsa))
# 
# FSA14 <- subset(df.ad, year == "2014")
# length(unique(FSA14$fsa))
# 
# FSA15 <- subset(df.ad, year == "2015")
# length(unique(FSA15$fsa))

#Creating variables for Hospital Admissions dataframe
df.ad$GeoArea <- ifelse(df.ad$fsa %in% c("K","L","M","P0G","P0A","P1H","P1L","P1P","P0E","P0B","P0C","P2A"), 'Southern',
                        ifelse(df.ad$fsa %in% c("P0V","P0X","P9N","P8T","P8N","P0W","P0Y","P9A","P0T","P0L","P5N","P0S","P0M",
                                                "P3N","P3P","P3Y","P5E","P0R","P5A","P0P","P0H","P0J","P3L","P2N","P0K","P0N"), 'Northern', 'Southwestern'))
# summary(df.ad$GeoArea)
# head(df.ad$GeoArea)
# tail(df.ad$GeoArea)

df.ad$ClimateArea <- ifelse(df.ad$fsa %in% c("P0V","P0T","P5N"), 'Dfc',
                            ifelse(df.ad$fsa %in% c("N0R","N9V","N9Y","N8H","N0P","N8A","N0N"), 'Dfa', 'Dfb'))
# summary(df.ad$ClimateArea)
# head(df.ad$ClimateArea)
# tail(df.ad$ClimateArea)


df.ad$MaxWarning <- ifelse(grepl("Northern",df.ad$GeoArea),'29','31') #warning when max temp is 29+ in northern, or when 31+ elsewhere
# head(df.ad$MaxWarning)
# tail(df.ad$MaxWarning)


df.ad$MinWarning <- ifelse(grepl("Northern",df.ad$GeoArea),'18',
                           ifelse(grepl("Southern",df.ad$GeoArea),'20','21')) #warning when min temp is 18+ in northern, 20+ in south, or 21+ in wouthwest
# head(df.ad$MinWarning)
# tail(df.ad$MinWarning)


#Creating variables for Emergency Department dataframe
df.ed$GeoArea <- ifelse(df.ed$fsa %in% c("K","L","M","P0G","P0A","P1H","P1L","P1P","P0E","P0B","P0C","P2A"), 'Southern',
                        ifelse(df.ed$fsa %in% c("P0V","P0X","P9N","P8T","P8N","P0W","P0Y","P9A","P0T","P0L","P5N","P0S","P0M",
                                                "P3N","P3P","P3Y","P5E","P0R","P5A","P0P","P0H","P0J","P3L","P2N","P0K","P0N"), 'Northern', 'Southwestern'))
# summary(df.ed$GeoArea)
# head(df.ed$GeoArea)
# tail(df.ed$GeoArea)

df.ed$ClimateArea <- ifelse(df.ed$fsa %in% c("P0V","P0T","P5N"), 'Dfc',
                            ifelse(df.ed$fsa %in% c("N0R","N9V","N9Y","N8H","N0P","N8A","N0N"), 'Dfa', 'Dfb'))
# summary(df.ed$ClimateArea)
# head(df.ed$ClimateArea)
# tail(df.ed$ClimateArea)


df.ed$MaxWarning <- ifelse(grepl("Northern",df.ed$GeoArea),'29','31') #warning when max temp is 29+ in northern, or when 31+ elsewhere
# head(df.ed$MaxWarning)
# tail(df.ed$MaxWarning)


df.ed$MinWarning <- ifelse(grepl("Northern",df.ed$GeoArea),'18',
                           ifelse(grepl("Southern",df.ed$GeoArea),'20','21')) #warning when min temp is 18+ in northern, 20+ in south, or 21+ in wouthwest
# head(df.ed$MinWarning)
# tail(df.ed$MinWarning)


#IDENTIFYING EHE's
df.ad$EHE<-ifelse(df.ad$Tmax_avg >= df.ad$MaxWarning,1,0)
df.ad$EHE2<-ifelse(df.ad$Tmax_avg >= df.ad$MaxWarning & df.ad$Tmin_avg >= df.ad$MinWarning,1,0)

df.ed$EHE<-ifelse(df.ed$Tmax_avg >= df.ed$MaxWarning,1,0)
df.ed$EHE2<-ifelse(df.ed$Tmax_avg >= df.ed$MaxWarning & df.ed$Tmin_avg >= df.ed$MinWarning,1,0)


#RESTRICTING DATASET TO WARM SEASON (MAY TO SEP)
df.ad$Month <- month(df.ad$date)
df.ad$Season <- ifelse(df.ad$Month %in% c(10, 11, 12, 1, 2, 3, 4),'Cold','Warm') 

df.ed$Month <- month(df.ed$date)
df.ed$Season <- ifelse(df.ed$Month %in% c(10, 11, 12, 1, 2, 3, 4),'Cold','Warm') 


#Creating variable for total admissions by rural and urban
df.ad$Warm <- ifelse(df.ad$Season %in% c("Warm"), 1, 0)
df.ed$Warm <- ifelse(df.ed$Season %in% c("Warm"), 1, 0)

#sex per age


########################################################################################################################
########################################################################################################################
########################################################################################################################
#TABLE 1 HOSPITAL VARIABLE VALUES

########################################################################################################################
#Whole period hospital admissions 

#For age and sex cell values
#?colSums
colSums(df.ad[,c(4:93)])#sum of all-cause hospital admissions over the whole period by all cause, all cause 0-4, all cause 5-12, all cause 13-18

#Creating rural df and getting cell values
df.ad.rural <- df.ad%>%filter(grepl("0",df.ad$fsa))
summary(unique(df.ad.rural$fsa))
head(df.ad.rural$fsa)
tail(df.ad.rural$fsa)
colSums(df.ad.rural[,c(4:8, 14:23)])
colSums(df.ad.rural[,c(14:23)])

#Creating urban df and getting cell values
df.ad.urban <- subset(df.ad, density.a == "urban")
summary(unique(df.ad.urban$fsa))
head(df.ad.urban$fsa)
tail(df.ad.urban$fsa)
colSums(df.ad.urban[,c(4:8, 14:23)])
colSums(df.ad.urban[,c(14:23)])

#Column totals
sum(df.ad$allcause_age_0_4, df.ad$respiratory_age_0_4, df.ad$asthma_age_0_4, df.ad$injury_age_0_4, 
    df.ad$drowning_age_0_4, df.ad$falls_age_0_4, df.ad$transport_accidents_age_0_4,
    df.ad$heat_age_0_4, df.ad$heatstroke_age_0_4, df.ad$dehydration_electrolyte_age_0_4, 
    df.ad$renal_age_0_4, df.ad$infectious_parasitic_age_0_4, df.ad$otitis_age_0_4, 
    df.ad$lower_respiratory_age_0_4, df.ad$bacterial_enteritis_age_0_4)

sum(df.ad$allcause_age_5_12, df.ad$respiratory_age_5_12, df.ad$asthma_age_5_12, df.ad$injury_age_5_12, 
    df.ad$drowning_age_5_12, df.ad$falls_age_5_12, df.ad$transport_accidents_age_5_12,
    df.ad$heat_age_5_12, df.ad$heatstroke_age_5_12, df.ad$dehydration_electrolyte_age_5_12, 
    df.ad$renal_age_5_12, df.ad$infectious_parasitic_age_5_12, df.ad$otitis_age_5_12, 
    df.ad$lower_respiratory_age_5_12, df.ad$bacterial_enteritis_age_5_12)

sum(df.ad$allcause_age_13_18, df.ad$respiratory_age_13_18, df.ad$asthma_age_13_18, df.ad$injury_age_13_18, 
    df.ad$drowning_age_13_18, df.ad$falls_age_13_18, df.ad$transport_accidents_age_13_18,
    df.ad$heat_age_13_18, df.ad$heatstroke_age_13_18, df.ad$dehydration_electrolyte_age_13_18, 
    df.ad$renal_age_13_18, df.ad$infectious_parasitic_age_13_18, df.ad$otitis_age_13_18, 
    df.ad$lower_respiratory_age_13_18, df.ad$bacterial_enteritis_age_13_18)

sum(df.ad$allcause_female, df.ad$respiratory_female, df.ad$asthma_female, df.ad$injury_female, 
    df.ad$drowning_female, df.ad$falls_female, df.ad$transport_accidents_female,
    df.ad$heat_female, df.ad$heatstroke_female, df.ad$dehydration_electrolyte_female, 
    df.ad$renal_female, df.ad$infectious_parasitic_female, df.ad$otitis_female, 
    df.ad$lower_respiratory_female, df.ad$bacterial_enteritis_female)

sum(df.ad$allcause_male, df.ad$respiratory_male, df.ad$asthma_male, df.ad$injury_male, 
    df.ad$drowning_male, df.ad$falls_male, df.ad$transport_accidents_male,
    df.ad$heat_male, df.ad$heatstroke_male, df.ad$dehydration_electrolyte_male, 
    df.ad$renal_male, df.ad$infectious_parasitic_male, df.ad$otitis_male, 
    df.ad$lower_respiratory_male, df.ad$bacterial_enteritis_male)

sum(df.ad.rural$allcause, df.ad.rural$respiratory, df.ad.rural$asthma, df.ad.rural$injury, 
    df.ad.rural$drowning, df.ad.rural$falls, df.ad.rural$transport_accidents,
    df.ad.rural$heat, df.ad.rural$heatstroke, df.ad.rural$dehydration_electrolyte, 
    df.ad.rural$renal, df.ad.rural$infectious_parasitic, df.ad.rural$otitis, 
    df.ad.rural$lower_respiratory, df.ad.rural$bacterial_enteritis)

sum(df.ad.urban$allcause, df.ad.urban$respiratory, df.ad.urban$asthma, df.ad.urban$injury, 
    df.ad.urban$drowning, df.ad.urban$falls, df.ad.urban$transport_accidents,
    df.ad.urban$heat, df.ad.urban$heatstroke, df.ad.urban$dehydration_electrolyte, 
    df.ad.urban$renal, df.ad.urban$infectious_parasitic, df.ad.urban$otitis, 
    df.ad.urban$lower_respiratory, df.ad.urban$bacterial_enteritis)

#Failed attemps:
#df$Rural <- ifelse(df$density %in% c("rural"), 1, 0) # didn't work
#df$Urban <- ifelse(df$density %in% c("urban"), 1, 0) # didn't work

#df_ruralx <- subset(df, density.x == "rural")
#head(df_ruralx$fsa)
#tail(df_ruralx$fsa)

#df_ruraly <- subset(df, density.y == "rural")
#head(df_ruralx$fsa)
#tail(df_ruralx$fsa)

#colSums(df_ruralx[,c(4:33)])
#colSums(df_ruraly[,c(4:33)])


########################################################################################################################
#Warm months hospital admissions 

#WARM MONTHS
df.ad.warm <- df.ad%>%filter(grepl("1",df.ad$Warm))
tail(df.ad$Warm)
colSums(df.ad.warm[,c(4:93)])
nrow(df.ad.warm)


df.ad.warm.rural <- df.ad.rural%>%filter(grepl("1",df.ad.rural$Warm))
colSums(df.ad.warm.rural[,c(4:8, 14:23)])
colSums(df.ad.warm.rural[,c(14:23)])
df.ad.warm.urban <- df.ad.urban%>%filter(grepl("1",df.ad.urban$Warm))
colSums(df.ad.warm.urban[,c(4:8, 14:23)])
colSums(df.ad.warm.urban[,c(14:23)])


#Column totals
sum(df.ad.warm$allcause_age_0_4, df.ad.warm$respiratory_age_0_4, df.ad.warm$asthma_age_0_4, df.ad.warm$injury_age_0_4, 
    df.ad.warm$drowning_age_0_4, df.ad.warm$falls_age_0_4, df.ad.warm$transport_accidents_age_0_4,
    df.ad.warm$heat_age_0_4, df.ad.warm$heatstroke_age_0_4, df.ad.warm$dehydration_electrolyte_age_0_4, 
    df.ad.warm$renal_age_0_4, df.ad.warm$infectious_parasitic_age_0_4, df.ad.warm$otitis_age_0_4, 
    df.ad.warm$lower_respiratory_age_0_4, df.ad.warm$bacterial_enteritis_age_0_4)

sum(df.ad.warm$allcause_age_5_12, df.ad.warm$respiratory_age_5_12, df.ad.warm$asthma_age_5_12, df.ad.warm$injury_age_5_12, 
    df.ad.warm$drowning_age_5_12, df.ad.warm$falls_age_5_12, df.ad.warm$transport_accidents_age_5_12,
    df.ad.warm$heat_age_5_12, df.ad.warm$heatstroke_age_5_12, df.ad.warm$dehydration_electrolyte_age_5_12, 
    df.ad.warm$renal_age_5_12, df.ad.warm$infectious_parasitic_age_5_12, df.ad.warm$otitis_age_5_12, 
    df.ad.warm$lower_respiratory_age_5_12, df.ad.warm$bacterial_enteritis_age_5_12)

sum(df.ad.warm$allcause_age_13_18, df.ad.warm$respiratory_age_13_18, df.ad.warm$asthma_age_13_18, df.ad.warm$injury_age_13_18, 
    df.ad.warm$drowning_age_13_18, df.ad.warm$falls_age_13_18, df.ad.warm$transport_accidents_age_13_18,
    df.ad.warm$heat_age_13_18, df.ad.warm$heatstroke_age_13_18, df.ad.warm$dehydration_electrolyte_age_13_18, 
    df.ad.warm$renal_age_13_18, df.ad.warm$infectious_parasitic_age_13_18, df.ad.warm$otitis_age_13_18, 
    df.ad.warm$lower_respiratory_age_13_18, df.ad.warm$bacterial_enteritis_age_13_18)

sum(df.ad.warm$allcause_female, df.ad.warm$respiratory_female, df.ad.warm$asthma_female, df.ad.warm$injury_female, 
    df.ad.warm$drowning_female, df.ad.warm$falls_female, df.ad.warm$transport_accidents_female,
    df.ad.warm$heat_female, df.ad.warm$heatstroke_female, df.ad.warm$dehydration_electrolyte_female, 
    df.ad.warm$renal_female, df.ad.warm$infectious_parasitic_female, df.ad.warm$otitis_female, 
    df.ad.warm$lower_respiratory_female, df.ad.warm$bacterial_enteritis_female)

sum(df.ad.warm$allcause_male, df.ad.warm$respiratory_male, df.ad.warm$asthma_male, df.ad.warm$injury_male, 
    df.ad.warm$drowning_male, df.ad.warm$falls_male, df.ad.warm$transport_accidents_male,
    df.ad.warm$heat_male, df.ad.warm$heatstroke_male, df.ad.warm$dehydration_electrolyte_male, 
    df.ad.warm$renal_male, df.ad.warm$infectious_parasitic_male, df.ad.warm$otitis_male, 
    df.ad.warm$lower_respiratory_male, df.ad.warm$bacterial_enteritis_male)

sum(df.ad.warm.rural$allcause, df.ad.warm.rural$respiratory, df.ad.warm.rural$asthma, df.ad.warm.rural$injury, 
    df.ad.warm.rural$drowning, df.ad.warm.rural$falls, df.ad.warm.rural$transport_accidents,
    df.ad.warm.rural$heat, df.ad.warm.rural$heatstroke, df.ad.warm.rural$dehydration_electrolyte, 
    df.ad.warm.rural$renal, df.ad.warm.rural$infectious_parasitic, df.ad.warm.rural$otitis, 
    df.ad.warm.rural$lower_respiratory, df.ad.warm.rural$bacterial_enteritis)

sum(df.ad.warm.urban$allcause, df.ad.warm.urban$respiratory, df.ad.warm.urban$asthma, df.ad.warm.urban$injury, 
    df.ad.warm.urban$drowning, df.ad.warm.urban$falls, df.ad.warm.urban$transport_accidents,
    df.ad.warm.urban$heat, df.ad.warm.urban$heatstroke, df.ad.warm.urban$dehydration_electrolyte, 
    df.ad.warm.urban$renal, df.ad.warm.urban$infectious_parasitic, df.ad.warm.urban$otitis, 
    df.ad.warm.urban$lower_respiratory, df.ad.warm.urban$bacterial_enteritis)



########################################################################################################################
########################################################################################################################
########################################################################################################################
#TABLE 1 EMERGENCY DEPARTMENT VARIABLE VALUES

########################################################################################################################
#Whole period emergency department visits
sum(df.ed$allcause, na.rm = T)
sum(df.ed$allcause_age_0_4)
colnames(df.ed)

#For age and sex cell values
?colSums
colSums(df.ed[,c(4:93)])#sum of all-cause ED admissions over the whole period by all cause, all cause 0-4, all cause 5-12, all cause 13-18

#For rural/urban cell values
df.ed.rural <- df.ed%>%filter(grepl("0",df.ed$fsa))
head(df.ed.rural$fsa)
tail(df.ed.rural$fsa)
colSums(df.ed.rural[,c(4:8)])
colSums(df.ed.rural[,c(9:18)])

#df.urban <- df%>%filter(grepl("1:9",df$fsa))

df.ed.urban <- subset(df.ed, density.e == "urban")
head(df.ed.urban$fsa)
tail(df.ed.urban$fsa)
colSums(df.ed.urban[,c(4:8)])
colSums(df.ed.urban[,c(9:18)])


#Column totals
sum(df.ed$age_0_4, df.ed$respiratory_age_0_4, df.ed$asthma_age_0_4, df.ed$injury_age_0_4, 
    df.ed$drowning_age_0_4, df.ed$falls_age_0_4, df.ed$transport_accidents_age_0_4,
    df.ed$heat_age_0_4, df.ed$heatstroke_age_0_4, df.ed$dehydration_electrolyte_age_0_4, 
    df.ed$renal_age_0_4, df.ed$infectious_parasitic_age_0_4, df.ed$otitis_age_0_4, 
    df.ed$lower_respiratory_age_0_4, df.ed$bacterial_enteritis_age_0_4)

sum(df.ed$age_5_12, df.ed$respiratory_age_5_12, df.ed$asthma_age_5_12, df.ed$injury_age_5_12, 
    df.ed$drowning_age_5_12, df.ed$falls_age_5_12, df.ed$transport_accidents_age_5_12,
    df.ed$heat_age_5_12, df.ed$heatstroke_age_5_12, df.ed$dehydration_electrolyte_age_5_12, 
    df.ed$renal_age_5_12, df.ed$infectious_parasitic_age_5_12, df.ed$otitis_age_5_12, 
    df.ed$lower_respiratory_age_5_12, df.ed$bacterial_enteritis_age_5_12)

sum(df.ed$age_13_18, df.ed$respiratory_age_13_18, df.ed$asthma_age_13_18, df.ed$injury_age_13_18, 
    df.ed$drowning_age_13_18, df.ed$falls_age_13_18, df.ed$transport_accidents_age_13_18,
    df.ed$heat_age_13_18, df.ed$heatstroke_age_13_18, df.ed$dehydration_electrolyte_age_13_18, 
    df.ed$renal_age_13_18, df.ed$infectious_parasitic_age_13_18, df.ed$otitis_age_13_18, 
    df.ed$lower_respiratory_age_13_18, df.ed$bacterial_enteritis_age_13_18)

sum(df.ed$female, df.ed$respiratory_female, df.ed$asthma_female, df.ed$injury_female, 
    df.ed$drowning_female, df.ed$falls_female, df.ed$transport_accidents_female,
    df.ed$heat_female, df.ed$heatstroke_female, df.ed$dehydration_electrolyte_female, 
    df.ed$renal_female, df.ed$infectious_parasitic_female, df.ed$otitis_female, 
    df.ed$lower_respiratory_female, df.ed$bacterial_enteritis_female)

sum(df.ed$male, df.ed$respiratory_male, df.ed$asthma_male, df.ed$injury_male, 
    df.ed$drowning_male, df.ed$falls_male, df.ed$transport_accidents_male,
    df.ed$heat_male, df.ed$heatstroke_male, df.ed$dehydration_electrolyte_male, 
    df.ed$renal_male, df.ed$infectious_parasitic_male, df.ed$otitis_male, 
    df.ed$lower_respiratory_male, df.ed$bacterial_enteritis_male)

sum(df.ed.rural$allcause, df.ed.rural$respiratory, df.ed.rural$asthma, df.ed.rural$injury, 
    df.ed.rural$drowning, df.ed.rural$falls, df.ed.rural$transport_accidents,
    df.ed.rural$heat, df.ed.rural$heatstroke, df.ed.rural$dehydration_electrolyte, 
    df.ed.rural$renal, df.ed.rural$infectious_parasitic, df.ed.rural$otitis, 
    df.ed.rural$lower_respiratory, df.ed.rural$bacterial_enteritis)

sum(df.ed.urban$allcause, df.ed.urban$respiratory, df.ed.urban$asthma, df.ed.urban$injury, 
    df.ed.urban$drowning, df.ed.urban$falls, df.ed.urban$transport_accidents,
    df.ed.urban$heat, df.ed.urban$heatstroke, df.ed.urban$dehydration_electrolyte, 
    df.ed.urban$renal, df.ed.urban$infectious_parasitic, df.ed.urban$otitis, 
    df.ed.urban$lower_respiratory, df.ed.urban$bacterial_enteritis)

# summary(df.ed.urban$density.x)
# summary(df.ed.urban$density.y)
# tail(df.ed.urban$density.x)
# tail(df.ed.urban$density.y)
# str(df.ed.urban$density.x)
# str(df.ed.urban$density.y)


#Failed attemps:
#df$Rural <- ifelse(df$density %in% c("rural"), 1, 0) # didn't work
#df$Urban <- ifelse(df$density %in% c("urban"), 1, 0) # didn't work

#df_ruralx <- subset(df, density.x == "rural")
#head(df_ruralx$fsa)
#tail(df_ruralx$fsa)

#df_ruraly <- subset(df, density.y == "rural")
#head(df_ruralx$fsa)
#tail(df_ruralx$fsa)

#colSums(df_ruralx[,c(4:33)])
#colSums(df_ruraly[,c(4:33)])

#WARM MONTHS
df.ed.warm <- df.ed%>%filter(grepl("1",df.ed$Warm))
colnames(df.ed.warm)
tail(df.ed$Warm)
colSums(df.ed.warm[,c(4:93)])


df.ed.warm.rural <- df.ed.rural%>%filter(grepl("1",df.ed.rural$Warm))
colSums(df.ed.warm.rural[,c(4:8)])
colSums(df.ed.warm.rural[,c(9:18)])
df.ed.warm.urban <- df.ed.urban%>%filter(grepl("1",df.ed.urban$Warm))
colSums(df.ed.warm.urban[,c(4:8)])
colSums(df.ed.warm.urban[,c(9:18)])

#Column totals
sum(df.ed.warm$age_0_4, df.ed.warm$respiratory_age_0_4, df.ed.warm$asthma_age_0_4, df.ed.warm$injury_age_0_4, 
    df.ed.warm$drowning_age_0_4, df.ed.warm$falls_age_0_4, df.ed.warm$transport_accidents_age_0_4,
    df.ed.warm$heat_age_0_4, df.ed.warm$heatstroke_age_0_4, df.ed.warm$dehydration_electrolyte_age_0_4, 
    df.ed.warm$renal_age_0_4, df.ed.warm$infectious_parasitic_age_0_4, df.ed.warm$otitis_age_0_4, 
    df.ed.warm$lower_respiratory_age_0_4, df.ed.warm$bacterial_enteritis_age_0_4)

sum(df.ed.warm$age_5_12, df.ed.warm$respiratory_age_5_12, df.ed.warm$asthma_age_5_12, df.ed.warm$injury_age_5_12, 
    df.ed.warm$drowning_age_5_12, df.ed.warm$falls_age_5_12, df.ed.warm$transport_accidents_age_5_12,
    df.ed.warm$heat_age_5_12, df.ed.warm$heatstroke_age_5_12, df.ed.warm$dehydration_electrolyte_age_5_12, 
    df.ed.warm$renal_age_5_12, df.ed.warm$infectious_parasitic_age_5_12, df.ed.warm$otitis_age_5_12, 
    df.ed.warm$lower_respiratory_age_5_12, df.ed.warm$bacterial_enteritis_age_5_12)

sum(df.ed.warm$age_13_18, df.ed.warm$respiratory_age_13_18, df.ed.warm$asthma_age_13_18, df.ed.warm$injury_age_13_18, 
    df.ed.warm$drowning_age_13_18, df.ed.warm$falls_age_13_18, df.ed.warm$transport_accidents_age_13_18,
    df.ed.warm$heat_age_13_18, df.ed.warm$heatstroke_age_13_18, df.ed.warm$dehydration_electrolyte_age_13_18, 
    df.ed.warm$renal_age_13_18, df.ed.warm$infectious_parasitic_age_13_18, df.ed.warm$otitis_age_13_18, 
    df.ed.warm$lower_respiratory_age_13_18, df.ed.warm$bacterial_enteritis_age_13_18)

sum(df.ed.warm$female, df.ed.warm$respiratory_female, df.ed.warm$asthma_female, df.ed.warm$injury_female, 
    df.ed.warm$drowning_female, df.ed.warm$falls_female, df.ed.warm$transport_accidents_female,
    df.ed.warm$heat_female, df.ed.warm$heatstroke_female, df.ed.warm$dehydration_electrolyte_female, 
    df.ed.warm$renal_female, df.ed.warm$infectious_parasitic_female, df.ed.warm$otitis_female, 
    df.ed.warm$lower_respiratory_female, df.ed.warm$bacterial_enteritis_female)

sum(df.ed.warm$male, df.ed.warm$respiratory_male, df.ed.warm$asthma_male, df.ed.warm$injury_male, 
    df.ed.warm$drowning_male, df.ed.warm$falls_male, df.ed.warm$transport_accidents_male,
    df.ed.warm$heat_male, df.ed.warm$heatstroke_male, df.ed.warm$dehydration_electrolyte_male, 
    df.ed.warm$renal_male, df.ed.warm$infectious_parasitic_male, df.ed.warm$otitis_male, 
    df.ed.warm$lower_respiratory_male, df.ed.warm$bacterial_enteritis_male)

sum(df.ed.warm.rural$allcause, df.ed.warm.rural$respiratory, df.ed.warm.rural$asthma, df.ed.warm.rural$injury, 
    df.ed.warm.rural$drowning, df.ed.warm.rural$falls, df.ed.warm.rural$transport_accidents,
    df.ed.warm.rural$heat, df.ed.warm.rural$heatstroke, df.ed.warm.rural$dehydration_electrolyte, 
    df.ed.warm.rural$renal, df.ed.warm.rural$infectious_parasitic, df.ed.warm.rural$otitis, 
    df.ed.warm.rural$lower_respiratory, df.ed.warm.rural$bacterial_enteritis)

sum(df.ed.warm.urban$allcause, df.ed.warm.urban$respiratory, df.ed.warm.urban$asthma, df.ed.warm.urban$injury, 
    df.ed.warm.urban$drowning, df.ed.warm.urban$falls, df.ed.warm.urban$transport_accidents,
    df.ed.warm.urban$heat, df.ed.warm.urban$heatstroke, df.ed.warm.urban$dehydration_electrolyte, 
    df.ed.warm.urban$renal, df.ed.warm.urban$infectious_parasitic, df.ed.warm.urban$otitis, 
    df.ed.warm.urban$lower_respiratory, df.ed.warm.urban$bacterial_enteritis)

########################################################################################################################
########################################################################################################################
########################################################################################################################
#TIMESERIES PLOTS DAY VS COUNT
# 
# #TIMESERIES OF HOSPITAL ADMISSIONS PLOTS DAY VS COUNT
# df.ad1.0 <- df.ad %>% group_by(year) %>% summarise(tot = sum(allcause)) #group all of the information for each year together to see cahnges by year
# plot(df.ad1.0$year, df.ad1.0$tot,  xlab= "Year", ylab = "All-cause Addmissions",
#      main = "Annual All-cause Hospital Admissions 2005-2015")
# 
# df.ad1.1 <- df.ad %>% group_by(date) %>% summarise(tot = sum(allcause)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad1.1$date, df.ad1.1$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause Hospital Admissions 2005-2015")
# df.ad1.2 <- df.ad %>% group_by(date) %>% summarise(tot = sum(allcause_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad1.2$date, df.ad1.2$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause Hospital Admissions for Kids Ages 0-4")
# df.ad1.3 <- df.ad %>% group_by(date) %>% summarise(tot = sum(allcause_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad1.3$date, df.ad1.3$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause Hospital Admissions for Kids Ages 5-12")
# df.ad1.4 <- df.ad %>% group_by(date) %>% summarise(tot = sum(allcause_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad1.4$date, df.ad1.4$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause Hospital Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ad2.0 <- df.ad %>% group_by(year) %>% summarise(tot = sum(infectious_parasitic)) #group all of the information for each year together to see cahnges by year
# plot(df.ad2.0$year, df.ad2.0$tot,  xlab= "Year", ylab = "Infectious/Parasitic Addmissions",
#      main = "Annual Infectious/Parasitic Hospital Admissions 2005-2015")
# 
# df.ad2.1 <- df.ad %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad2.1$date, df.ad2.1$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic Hospital Admissions 2005-2015")
# df.ad2.2 <- df.ad %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad2.2$date, df.ad2.2$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic Hospital Admissions for Kids Ages 0-4")
# df.ad2.3 <- df.ad %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad2.3$date, df.ad2.3$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic Hospital Admissions for Kids Ages 5-12")
# df.ad2.4 <- df.ad %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad2.4$date, df.ad2.4$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic Hospital Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ad3.0 <- df.ad %>% group_by(year) %>% summarise(tot = sum(respiratory)) #group all of the information for each year together to see cahnges by year
# plot(df.ad3.0$year, df.ad3.0$tot,  xlab= "Year", ylab = "Respiratory Addmissions",
#      main = "Annual Respiratory Hospital Admissions 2005-2015")
# 
# df.ad3.1 <- df.ad %>% group_by(date) %>% summarise(tot = sum(respiratory)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad3.1$date, df.ad3.1$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory Hospital Admissions 2005-2015")
# df.ad3.2 <- df.ad %>% group_by(date) %>% summarise(tot = sum(respiratory_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad3.2$date, df.ad3.2$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory Hospital Admissions for Kids Ages 0-4")
# df.ad3.3 <- df.ad %>% group_by(date) %>% summarise(tot = sum(respiratory_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad3.3$date, df.ad3.3$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory Hospital Admissions for Kids Ages 5-12")
# df.ad3.4 <- df.ad %>% group_by(date) %>% summarise(tot = sum(respiratory_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad3.4$date, df.ad3.4$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory Hospital Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ad4.0 <- df.ad %>% group_by(year) %>% summarise(tot = sum(injury)) #group all of the information for each year together to see cahnges by year
# plot(df.ad3.0$year, df.ad3.0$tot,  xlab= "Year", ylab = "Injury Addmissions",
#      main = "Annual Injury Hospital Admissions 2005-2015")
# 
# df.ad4.1 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad4.1$date, df.ad4.1$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury Hospital Admissions 2005-2015")
# df.ad4.2 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad4.2$date, df.ad4.2$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury Hospital Admissions for Kids Ages 0-4")
# df.ad4.3 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad4.3$date, df.ad4.3$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury Hospital Admissions for Kids Ages 5-12")
# df.ad4.4 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad4.4$date, df.ad4.4$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury Hospital Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ad5.0 <- df.ad %>% group_by(year) %>% summarise(tot = sum(heat)) #group all of the information for each year together to see cahnges by year
# plot(df.ad5.0$year, df.ad5.0$tot,  xlab= "Year", ylab = "Heat Addmissions",
#      main = "Annual Heat Hospital Admissions 2005-2015")
# 
# df.ad5.1 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad5.1$date, df.ad5.1$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat Hospital Admissions 2005-2015")
# df.ad5.2 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad5.2$date, df.ad5.2$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat Hospital Admissions for Kids Ages 0-4")
# df.ad5.3 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad5.3$date, df.ad5.3$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat Hospital Admissions for Kids Ages 5-12")
# df.ad5.4 <- df.ad %>% group_by(date) %>% summarise(tot = sum(injury_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ad5.4$date, df.ad5.4$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat Hospital Admissions for Kids Ages 13-18")
# 
# 
# 
# 
# 
# #TIMESERIES OF EMERGENCY DEPT. ADMISSIONS PLOTS DAY VS COUNT
# df.ed1.0 <- df.ed %>% group_by(year) %>% summarise(tot = sum(allcause)) #group all of the information for each year together to see cahnges by year
# plot(df.ed1.0$year, df.ed1.0$tot,  xlab= "Year", ylab = "All-cause Addmissions",
#      main = "Annual All-cause ER Admissions 2005-2015")
# 
# df.ed1.1 <- df.ed %>% group_by(date) %>% summarise(tot = sum(allcause)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed1.1$date, df.ed1.1$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause ER Admissions 2005-2015")
# df.ed1.2 <- df.ed %>% group_by(date) %>% summarise(tot = sum(age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed1.2$date, df.ed1.2$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause ER Admissions for Kids Ages 0-4")
# df.ed1.3 <- df.ed %>% group_by(date) %>% summarise(tot = sum(age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed1.3$date, df.ed1.3$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause ER Admissions for Kids Ages 5-12")
# df.ed1.4 <- df.ed %>% group_by(date) %>% summarise(tot = sum(age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed1.4$date, df.ed1.4$tot,  xlab= "Date", ylab = "All-cause Addmissions",
#      main = "Daily All-cause ER Admissions for Kids Ages 13-18")
# 
# 
# 
# 
# df.ed2.0 <- df.ed %>% group_by(year) %>% summarise(tot = sum(infectious_parasitic)) #group all of the information for each year together to see cahnges by year
# plot(df.ed2.0$year, df.ed2.0$tot,  xlab= "Year", ylab = "Infectious/Parasitic Addmissions",
#      main = "Annual Infectious/Parasitic ER Admissions 2005-2015")
# 
# df.ed2.1 <- df.ed %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed2.1$date, df.ed2.1$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic ER Admissions 2005-2015")
# df.ed2.2 <- df.ed %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed2.2$date, df.ed2.2$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic ER Admissions for Kids Ages 0-4")
# df.ed2.3 <- df.ed %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed2.3$date, df.ed2.3$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic ER Admissions for Kids Ages 5-12")
# df.ed2.4 <- df.ed %>% group_by(date) %>% summarise(tot = sum(infectious_parasitic_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed2.4$date, df.ed2.4$tot,  xlab= "Date", ylab = "Infectious/Parasitic Addmissions",
#      main = "Daily Infectious/Parasitic ER Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ed3.0 <- df.ed %>% group_by(year) %>% summarise(tot = sum(respiratory)) #group all of the information for each year together to see cahnges by year
# plot(df.ed3.0$year, df.ed3.0$tot,  xlab= "Year", ylab = "Respiratory Addmissions",
#      main = "Annual Respiratory ER Admissions 2005-2015")
# 
# df.ed3.1 <- df.ed %>% group_by(date) %>% summarise(tot = sum(respiratory)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed3.1$date, df.ed3.1$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory ER Admissions 2005-2015")
# df.ed3.2 <- df.ed %>% group_by(date) %>% summarise(tot = sum(respiratory_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed3.2$date, df.ed3.2$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory ER Admissions for Kids Ages 0-4")
# df.ed3.3 <- df.ed %>% group_by(date) %>% summarise(tot = sum(respiratory_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed3.3$date, df.ed3.3$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory ER Admissions for Kids Ages 5-12")
# df.ed3.4 <- df.ed %>% group_by(date) %>% summarise(tot = sum(respiratory_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed3.4$date, df.ed3.4$tot,  xlab= "Date", ylab = "Respiratory Addmissions",
#      main = "Daily Respiratory ER Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ed4.0 <- df.ed %>% group_by(year) %>% summarise(tot = sum(injury)) #group all of the information for each year together to see cahnges by year
# plot(df.ed4.0$year, df.ed4.0$tot,  xlab= "Year", ylab = "Injury Addmissions",
#      main = "Annual Injury ER Admissions 2005-2015")
# 
# df.ed4.1 <- df.ed %>% group_by(date) %>% summarise(tot = sum(injury)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed4.1$date, df.ed4.1$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury ER Admissions 2005-2015")
# df.ed4.2 <- df.ed %>% group_by(date) %>% summarise(tot = sum(injury_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed4.2$date, df.ed4.2$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury ER Admissions for Kids Ages 0-4")
# df.ed4.3 <- df.ed %>% group_by(date) %>% summarise(tot = sum(injury_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed4.3$date, df.ed4.3$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury ER Admissions for Kids Ages 5-12")
# df.ed4.4 <- df.ed %>% group_by(date) %>% summarise(tot = sum(injury_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed4.4$date, df.ed4.4$tot,  xlab= "Date", ylab = "Injury Addmissions",
#      main = "Daily Injury ER Admissions for Kids Ages 13-18")
# 
# 
# 
# df.ed5.0 <- df.ed %>% group_by(year) %>% summarise(tot = sum(heat)) #group all of the information for each year together to see cahnges by year
# plot(df.ed5.0$year, df.ed5.0$tot,  xlab= "Year", ylab = "Heat Addmissions",
#      main = "Annual Heat ER Admissions 2005-2015")
# 
# df.ed5.1 <- df.ed %>% group_by(date) %>% summarise(tot = sum(heat)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed5.1$date, df.ed5.1$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat ER Admissions 2005-2015")
# df.ed5.2 <- df.ed %>% group_by(date) %>% summarise(tot = sum(heat_age_0_4)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed5.2$date, df.ed5.2$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat ER Admissions for Kids Ages 0-4")
# df.ed5.3 <- df.ed %>% group_by(date) %>% summarise(tot = sum(heat_age_5_12)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed5.3$date, df.ed5.3$tot,  xlab= "Date", ylab = "Heat Addmissions",
#      main = "Daily Heat ER Admissions for Kids Ages 5-12")
# df.ed5.4 <- df.ed %>% group_by(date) %>% summarise(tot = sum(heat_age_13_18)) #group all of the information for each date together and then add the counts in each FSA on that day
# plot(df.ed5.4$date, df.ed5.4$tot,  xlab= "Date", ylab = "Heat Admissions",
#      main = "Daily Heat ER Admissions for Kids Ages 13-18")
# 
# 
# ########################################################################################################################
# ########################################################################################################################
# ########################################################################################################################
# #TIMESERIES PLOT OF DAILY MAX TEMPS OVER 2005 FOR K0A, M5J AND P3A
# #ont.temp.data %>% filter(Date < ymd("2006-01-01"))
# 
# ont.temp.data.K0A<-subset(ont.temp.data, fsa == "K0A")
# plot(ont.temp.data.K0A$date, ont.temp.data.K0A$Tmax_avg, xlab= "Date", ylab = "Daily Maximum Temperature",
#      main = "Daily maximum temperature in K0A, 2005-2015")
# ont.temp.data.K0A.05<-subset(ont.temp.data, fsa == "K0A" & year == "2005")
# plot(ont.temp.data.K0A.05$date, ont.temp.data.K0A.05$Tmax_avg, xlab= "Date", ylab = "Daily Maximum Temperature",
#      main = "Daily maximum temperature in K0A, 2005")
# 
# ont.temp.data.M5J.05<-subset(ont.temp.data, fsa == "M5J" & year == "2005")
# plot(ont.temp.data.M5J.05$date, ont.temp.data.M5J.05$Tmax_avg, xlab= "Date", ylab = "Daily Maximum Temperature",
#      main = "Daily maximum temperature in M5J, 2005")
# hist(ont.temp.data.M5J.05$Tmax_avg)
# descr(ont.temp.data.M5J.05)
# 
# ont.temp.data.P3A.05<-subset(ont.temp.data, fsa == "P3A" & year == "2005")
# plot(ont.temp.data.P3A.05$date, ont.temp.data.P3A.05$Tmax_avg, xlab= "Date", ylab = "Daily Maximum Temperature",
#      main = "Daily maximum temperature in P3A, 2005")
# hist(ont.temp.data.P3A.05$Tmax_avg)
# descr(ont.temp.data.P3A.05)

#M5J: mean max=13.84, mean min= 4.86, min max=-14.95, min min=-23.53, median max=13.98, median min=5.1, max max=35, max min=23.03
#P3A: mean max=10.6, mean min=0.34, min max=-21.07, min min= -33.42, median max=10.56, median min=0.89, max max=34.66, max min=20.27
#Sudbury has more variation in max and min temperatures, is overall colder, and has a cmparable max max and max min


########################################################################################################################
########################################################################################################################
########################################################################################################################
#EXPOSURE DEFINITIONS BY95TH, 97.5TH, AND 99TH%
#DATAFRAME PER FSA
# quantile(ont.temp.data$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# FSA.names <- as.data.frame(table(ont.temp.data$fsa)) #there are FSA's that start with K,L,M,N and P. There is no O.

# ont.temp.data.K0A<-subset(ont.temp.data, fsa == "K0A")
# quantile(ont.temp.data.K0A$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)

# 
# #Creating df and quantiles for each FSA K0-K9
# ont.temp.data.K0 <- ont.temp.data%>%filter(grepl("K0", ont.temp.data$fsa))
# quantile(ont.temp.data.K0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K1 <- ont.temp.data%>%filter(grepl("K1", ont.temp.data$fsa))
# quantile(ont.temp.data.K1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K2 <- ont.temp.data%>%filter(grepl("K2", ont.temp.data$fsa))
# quantile(ont.temp.data.K2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K3 <- ont.temp.data%>%filter(grepl("K3", ont.temp.data$fsa))
# quantile(ont.temp.data.K3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K4 <- ont.temp.data%>%filter(grepl("K4", ont.temp.data$fsa))
# quantile(ont.temp.data.K4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K5 <- ont.temp.data%>%filter(grepl("K5", ont.temp.data$fsa))
# quantile(ont.temp.data.K5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K6 <- ont.temp.data%>%filter(grepl("K6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K7 <- ont.temp.data%>%filter(grepl("K7", ont.temp.data$fsa))
# quantile(ont.temp.data.K7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K8 <- ont.temp.data%>%filter(grepl("K8", ont.temp.data$fsa))
# quantile(ont.temp.data.K8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.K9 <- ont.temp.data%>%filter(grepl("K9", ont.temp.data$fsa))
# quantile(ont.temp.data.K9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# 
# 
# #Creating df and quantiles for each FSA L0-L9
# ont.temp.data.L0 <- ont.temp.data%>%filter(grepl("L0", ont.temp.data$fsa))
# quantile(ont.temp.data.L0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L1 <- ont.temp.data%>%filter(grepl("L1", ont.temp.data$fsa))
# quantile(ont.temp.data.L1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L2 <- ont.temp.data%>%filter(grepl("L2", ont.temp.data$fsa))
# quantile(ont.temp.data.L2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L3 <- ont.temp.data%>%filter(grepl("L3", ont.temp.data$fsa))
# quantile(ont.temp.data.L3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L4 <- ont.temp.data%>%filter(grepl("L4", ont.temp.data$fsa))
# quantile(ont.temp.data.L4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L5 <- ont.temp.data%>%filter(grepl("L5", ont.temp.data$fsa))
# quantile(ont.temp.data.L5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L6 <- ont.temp.data%>%filter(grepl("L6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L7 <- ont.temp.data%>%filter(grepl("L7", ont.temp.data$fsa))
# quantile(ont.temp.data.L7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L8 <- ont.temp.data%>%filter(grepl("L8", ont.temp.data$fsa))
# quantile(ont.temp.data.L8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.L9 <- ont.temp.data%>%filter(grepl("L9", ont.temp.data$fsa))
# quantile(ont.temp.data.L9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# 
# 
# #Creating df and quantiles for each FSA M0-M9
# ont.temp.data.M0 <- ont.temp.data%>%filter(grepl("M0", ont.temp.data$fsa))
# quantile(ont.temp.data.M0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M1 <- ont.temp.data%>%filter(grepl("M1", ont.temp.data$fsa))
# quantile(ont.temp.data.M1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M2 <- ont.temp.data%>%filter(grepl("M2", ont.temp.data$fsa))
# quantile(ont.temp.data.M2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M3 <- ont.temp.data%>%filter(grepl("M3", ont.temp.data$fsa))
# quantile(ont.temp.data.M3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M4 <- ont.temp.data%>%filter(grepl("M4", ont.temp.data$fsa))
# quantile(ont.temp.data.M4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M5 <- ont.temp.data%>%filter(grepl("M5", ont.temp.data$fsa))
# quantile(ont.temp.data.M5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M6 <- ont.temp.data%>%filter(grepl("M6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M7 <- ont.temp.data%>%filter(grepl("M7", ont.temp.data$fsa))
# quantile(ont.temp.data.M7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M8 <- ont.temp.data%>%filter(grepl("M8", ont.temp.data$fsa))
# quantile(ont.temp.data.M8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.M9 <- ont.temp.data%>%filter(grepl("M9", ont.temp.data$fsa))
# quantile(ont.temp.data.M9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# 
# #Creating df and quantiles for each FSA N0-N9
# ont.temp.data.N0 <- ont.temp.data%>%filter(grepl("N0", ont.temp.data$fsa))
# quantile(ont.temp.data.N0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N1 <- ont.temp.data%>%filter(grepl("N1", ont.temp.data$fsa))
# quantile(ont.temp.data.N1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N2 <- ont.temp.data%>%filter(grepl("N2", ont.temp.data$fsa))
# quantile(ont.temp.data.N2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N3 <- ont.temp.data%>%filter(grepl("N3", ont.temp.data$fsa))
# quantile(ont.temp.data.N3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N4 <- ont.temp.data%>%filter(grepl("N4", ont.temp.data$fsa))
# quantile(ont.temp.data.N4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N5 <- ont.temp.data%>%filter(grepl("N5", ont.temp.data$fsa))
# quantile(ont.temp.data.N5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N6 <- ont.temp.data%>%filter(grepl("N6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N7 <- ont.temp.data%>%filter(grepl("N7", ont.temp.data$fsa))
# quantile(ont.temp.data.N7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N8 <- ont.temp.data%>%filter(grepl("N8", ont.temp.data$fsa))
# quantile(ont.temp.data.N8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.N9 <- ont.temp.data%>%filter(grepl("N9", ont.temp.data$fsa))
# quantile(ont.temp.data.N9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# 
# 
# #Creating df and quantiles for each FSA O0-O9
# ont.temp.data.O0 <- ont.temp.data%>%filter(grepl("O0", ont.temp.data$fsa))
# quantile(ont.temp.data.O0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O1 <- ont.temp.data%>%filter(grepl("O1", ont.temp.data$fsa))
# quantile(ont.temp.data.O1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O2 <- ont.temp.data%>%filter(grepl("O2", ont.temp.data$fsa))
# quantile(ont.temp.data.O2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O3 <- ont.temp.data%>%filter(grepl("O3", ont.temp.data$fsa))
# quantile(ont.temp.data.O3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O4 <- ont.temp.data%>%filter(grepl("O4", ont.temp.data$fsa))
# quantile(ont.temp.data.O4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O5 <- ont.temp.data%>%filter(grepl("O5", ont.temp.data$fsa))
# quantile(ont.temp.data.O5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O6 <- ont.temp.data%>%filter(grepl("O6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O7 <- ont.temp.data%>%filter(grepl("O7", ont.temp.data$fsa))
# quantile(ont.temp.data.O7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O8 <- ont.temp.data%>%filter(grepl("O8", ont.temp.data$fsa))
# quantile(ont.temp.data.O8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.O9 <- ont.temp.data%>%filter(grepl("O9", ont.temp.data$fsa))
# quantile(ont.temp.data.O9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# 
# 
# #Creating df and quantiles for each FSA P0-P9
# ont.temp.data.P0 <- ont.temp.data%>%filter(grepl("P0", ont.temp.data$fsa))
# quantile(ont.temp.data.P0$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P1 <- ont.temp.data%>%filter(grepl("P1", ont.temp.data$fsa))
# quantile(ont.temp.data.P1$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P2 <- ont.temp.data%>%filter(grepl("P2", ont.temp.data$fsa))
# quantile(ont.temp.data.P2$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P3 <- ont.temp.data%>%filter(grepl("P3", ont.temp.data$fsa))
# quantile(ont.temp.data.P3$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P4 <- ont.temp.data%>%filter(grepl("P4", ont.temp.data$fsa))
# quantile(ont.temp.data.P4$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P5 <- ont.temp.data%>%filter(grepl("P5", ont.temp.data$fsa))
# quantile(ont.temp.data.P5$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P6 <- ont.temp.data%>%filter(grepl("P6", ont.temp.data$fsa))
# quantile(ont.temp.data.K6$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P7 <- ont.temp.data%>%filter(grepl("P7", ont.temp.data$fsa))
# quantile(ont.temp.data.P7$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P8 <- ont.temp.data%>%filter(grepl("P8", ont.temp.data$fsa))
# quantile(ont.temp.data.P8$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)
# 
# ont.temp.data.P9 <- ont.temp.data%>%filter(grepl("P9", ont.temp.data$fsa))
# quantile(ont.temp.data.P9$Tmax_avg, c(0.95, 0.975, 0.99), na.rm = TRUE)

########################################################################################################################
#Trying forloops
# for (i in 1:4){
#         print("Hi")
# }
# 0
#Funtion for EHE definition (threshold and number of consecutive days)
# Generate a function to create a heat wave indicator by groups. Function will mark all
# days meeting criteria with a value of 1. 
# Inputs to the fuction are:
#   - x: the time series of daily temperature values
#   - thr: the threhold value for the heat wave in degrees
#   - dur: the length of the heat wave
#   - group: the grouping variable (optional)

fun.hw.thr <- function(x,thr,dur,group=NULL) {
  as.numeric(apply(Lag(x>=thr,0:(dur-1),group=group),
                   1,sum,na.rm=T)>(dur-1))
}

#Loop for Hospital Admissions 99% 2 days and creating hw lag 1 and 2
dat1 <- data.frame() # create an empty data frame named "dat3"
fsa.list.ad.99 <- unique(df.ad.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ad.99)) {
  dfa.99 <- subset(df.ad.warm, df.ad.warm$fsa == fsa.list.ad.99[i]) # create a dataframe ("dfa.99") that includes only the data for a single FSA
  threshold_99a <- quantile(dfa.99$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
  dfa.99$tmax_99a <- ifelse(dfa.99$Tmax_avg >= threshold_99a, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfa.99$hw <- fun.hw.thr(dfa.99$Tmax_avg, threshold_99a, 2, group = dfa.99$year) # Apply the function to the fake data (example threshold of 91 degrees, example duration of 2 days)
  dfa.99$hw_lag1 <- as.factor(lag(dfa.99$hw, 1, group = dfa.99$year))
  dfa.99$hw_lag2 <- as.factor(lag(dfa.99$hw, 2, group = dfa.99$year))
  dat1 <- rbind(dat1, dfa.99) # add the single-FSA data frame ("dfa.99") to dat3
  print(i)
}
df.ad.99 <- dat1
df.ad.99$tmax_99a <- as.factor(df.ad.99$tmax_99a) # convert the new indicator variable to factor

tail(df.ad.99$tmax_99a)
head(df.ad.99$tmax_99a)
table(df.ad.99$tmax_99a)

df.ad.99.tab3.1<-subset(df.ad.99, hw == "1")
tail(df.ad.99.tab3.1$hw)
head(df.ad.99.tab3.1$hw)
colSums(df.ad.99.tab3.1[,c(4:8)])
colSums(df.ad.99.tab3.1[,c(14:23)])


df.ad.99.tab3.0<-subset(df.ad.99, hw == "0")
tail(df.ad.99.tab3.0$hw)
head(df.ad.99.tab3.0$hw)
colSums(df.ad.99.tab3.0[,c(4:8)])
colSums(df.ad.99.tab3.0[,c(14:23)])


#Loop for Emergency Visits 99% 2 days and creating hw lag 1 and 2
dat2 <- data.frame() # create an empty data frame named "dat4"
fsa.list.ed.99 <- unique(df.ed.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ed.99)) {
  dfe.99 <- subset(df.ed.warm, df.ed.warm$fsa == fsa.list.ed.99[i]) # create a dataframe ("dfe.99") that includes only the data for a single FSA
  threshold_99e <- quantile(dfe.99$Tmax_avg, 0.99, na.rm = TRUE) # identify the 97.5th percentile of temperature for that FSA
  dfe.99$tmax_99e <- ifelse(dfe.99$Tmax_avg >= threshold_99e, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfe.99$hw <- fun.hw.thr(dfe.99$Tmax_avg, threshold_99e, 2, group = dfe.99$year) # Apply the function to df.ed
  dfe.99$hw_lag1 <- as.factor(lag(dfe.99$hw, 1, group = dfe.99$year))
  dfe.99$hw_lag2 <- as.factor(lag(dfe.99$hw, 2, group = dfe.99$year))
  dat2 <- rbind(dat2, dfe.99) # add the single-FSA data frame ("dfe.99") to dat4
  print(i)
}
df.ed.99 <- dat2
df.ed.99$tmax_99e <- as.factor(df.ed.99$tmax_99e) # convert the new indicator variable to factor

ntail(df.ed.99$tmax_99e)
head(df.ed.99$tmax_99e)
table(df.ed.99$tmax_99e)


df.ed.99.tab3.1<-subset(df.ed.99, hw == "1")
tail(df.ed.99.tab3.1$hw)
head(df.ed.99.tab3.1$hw)
colSums(df.ed.99.tab3.1[,c(4:18)])

df.ed.99.tab3.0<-subset(df.ed.99, hw == "0")
tail(df.ed.99.tab3.0$hw)
head(df.ed.99.tab3.0$hw)
colSums(df.ed.99.tab3.0[,c(4:18)])

#Loop for Hospital Admissions 97.5% 2 days
dat3 <- data.frame() # create an empty data frame named "dat1"
fsa.list.ad.975<- unique(df.ad.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ad.975)) {
  dfa.975 <- subset(df.ad.warm, df.ad.warm$fsa == fsa.list.ad.975[i]) # create a dataframe ("dfa.975) that includes only the data for a single FSA
  threshold_975a <- quantile(dfa.975$Tmax_avg, 0.975, na.rm = TRUE) # identify the 97.5th percentile of temperature for that FSA
  dfa.975$tmax_975a <- ifelse(dfa.975$Tmax_avg >= threshold_975a, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfa.975$hw <- fun.hw.thr(dfa.975$Tmax_avg, threshold_975a, 2, group = dfa.975$year) # Apply the function to the fake data (example threshold of 91 degrees, example duration of 2 days)
  dat3 <- rbind(dat3, dfa.975) # add the single-FSA data frame ("dfa.975") to dat1
  print(i)
}
df.ad.975 <- dat3
df.ad.975$tmax_975a <- as.factor(df.ad.975$tmax_975a) # convert the new indicator variable to factor

tail(df.ad.975$tmax_975a)
head(df.ad.975$tmax_975a)
table(df.ad.975$tmax_975a)

#Loop for Emergency Visits 97.5% 2 days
dat4 <- data.frame() # create an empty data frame named "dat2"
fsa.list.ed.975 <- unique(df.ed.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ed.975)) {
  dfe.975 <- subset(df.ed.warm, df.ed.warm$fsa == fsa.list.ed.975[i]) # create a dataframe ("dfe") that includes only the data for a single FSA
  threshold_975e <- quantile(dfe.975$Tmax_avg, 0.975, na.rm = TRUE) # identify the 97.5th percentile of temperature for that FSA
  dfe.975$tmax_975e <- ifelse(dfe.975$Tmax_avg >= threshold_975e, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfe.975$hw <- fun.hw.thr(dfe.975$Tmax_avg, threshold_975e, 2, group = dfe.975$year) # Apply the function to df.ed
  dat4 <- rbind(dat4, dfe.975) # add the single-FSA data frame ("dfe.975") to dat2
  print(i)
}
df.ed.975 <- dat4
df.ed.975$tmax_975e <- as.factor(df.ed.975$tmax_975e) # convert the new indicator variable to factor

tail(df.ed.975$tmax_975e)
head(df.ed.975$tmax_975e)
table(df.ed.975$tmax_975e)



#Loop for Hospital Admissions 95% 2 days
dat5 <- data.frame() # create an empty data frame named "dat1"
fsa.list.ad.95<- unique(df.ad.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ad.95)) {
  dfa.95 <- subset(df.ad.warm, df.ad.warm$fsa == fsa.list.ad.95[i]) # create a dataframe ("dfa.95) that includes only the data for a single FSA
  threshold_95a <- quantile(dfa.95$Tmax_avg, 0.95, na.rm = TRUE) # identify the 97.5th percentile of temperature for that FSA
  dfa.95$tmax_95a <- ifelse(dfa.95$Tmax_avg >= threshold_95a, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfa.95$hw <- fun.hw.thr(dfa.95$Tmax_avg, threshold_95a, 2, group = dfa.95$year) # Apply the function to the fake data (example threshold of 91 degrees, example duration of 2 days)
  dat5 <- rbind(dat5, dfa.95) # add the single-FSA data frame ("dfa.95") to dat1
  print(i)
}
df.ad.95 <- dat5
df.ad.95$tmax_95a <- as.factor(df.ad.95$tmax_95a) # convert the new indicator variable to factor

tail(df.ad.95$tmax_95a)
head(df.ad.95$tmax_95a)
table(df.ad.95$tmax_95a)

#Loop for Emergency Visits 95% 2 days
dat6 <- data.frame() # create an empty data frame named "dat2"
fsa.list.ed.95 <- unique(df.ed.warm$fsa) # create a list of the unique FSAs in your original dataset
for (i in seq_along(fsa.list.ed.95)) {
  dfe.95 <- subset(df.ed.warm, df.ed.warm$fsa == fsa.list.ed.95[i]) # create a dataframe ("dfe") that includes only the data for a single FSA
  threshold_95e <- quantile(dfe.95$Tmax_avg, 0.95, na.rm = TRUE) # identify the 97.5th percentile of temperature for that FSA
  dfe.95$tmax_95e <- ifelse(dfe.95$Tmax_avg >= threshold_95e, 1, 0) # create an indicator variable for whether or not each day is above the threshold
  dfe.95$hw <- fun.hw.thr(dfe.95$Tmax_avg, threshold_95e, 2, group = dfe.95$year) # Apply the function to df.ed
  dat6 <- rbind(dat6, dfe.95) # add the single-FSA data frame ("dfe.975") to dat2
  print(i)
}
df.ed.95 <- dat6
df.ed.975$tmax_95e <- as.factor(df.ed.95$tmax_95e) # convert the new indicator variable to factor

tail(df.ed.95$tmax_95e)
head(df.ed.95$tmax_95e)
table(df.ed.95$tmax_95e)










########################################################################################################################
########################################################################################################################
########################################################################################################################
#CONDITIONAL POISSON REGRESSION HOSPITAL 99
AD 99

#data <- read.dta("df.ad.99")
#summary(data)

# GENERATE DOW AND STRATUM
df.ad.99$month   <- as.factor(months(df.ad.99$date))
df.ad.99$year    <- as.factor(format(df.ad.99$date, format="%Y") )
df.ad.99$dow     <- as.factor(weekdays(df.ad.99$date))
df.ad.99$fsa     <- as.factor(df.ad.99$fsa)
df.ad.99$stratum <- as.factor(df.ad.99$year:df.ad.99$month:df.ad.99$dow:df.ad.99$fsa)
length(unique(df.ad.99$stratum))
df.ad.99 <- data[order(df.ad.99$date),]

names(df.ad.99)
glimpse(df.ad.99)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_99, data=df.ad.99, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
dim(df.ad.99)

fit1.1<- gnm(allcause ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.1)
exp(0.04860)
exp(0.04860 - 1.96 * 0.03407)
exp(0.04860 + 1.96 * 0.03407)

fit1.2 <- gnm(respiratory ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.2) 
exp(0.23443)
exp(0.23443 - 1.96 * 0.05054)
exp(0.23443 + 1.96 * 0.05054)
fit1.2.1 <- gnm(asthma ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.2.1) 
exp(0.25737)
exp(0.25737 - 1.96 * 0.05663)
exp(0.25737 + 1.96 * 0.05663)

fit1.3 <- gnm(injury ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3) 
exp(-0.13611)
exp(-0.13611 - 1.96 * 0.04915)
exp(-0.13611 + 1.96 * 0.04915)
fit1.3.1 <- gnm(drowning ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.1) 
exp(0.06800)
exp(0.06800 - 1.96 * 0.04675)
exp(0.06800 + 1.96 * 0.04675)
fit1.3.2 <- gnm(falls ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.2) 
exp(-0.07151)
exp(-0.07151 - 1.96 * 0.05483)
exp(-0.07151 + 1.96 * 0.05483)
fit1.3.3 <- gnm(transport_accidents ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.3) 
exp(-0.13747)
exp(-0.13747 - 1.96 * 0.04963)
exp(-0.13747 + 1.96 * 0.04963)


fit1.4 <- gnm(heat ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4) 
exp(0.36669)
exp(0.36669 - 1.96 * 0.03968)
exp(0.36669 + 1.96 * 0.03968)
fit1.4.1 <- gnm(heatstroke ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.1) 
exp(12.385)
exp(12.385 - 1.96 * 2.739)
exp(12.385 + 1.96 * 2.739)
fit1.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.2) 
exp(0.08057)
exp(0.08057 - 1.96 * 0.04343)
exp(0.08057 + 1.96 * 0.04343)
fit1.4.3 <- gnm(renal ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.3)
exp(-0.05671)
exp(-0.05671 - 1.96 * 0.04831)
exp(-0.05671 + 1.96 * 0.04831)

fit1.5 <- gnm(infectious_parasitic ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5)
exp(0.30950)
exp(0.30950  - 1.96 *  0.04871)
exp(0.30950  + 1.96 *  0.04871)
fit1.5.1 <- gnm(otitis ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.1)
exp(-9.271)
exp(-9.271  - 1.96 *  3.906)
exp(-9.271  + 1.96 *  3.906)
fit1.5.2 <- gnm(lower_respiratory ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.2)
exp(0.4090)
exp(0.4090  - 1.96 *  0.0516)
exp(0.4090  + 1.96 *  0.0516)
fit1.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.3)
exp(0.17207)
exp(0.17207  - 1.96 *  0.05335)
exp(0.17207  + 1.96 *  0.05335)
##########################################################
#CONDITIONAL POISSON REGRESSION AD 99 BY AGE AND SEX
library(gnm)
dim(df.ad.99)

AD 99 by age and sex

#AD ALL CAUSE BY AGE
fit1.11<- gnm(allcause_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.11)
exp(0.05059)
exp(0.05059 - 1.96 * 0.03981)
exp(0.05059 + 1.96 * 0.03981)
fit1.12<- gnm(allcause_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.12)
exp(-0.02733)
exp(-0.02733 - 1.96 * 0.04801)
exp(-0.02733 + 1.96 * 0.04801)
fit1.13<- gnm(allcause_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.13)
exp(0.09747)
exp(0.09747 - 1.96 * 0.04276 )
exp(0.09747 + 1.96 * 0.04276 )

#AD RESPIRATORY BY AGE
fit1.21<- gnm(respiratory_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.21)
exp(0.17175)
exp(0.17175 - 1.96 * 0.05233)
exp(0.17175 + 1.96 * 0.05233)
fit1.22<- gnm(respiratory_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.22)
exp(-0.06810)
exp(-0.06810 - 1.96 * 0.06726)
exp(-0.06810 + 1.96 * 0.06726)
fit1.23<- gnm(respiratory_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.23)
exp(0.78488)
exp(0.78488 - 1.96 * 0.04252)
exp(0.78488 + 1.96 * 0.04252)

fit1.21.1<- gnm(asthma_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.21.1)
exp(0.32053)
exp(0.32053 - 1.96 * 0.05361)
exp(0.32053 + 1.96 * 0.05361)
fit1.22.1<- gnm(asthma_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.22.1)
exp(-0.21212)
exp(-0.21212 - 1.96 * 0.07936)
exp(-0.21212 + 1.96 * 0.07936)
fit1.23.1<- gnm(asthma_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.23.1)
exp(0.80615)
exp(0.80615 - 1.96 * 0.04539)
exp(0.80615 + 1.96 * 0.04539)

#AD INJURY BY AGE
#Injury 0-4
fit1.31<- gnm(injury_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.31)
exp(0.12634)
exp(0.12634 - 1.96 * 0.04654)
exp(0.12634 + 1.96 * 0.04654)
fit1.32<- gnm(injury_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.32)
exp(-0.2833)
exp(-0.2833 - 1.96 * 0.0558)
exp(-0.2833 + 1.96 * 0.0558)
fit1.33<- gnm(injury_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.33)
exp(-0.21436)
exp(-0.21436 - 1.96 * 0.05278)
exp(-0.21436 + 1.96 * 0.05278)

fit1.31.1<- gnm(drowning_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.31.1)
exp(0.00)
exp(0.00 - 1.96 * 88.28)
exp(0.00 + 1.96 * 88.28)
fit1.32.1<- gnm(drowning_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.32.1)
exp(-9.285)
exp(-9.285 - 1.96 * 4.194)
exp(-9.285 + 1.96 * 4.194)
fit1.33.1<- gnm(drowning_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.33.1)
exp(0.49820)
exp(0.49820 - 1.96 * 0.02094)
exp(0.49820 + 1.96 * 0.02094)

fit1.31.2<- gnm(falls_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.31.2)
exp(0.32926)
exp(0.32926 - 1.96 * 0.04693)
exp(0.32926 + 1.96 * 0.04693)
fit1.32.2<- gnm(falls_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.32.2)
exp(-0.48081)
exp(-0.48081 - 1.96 * 0.06912)
exp(-0.48081 + 1.96 * 0.06912)
fit1.33.2<- gnm(falls_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.33.2)
exp(-0.08072)
exp(-0.08072 - 1.96 * 0.05259)
exp(-0.08072 + 1.96 * 0.05259)

fit1.31.3<- gnm(transport_accidents_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.31.3)
exp(-0.17856)
exp(-0.17856 - 1.96 * 0.04227)
exp(-0.17856 + 1.96 * 0.04227)
fit1.32.3<- gnm(transport_accidents_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.32.3)
exp(-0.31768)
exp(-0.31768 - 1.96 * 0.05375)
exp(-0.31768 + 1.96 * 0.05375)
fit1.33.3<- gnm(transport_accidents_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.33.3)
exp(-0.02682)
exp(-0.02682 - 1.96 * 0.04955)
exp(-0.02682 + 1.96 * 0.04955)

#AD HEAT BY AGE
fit1.41<- gnm(heat_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.41)
exp(0.23908)
exp(0.23908 - 1.96 * 0.03776)
exp(0.23908 + 1.96 * 0.03776)
fit1.42<- gnm(heat_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.42)
exp(0.69315)
exp(0.69315 - 1.96 * 0.04347)
exp(0.69315 + 1.96 * 0.04347)
fit1.43<- gnm(heat_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.43)
exp(0.49820)
exp(0.49820 - 1.96 * 0.04392)
exp(0.49820 + 1.96 * 0.04392)

fit1.41.1<- gnm(heatstroke_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.41.1)
exp(13.37)
exp(13.37 - 1.96 * 2.57)
exp(13.37 + 1.96 * 2.57)
fit1.42.1<- gnm(heatstroke_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.42.1)
exp(13.372)
exp(13.372 - 1.96 * 3.411)
exp(13.372 + 1.96 * 3.411)
fit1.43.1<- gnm(heatstroke_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.43.1)
exp(0.00)
exp(0.00 - 1.96 * 87.22)
exp(0.00 + 1.96 * 87.22)

fit1.41.2<- gnm(dehydration_electrolyte_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.41.2)
exp(0.01675)
exp(0.01675 - 1.96 * 0.04081)
exp(0.01675 + 1.96 * 0.04081)
fit1.42.2<- gnm(dehydration_electrolyte_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.42.2)
exp(0.00)
exp(0.00 - 1.96 * 0.05426)
exp(0.00 + 1.96 * 0.05426)
fit1.43.2<- gnm(dehydration_electrolyte_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.43.2)
exp(0.49820)
exp(0.49820 - 1.96 * 0.04212)
exp(0.49820 + 1.96 * 0.04212)

fit1.41.3<- gnm(renal_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.41.3)
exp(-0.21525)
exp(-0.21525 - 1.96 * 0.05099)
exp(-0.21525 + 1.96 * 0.05099)
fit1.42.3<- gnm(renal_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.42.3)
exp(-0.18168)
exp(-0.18168 - 1.96 * 0.06262)
exp(-0.18168 + 1.96 * 0.06262)
fit1.43.3<- gnm(renal_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.43.3)
exp(0.31539)
exp(0.31539 - 1.96 * 0.03879)
exp(0.31539 + 1.96 * 0.03879)

#AD INFECTIOUS AND PARASITIC BY AGE
fit1.51<- gnm(infectious_parasitic_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.51)
exp(0.10864)
exp(0.10864 - 1.96 * 0.05269)
exp(0.10864 + 1.96 * 0.05269)
fit1.52<- gnm(infectious_parasitic_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.52)
exp(1.29807)
exp(1.29807 - 1.96 * 0.04855)
exp(1.29807 + 1.96 * 0.04855)
fit1.53<- gnm(infectious_parasitic_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.53)
exp(-0.3652)
exp(-0.3652 - 1.96 * 0.0536)
exp(-0.3652 + 1.96 * 0.0536)

fit1.51.1<- gnm(otitis_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.51.1)
exp(-9.268)
exp(-9.268 - 1.96 * 4.333)
exp(-9.268 + 1.96 * 4.333)
fit1.52.1<- gnm(otitis_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.52.1)
exp(-10.29)
exp(-10.29 - 1.96 * 4.98)
exp(-10.29 + 1.96 * 4.98)
fit1.53.1<- gnm(otitis_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.53.1)
exp(-9.221)
exp(-9.221 - 1.96 * 3.673)
exp(-9.221 + 1.96 * 3.673)

fit1.51.2<- gnm(lower_respiratory_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.51.2)
exp(0.45667)
exp(0.45667 - 1.96 * 0.05029)
exp(0.45667 + 1.96 * 0.05029)
fit1.52.2<- gnm(lower_respiratory_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.52.2)
exp(-0.22039)
exp(-0.22039 - 1.96 * 0.06658)
exp(-0.22039 + 1.96 * 0.06658)
fit1.53.2<- gnm(lower_respiratory_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.53.2)
exp(1.563)
exp(1.563 - 1.96 * 0.058)
exp(1.563 + 1.96 * 0.058)

fit1.51.3<- gnm(bacterial_enteritis_age_0_4 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.51.3)
exp(-0.63334)
exp(-0.63334 - 1.96 * 0.07779)
exp(-0.63334 + 1.96 * 0.07779)
fit1.52.3<- gnm(bacterial_enteritis_age_5_12 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.52.3)
exp(1.61707)
exp(1.61707 - 1.96 * 0.04574)
exp(1.61707 + 1.96 * 0.04574)
fit1.53.3<- gnm(bacterial_enteritis_age_13_18 ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.53.3)
exp(-0.79795)
exp(-0.79795 - 1.96 * 0.05135)
exp(-0.79795 + 1.96 * 0.05135)

############################
#AD ALL CAUSE BY SEX
fit1.011<- gnm(allcause_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.011)
exp(0.08079)
exp(0.08079 - 1.96 * 0.03903)
exp(0.08079 + 1.96 * 0.03903)
fit1.012<- gnm(allcause_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.012)
exp(0.006002)
exp(0.006002 - 1.96 * 0.041627)
exp(0.006002 + 1.96 * 0.041627)

#AD RESPIRATORY BY SEX
fit1.021<- gnm(respiratory_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.021)
exp(0.2316)
exp(0.2316 - 1.96 * 0.0524)
exp(0.2316 + 1.96 * 0.0524)
fit1.022<- gnm(respiratory_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.022)
exp(0.23892)
exp(0.23892 - 1.96 * 0.05349)
exp(0.23892 + 1.96 * 0.05349)

fit1.021.1<- gnm(asthma_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.021.1)
exp(0.29040)
exp(0.29040 - 1.96 * 0.05712)
exp(0.29040 + 1.96 * 0.05712)
fit1.022.1<- gnm(asthma_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.022.1)
exp(0.1944)
exp(0.1944 - 1.96 * 0.0594)
exp(0.1944 + 1.96 * 0.0594)

#AD INJURY BY SEX
fit1.031<- gnm(injury_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.031)
exp(-0.33362)
exp(-0.33362 - 1.96 * 0.05323)
exp(-0.33362 + 1.96 * 0.05323)
fit1.032<- gnm(injury_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.032)
exp(0.15607)
exp(0.15607 - 1.96 * 0.04842)
exp(0.15607 + 1.96 * 0.04842)

fit1.031.1<- gnm(drowning_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.031.1)
exp(-9.283)
exp(-9.283 - 1.96 * 5.904)
exp(-9.283 + 1.96 * 5.904)
fit1.032.1<- gnm(drowning_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.032.1)
exp(0.49820)
exp(0.49820 - 1.96 * 0.02886)
exp(0.49820 + 1.96 * 0.02886)

fit1.031.2<- gnm(falls_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.031.2)
exp(-0.1498)
exp(-0.1498 - 1.96 * 0.0565)
exp(-0.1498 + 1.96 * 0.0565)
fit1.032.2<- gnm(falls_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.032.2)
exp(0.04109)
exp(0.04109 - 1.96 * 0.05448)
exp(0.04109 + 1.96 * 0.05448)

fit1.031.3<- gnm(transport_accidents_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.031.3)
exp(-0.43360 )
exp(-0.43360  - 1.96 * 0.05508)
exp(-0.43360  + 1.96 * 0.05508)
fit1.032.3<- gnm(transport_accidents_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.032.3)
exp(0.40547)
exp(0.40547 - 1.96 * 0.04348)
exp(0.40547 + 1.96 * 0.04348)

#AD HEAT BY SEX
fit1.041<- gnm(heat_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.041)
exp(0.56412)
exp(0.56412 - 1.96 * 0.04278)
exp(0.56412 + 1.96 * 0.04278)
fit1.042<- gnm(heat_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.042)
exp(0.20165)
exp(0.20165 - 1.96 * 0.03716)
exp(0.20165 + 1.96 * 0.03716)

fit1.041.1<- gnm(heatstroke_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.041.1)
exp(0.00)
exp(0.00 - 1.96 * 85.99)
exp(0.00 + 1.96 * 85.99)
fit1.042.1<- gnm(heatstroke_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.042.1)
exp(13.38)
exp(13.38 - 1.96 * 2.73)
exp(13.38 + 1.96 * 2.73)

fit1.041.2<- gnm(dehydration_electrolyte_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.041.2)
exp(0.56412)
exp(0.56412 - 1.96 * 0.04222)
exp(0.56412 + 1.96 * 0.04222)
fit1.042.2<- gnm(dehydration_electrolyte_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.042.2)
exp(-0.48891)
exp(-0.48891 - 1.96 * 0.04816)
exp(-0.48891 + 1.96 * 0.04816)

fit1.041.3<- gnm(renal_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.041.3)
exp(-0.30388)
exp(-0.30388 - 1.96 * 0.05399)
exp(-0.30388 + 1.96 * 0.05399)
fit1.042.3<- gnm(renal_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.042.3)
exp(0.07603)
exp(0.07603 - 1.96 * 0.04629)
exp(0.07603 + 1.96 * 0.04629)

#AD INFECTIOUS AND PARASITIC BY SEX
fit1.051<- gnm(infectious_parasitic_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.051)
exp(0.31687)
exp(0.31687 - 1.96 * 0.04573)
exp(0.31687 + 1.96 * 0.04573)
fit1.052<- gnm(infectious_parasitic_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.052)
exp(0.29636)
exp(0.29636 - 1.96 * 0.05694)
exp(0.29636 + 1.96 * 0.05694)

fit1.051.1<- gnm(otitis_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.051.1)
exp(-9.268)
exp(-9.268 - 1.96 * 3.946)
exp(-9.268 + 1.96 * 3.946)
fit1.052.1<- gnm(otitis_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.052.1)
exp(-9.273)
exp(-9.273 - 1.96 * 3.848)
exp(-9.273 + 1.96 * 3.848)

fit1.051.2<- gnm(lower_respiratory_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.051.2)
exp(0.34870)
exp(0.34870 - 1.96 * 0.05223)
exp(0.34870 + 1.96 * 0.05223)
fit1.052.2<- gnm(lower_respiratory_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.052.2)
exp(0.49820)
exp(0.49820 - 1.96 * 0.05318)
exp(0.49820 + 1.96 * 0.05318)

fit1.051.3<- gnm(bacterial_enteritis_male ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.051.3)
exp(0.19530)
exp(0.19530 - 1.96 * 0.05013)
exp(0.19530 + 1.96 * 0.05013)
fit1.052.3<- gnm(bacterial_enteritis_female ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.052.3)
exp(0.13222)
exp(0.13222 - 1.96 * 0.06102)
exp(0.13222 + 1.96 * 0.06102)

#EFFECT ESTIMATE (RR)
#Exp(Effect estimate (Standard error * 1.96))
# #Exp((effect estimate)+/-(standard error*1.96))
# exp(0.02854)
# exp(0.02854 - 1.96 * 0.02262)
# exp(0.02854 + 1.96 * 0.02262)

########################################################################################################################
#CONDITIONAL POISSON REGRESSION HOSPITAL 975
AD 975

#data <- read.dta("df.ad.99")
#summary(data)

# GENERATE DOW AND STRATUM
df.ad.975$month   <- as.factor(months(df.ad.975$date))
df.ad.975$year    <- as.factor(format(df.ad.975$date, format="%Y") )
df.ad.975$dow     <- as.factor(weekdays(df.ad.975$date))
df.ad.975$fsa     <- as.factor(df.ad.975$fsa)
df.ad.975$stratum <- as.factor(df.ad.975$year:df.ad.975$month:df.ad.975$dow:df.ad.975$fsa)

df.ad.975 <- data[order(df.ad.975$date),]

names(df.ad.975)
glimpse(df.ad.975)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_99, data=df.ad.99, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
fit2.1 <- gnm(allcause ~ hw, data=df.ad.975, family=quasipoisson, eliminate=factor(stratum))
summary(fit2.1) 
exp(0.03683)
exp(0.03683 - 1.96 * 0.01723)
exp(0.03683 + 1.96 * 0.01723)

fit2.2 <- gnm(respiratory ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.2) 
exp(0.16412)
exp(0.16412 - 1.96 * 0.02595)
exp(0.16412 + 1.96 * 0.02595)
fit2.2.1 <- gnm(asthma ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.2.1) 
exp(0.07822)
exp(0.07822 - 1.96 * 0.02988)
exp(0.07822 + 1.96 * 0.02988)


fit2.3 <- gnm(injury ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.3) 
exp(-0.06809)
exp(-0.06809 - 1.96 * 0.02415)
exp(-0.06809 + 1.96 * 0.02415)
fit2.3.1 <- gnm(drowning ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.3.1) 
exp(-0.12273)
exp(-0.12273 - 1.96 * 0.02279)
exp(-0.12273 + 1.96 * 0.02279)
fit2.3.2 <- gnm(falls ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.3.2) 
exp(-0.03263)
exp(-0.03263 - 1.96 * 0.02586)
exp(-0.03263 + 1.96 * 0.02586)
fit2.3.3 <- gnm(transport_accidents ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.3.3) 
exp(-0.13095)
exp(-0.13095 - 1.96 * 0.02506 )
exp(-0.13095 + 1.96 * 0.02506 )

fit2.4 <- gnm(heat ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.4) 
exp(0.28042)
exp(0.28042 - 1.96 * 0.02236)
exp(0.28042 + 1.96 * 0.02236)
fit2.4.1 <- gnm(heatstroke ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.4.1) 
exp(13.385)
exp(13.385 - 1.96 * 2.482)
exp(13.385 + 1.96 * 2.482)
fit2.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.4.2) 
exp(0.00000)
exp(0.00000 - 1.96 * 0.02459)
exp(0.00000 + 1.96 * 0.02459)
fit2.4.3 <- gnm(renal ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.4.3) 
exp(0.05549)
exp(0.05549 - 1.96 * 0.02394)
exp(0.05549 + 1.96 * 0.02394)


fit2.5 <- gnm(infectious_parasitic ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.5)
exp(0.09742)
exp(0.09742 - 1.96 *  0.02413)
exp(0.09742 + 1.96 *  0.02413)
fit2.5.1 <- gnm(otitis ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.5.1)
exp(-0.71121)
exp(-0.71121 - 1.96 * 0.02912)
exp(-0.71121 + 1.96 * 0.02912)
fit2.5.2 <- gnm(lower_respiratory ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.5.2)
exp(0.29264)
exp(0.29264 - 1.96 * 0.02675)
exp(0.29264 + 1.96 * 0.02675)
fit2.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ad.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit2.5.3)
exp(0.16740)
exp(0.16740 - 1.96 * 0.02588)
exp(0.16740 + 1.96 * 0.02588)

#EFFECT ESTIMATE (RR)
#Exp(Effect estimate (Standard error * 1.96))
exp(0.02854)
exp(0.02854 - 1.96 * 0.02262)
exp(0.02854 + 1.96 * 0.02262)


########################################################################################################################
#CONDITIONAL POISSON REGRESSION HOSPITAL 95
AD 95

#data <- read.dta("df.ad.95")
#summary(data)

# GENERATE DOW AND STRATUM
df.ad.95$month   <- as.factor(months(df.ad.95$date))
df.ad.95$year    <- as.factor(format(df.ad.95$date, format="%Y") )
df.ad.95$dow     <- as.factor(weekdays(df.ad.95$date))
df.ad.95$fsa     <- as.factor(df.ad.95$fsa)
df.ad.95$stratum <- as.factor(df.ad.95$year:df.ad.95$month:df.ad.95$dow:df.ad.95$fsa)

df.ad.95 <- data[order(df.ad.95$date),]

names(df.ad.95)
glimpse(df.ad.95)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_99, data=df.ad.99, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 


# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
fit3.1 <- gnm(allcause ~ hw, data=df.ad.95, family=quasipoisson, eliminate=factor(stratum))
summary(fit3.1) 
exp(0.006996)
exp(0.006996 - 1.96 * 0.011433)
exp(0.006996 + 1.96 * 0.011433)

fit3.2 <- gnm(respiratory ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.2) 
exp(0.009661)
exp(0.009661 - 1.96 * 0.017404)
exp(0.009661 + 1.96 * 0.017404)
fit3.2.1 <- gnm(asthma ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.2.1) 
exp(-0.03576)
exp(-0.03576 - 1.96 * 0.01918)
exp(-0.03576 + 1.96 * 0.01918)

fit3.3 <- gnm(injury ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.3) 
exp(-0.01146)
exp(-0.01146 - 1.96 * 0.01561)
exp(-0.01146 + 1.96 * 0.01561)
fit3.3.1 <- gnm(drowning ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.3.1) 
exp(0.68406 )
exp(0.68406  - 1.96 * 0.01192)
exp(0.68406  + 1.96 * 0.01192)
fit3.3.2 <- gnm(falls ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.3.2) 
exp(0.02572)
exp(0.02572 - 1.96 * 0.01660)
exp(0.02572 + 1.96 * 0.01660)
fit3.3.3 <- gnm(transport_accidents ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.3.3) 
exp(-0.05502)
exp(-0.05502 - 1.96 * 0.01625)
exp(-0.05502 + 1.96 * 0.01625)

fit3.4 <- gnm(heat ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.4) 
exp(0.34133)
exp(0.34133 - 1.96 * 0.01496)
exp(0.34133 + 1.96 * 0.01496)
fit3.4.1 <- gnm(heatstroke ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.4.1) 
exp(3.04452)
exp(3.04452 - 1.96 * 0.01501)
exp(3.04452 + 1.96 * 0.01501)
fit3.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.4.2) 
exp(0.21620)
exp(0.21620 - 1.96 * 0.01555)
exp(0.21620 + 1.96 * 0.01555)
fit3.4.3 <- gnm(renal ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.4.3) 
exp(0.12246)
exp(0.12246 - 1.96 * 0.01572)
exp(0.12246 + 1.96 * 0.01572)

fit3.5 <- gnm(infectious_parasitic ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.5)
exp(0.01694)
exp(0.01694 - 1.96 * 0.01653)
exp(0.01694 + 1.96 * 0.01653)
fit3.5.1 <- gnm(otitis ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.5.1)
exp(-0.64553)
exp(-0.64553 - 1.96 * 0.02073)
exp(-0.64553 + 1.96 * 0.02073)
fit3.5.2 <- gnm(lower_respiratory ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.5.2)
exp(0.07240)
exp(0.07240 - 1.96 * 0.01844)
exp(0.07240 + 1.96 * 0.01844)
fit3.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ad.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit3.5.3)
exp(0.09148)
exp(0.09148 - 1.96 * 0.01772)
exp(0.09148 + 1.96 * 0.01772)

#EFFECT ESTIMATE (RR)
#Exp(Effect estimate (Standard error * 1.96))
exp(0.02854)
exp(0.02854 - 1.96 * 0.02262)
exp(0.02854 + 1.96 * 0.02262)




########################################################################################################################
########################################################################################################################
#CONDITIONAL POISSON REGRESSION ED 99
ED 99

#data <- read.dta("df.ed.99")
#summary(data)

# GENERATE DOW AND STRATUM
df.ed.99$month   <- as.factor(months(df.ed.99$date))
df.ed.99$year    <- as.factor(format(df.ed.99$date, format="%Y") )
df.ed.99$dow     <- as.factor(weekdays(df.ed.99$date))
df.ed.99$fsa     <- as.factor(df.ed.99$fsa)
df.ed.99$stratum <- as.factor(df.ed.99$year:df.ed.99$month:df.ed.99$dow:df.ed.99$fsa)

df.ed.99 <- data[order(df.ed.99$date),]

names(df.ed.99)
glimpse(df.ed.99)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_99, data=df.ad.99, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
dim(df.ed.99)

fit4.1<- gnm(allcause ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.1)
exp(-0.018279)
exp(-0.018279 - 1.96 * 0.009786 )
exp(-0.018279 + 1.96 * 0.009786 )

fit4.2 <- gnm(respiratory ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.2) 
exp(-0.03525)
exp(-0.03525 - 1.96 * 0.02514)
exp(-0.03525 + 1.96 * 0.02514)
fit4.2.1 <- gnm(asthma ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.2.1) 
exp(0.16296)
exp(0.16296 - 1.96 * 0.04615)
exp(0.16296 + 1.96 * 0.04615)

fit4.3 <- gnm(injury ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3) 
exp(-0.06433)
exp(-0.06433  - 1.96 * 0.01734)
exp(-0.06433  + 1.96 * 0.01734)
fit4.3.1 <- gnm(drowning ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.1) 
exp(0.1408)
exp(0.1408  - 1.96 * 0.0461)
exp(0.1408  + 1.96 * 0.0461)
fit4.3.2 <- gnm(falls ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.2) 
exp(-0.09822)
exp(-0.09822  - 1.96 * 0.03013)
exp(-0.09822  + 1.96 * 0.03013)
fit4.3.3 <- gnm(transport_accidents ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.3) 
exp(-0.14156)
exp(-0.14156  - 1.96 * 0.03865)
exp(-0.14156  + 1.96 * 0.03865)

fit4.4 <- gnm(heat ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4) 
exp(1.13493)
exp(1.13493 - 1.96 * 0.02955)
exp(1.13493 + 1.96 * 0.02955)
fit4.4.1 <- gnm(heatstroke ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.1) 
exp(1.93202)
exp(1.93202 - 1.96 * 0.02323)
exp(1.93202 + 1.96 * 0.02323)
fit4.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.2) 
exp(0.29969)
exp(0.29969 - 1.96 * 0.03982)
exp(0.29969 + 1.96 * 0.03982)
fit4.4.3 <- gnm(renal ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.3) 
exp(0.04399)
exp(0.04399 - 1.96 * 0.04163)
exp(0.04399 + 1.96 * 0.04163)

fit4.5 <- gnm(infectious_parasitic ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5)
exp(-0.002931)
exp(-0.002931  - 1.96 *  0.034790)
exp(-0.002931  + 1.96 *  0.034790)
fit4.5.1 <- gnm(otitis ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.1)
exp(0.01602)
exp(0.01602  - 1.96 *  0.02884)
exp(0.01602  + 1.96 *  0.02884)
fit4.5.2 <- gnm(lower_respiratory ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.2)
exp(0.09334)
exp(0.09334  - 1.96 *  0.04786)
exp(0.09334  + 1.96 *  0.04786)
fit4.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.3)
exp(-0.07457)
exp(-0.07457  - 1.96 *  0.04866)
exp(-0.07457  + 1.96 *  0.04866)


##########################################################
ED 99 by age and sex

#CONDITIONAL POISSON REGRESSION ED 99 BY AGE AND SEX
library(gnm)
dim(df.ed.99)

#ED ALL CAUSE BY AGE
fit4.11<- gnm(age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.11)
exp(-0.02613)
exp(-0.02613 - 1.96 * 0.01641)
exp(-0.02613 + 1.96 * 0.01641)
fit4.12<- gnm(age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.12)
exp(-0.04523)
exp(-0.04523 - 1.96 * 0.01666)
exp(-0.04523 + 1.96 * 0.01666)
fit4.13<- gnm(age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.13)
exp(0.01541)
exp(0.01541 - 1.96 * 0.01617)
exp(0.01541 + 1.96 * 0.01617)

#ED RESPIRATORY BY AGE
fit4.21<- gnm(respiratory_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.21)
exp(-0.01426)
exp(-0.01426 - 1.96 * 0.03354)
exp(-0.01426 + 1.96 * 0.03354)
fit4.22<- gnm(respiratory_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.22)
exp(-0.11858)
exp(-0.11858 - 1.96 * 0.03645)
exp(-0.11858 + 1.96 * 0.03645)
fit4.23<- gnm(respiratory_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.23)
exp(0.04087)
exp(0.04087 - 1.96 * 0.03767)
exp(0.04087 + 1.96 * 0.03767)

fit4.21.1<- gnm(asthma_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.21.1)
exp(0.26434)
exp(0.26434 - 1.96 * 0.05108)
exp(0.26434 + 1.96 * 0.05108)
fit4.22.1<- gnm(asthma_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.22.1)
exp(-0.003945)
exp(-0.003945 - 1.96 * 0.053576)
exp(-0.003945 + 1.96 * 0.053576)
fit4.23.1<- gnm(asthma_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.23.1)
exp(0.28506)
exp(0.28506 - 1.96 * 0.05086)
exp(0.28506 + 1.96 * 0.05086)

#ED INJURY BY AGE
fit4.31<- gnm(injury_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.31)
exp(-0.06878)
exp(-0.06878 - 1.96 * 0.03124)
exp(-0.06878 + 1.96 * 0.03124)
fit4.32<- gnm(injury_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.32)
exp(-0.13322 )
exp(-0.13322  - 1.96 * 0.02754)
exp(-0.13322  + 1.96 * 0.02754)
fit4.33<- gnm(injury_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.33)
exp(0.00007692)
exp(0.00007692 - 1.96 * 0.02567428)
exp(0.00007692 + 1.96 * 0.02567428)

fit4.31.1<- gnm(drowning_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.31.1)
exp(0.06800)
exp(0.06800 - 1.96 * 0.06146)
exp(0.06800 + 1.96 * 0.06146)
fit4.32.1<- gnm(drowning_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.32.1)
exp(-9.287 )
exp(-9.287  - 1.96 * 4.009)
exp(-9.287  + 1.96 * 4.009)
fit4.33.1<- gnm(drowning_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.33.1)
exp(0.80615)
exp(0.80615 - 1.96 * 0.03263)
exp(0.80615 + 1.96 * 0.03263)

fit4.31.2<- gnm(falls_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.31.2)
exp(-0.07676)
exp(-0.07676 - 1.96 * 0.04008)
exp(-0.07676 + 1.96 * 0.04008)
fit4.32.2<- gnm(falls_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.32.2)
exp(-0.16377)
exp(-0.16377  - 1.96 * 0.04198)
exp(-0.16377  + 1.96 * 0.04198)
fit4.33.2<- gnm(falls_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.33.2)
exp(-0.02790)
exp(-0.02790 - 1.96 * 0.04415)
exp(-0.02790 + 1.96 * 0.04415)

fit4.31.3<- gnm(transport_accidents_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.31.3)
exp(-0.27165)
exp(-0.27165 - 1.96 * 0.05644)
exp(-0.27165 + 1.96 * 0.05644)
fit4.32.3<- gnm(transport_accidents_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.32.3)
exp(-0.24302)
exp(-0.24302  - 1.96 * 0.04667)
exp(-0.24302  + 1.96 * 0.04667)
fit4.33.3<- gnm(transport_accidents_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.33.3)
exp(-0.05766)
exp(-0.05766 - 1.96 * 0.04130)
exp(-0.05766 + 1.96 * 0.04130)

#ED HEAT BY AGE
fit4.41<- gnm(heat_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.41)
exp(0.55373)
exp(0.55373 - 1.96 * 0.03513)
exp(0.55373 + 1.96 * 0.03513)
fit4.42<- gnm(heat_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.42)
exp(1.53261)
exp(1.53261 - 1.96 * 0.02976)
exp(1.53261 + 1.96 * 0.02976)
fit4.43<- gnm(heat_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.43)
exp(1.36501)
exp(1.36501 - 1.96 * 0.02675)
exp(1.36501 + 1.96 * 0.02675)

fit4.41.1<- gnm(heatstroke_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.41.1)
exp(1.45568)
exp(1.45568 - 1.96 * 0.01752)
exp(1.45568 + 1.96 * 0.01752)
fit4.42.1<- gnm(heatstroke_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.42.1)
exp(2.34971)
exp(2.34971 - 1.96 * 0.02808)
exp(2.34971 + 1.96 * 0.02808)
fit4.43.1<- gnm(heatstroke_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.43.1)
exp(1.95310)
exp(1.95310 - 1.96 * 0.02418)
exp(1.95310 + 1.96 * 0.02418)

fit4.41.2<- gnm(dehydration_electrolyte_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.41.2)
exp(-0.02875)
exp(-0.02875 - 1.96 * 0.04701)
exp(-0.02875 + 1.96 * 0.04701)
fit4.42.2<- gnm(dehydration_electrolyte_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.42.2)
exp(0.60592)
exp(0.60592 - 1.96 * 0.03713)
exp(0.60592 + 1.96 * 0.03713)
fit4.43.2<- gnm(dehydration_electrolyte_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.43.2)
exp(0.49820)
exp(0.49820 - 1.96 * 0.03394)
exp(0.49820 + 1.96 * 0.03394)

fit4.41.3<- gnm(renal_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.41.3)
exp(0.22437)
exp(0.22437 - 1.96 * 0.04602)
exp(0.22437 + 1.96 * 0.04602)
fit4.42.3<- gnm(renal_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.42.3)
exp(-0.20907)
exp(-0.20907 - 1.96 * 0.05465)
exp(-0.20907 + 1.96 * 0.05465)
fit4.43.3<- gnm(renal_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.43.3)
exp(0.04336)
exp(0.04336 - 1.96 * 0.04429)
exp(0.04336 + 1.96 * 0.04429)

#ED INFECTIOUS AND PARASITIC BY AGE
fit4.51<- gnm(infectious_parasitic_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.51)
exp(-0.009807)
exp(-0.009807 - 1.96 * 0.040174)
exp(-0.009807 + 1.96 * 0.040174)
fit4.52<- gnm(infectious_parasitic_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.52)
exp(-0.06598)
exp(-0.06598 - 1.96 * 0.04697)
exp(-0.06598 + 1.96 * 0.04697)
fit4.53<- gnm(infectious_parasitic_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.53)
exp(0.11711)
exp(0.11711 - 1.96 * 0.04787)
exp(0.11711 + 1.96 * 0.04787)

fit4.51.1<- gnm(otitis_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.51.1)
exp(-0.02014)
exp(-0.02014 - 1.96 * 0.03904)
exp(-0.02014 + 1.96 * 0.03904)
fit4.52.1<- gnm(otitis_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.52.1)
exp(0.05431)
exp(0.05431 - 1.96 * 0.03174)
exp(0.05431 + 1.96 * 0.03174)
fit4.53.1<- gnm(otitis_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.53.1)
exp(-0.0008678)
exp(-0.0008678 - 1.96 * 0.0344019)
exp(-0.0008678 + 1.96 * 0.0344019)

fit4.51.2<- gnm(lower_respiratory_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.51.2)
exp(0.13096)
exp(0.13096 - 1.96 * 0.05061)
exp(0.13096 + 1.96 * 0.05061)
fit4.52.2<- gnm(lower_respiratory_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.52.2)
exp(-0.10065)
exp(-0.10065 - 1.96 * 0.05556)
exp(-0.10065 + 1.96 * 0.05556)
fit4.53.2<- gnm(lower_respiratory_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.53.2)
exp(0.3601)
exp(0.3601 - 1.96 * 0.0587)
exp(0.3601 + 1.96 * 0.0587)

fit4.51.3<- gnm(bacterial_enteritis_age_0_4 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.51.3)
exp(-0.16183)
exp(-0.16183 - 1.96 * 0.05368)
exp(-0.16183 + 1.96 * 0.05368)
fit4.52.3<- gnm(bacterial_enteritis_age_5_12 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.52.3)
exp(-0.08278)
exp(-0.08278 - 1.96 * 0.05710)
exp(-0.08278 + 1.96 * 0.05710)
fit4.53.3<- gnm(bacterial_enteritis_age_13_18 ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.53.3)
exp(0.24242)
exp(0.24242 - 1.96 * 0.05437)
exp(0.24242 + 1.96 * 0.05437)

###########################
#ED ALL CAUSE BY SEX
fit4.011<- gnm(male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.011)
exp(-0.02337)
exp(-0.02337  - 1.96 * 0.01313)
exp(-0.02337  + 1.96 * 0.01313)
fit4.012<- gnm(female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.012)
exp(-0.01218)
exp(-0.01218 - 1.96 * 0.01401)
exp(-0.01218 + 1.96 * 0.01401)

#ED RESPIRATORY BY SEX
fit4.021<- gnm(respiratory_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.021)
exp(-0.04855)
exp(-0.04855 - 1.96 * 0.03189)
exp(-0.04855 + 1.96 * 0.03189)
fit4.022<- gnm(respiratory_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.022)
exp(-0.02050)
exp(-0.02050 - 1.96 * 0.03212)
exp(-0.02050 + 1.96 * 0.03212)

fit4.021.1<- gnm(asthma_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.021.1)
exp(0.10593)
exp(0.10593 - 1.96 * 0.04987)
exp(0.10593 + 1.96 * 0.04987)
fit4.022.1<- gnm(asthma_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.022.1)
exp(0.25868)
exp(0.25868 - 1.96 * 0.05079)
exp(0.25868 + 1.96 * 0.05079)

#ED INJURY BY SEX
fit4.031<- gnm(injury_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.031)
exp(-0.04145)
exp(-0.04145 - 1.96 * 0.02182)
exp(-0.04145 + 1.96 * 0.02182)
fit4.032<- gnm(injury_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.032)
exp(-0.09790)
exp(-0.09790 - 1.96 * 0.02604)
exp(-0.09790 + 1.96 * 0.02604)

fit4.031.1<- gnm(drowning_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.031.1)
exp(0.59048)
exp(0.59048 - 1.96 * 0.03918)
exp(0.59048 + 1.96 * 0.03918)
fit4.032.1<- gnm(drowning_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.032.1)
exp(-9.276)
exp(-9.276 - 1.96 * 4.442)
exp(-9.276 + 1.96 * 4.442)

fit4.031.2<- gnm(falls_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.031.2)
exp(-0.05833)
exp(-0.05833 - 1.96 * 0.03629)
exp(-0.05833 + 1.96 * 0.03629)
fit4.032.2<- gnm(falls_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.032.2)
exp(-0.15095)
exp(-0.15095 - 1.96 * 0.04009)
exp(-0.15095 + 1.96 * 0.04009)

fit4.031.3<- gnm(transport_accidents_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.031.3)
exp(-0.19249)
exp(-0.19249 - 1.96 * 0.04203)
exp(-0.19249 + 1.96 * 0.04203)
fit4.032.3<- gnm(transport_accidents_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.032.3)
exp(-0.04729)
exp(-0.04729 - 1.96 * 0.04496)
exp(-0.04729 + 1.96 * 0.04496)

#ED HEAT BY SEX
fit4.041<- gnm(heat_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.041)
exp(1.3863)
exp(1.3863 - 1.96 * 0.0312)
exp(1.3863 + 1.96 * 0.0312)
fit4.042<- gnm(heat_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.042)
exp(0.9043)
exp(0.9043 - 1.96 * 0.0289)
exp(0.9043 + 1.96 * 0.0289)

fit4.041.1<- gnm(heatstroke_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.041.1)
exp(2.08712)
exp(2.08712 - 1.96 * 0.02663)
exp(2.08712 + 1.96 * 0.02663)
fit4.042.1<- gnm(heatstroke_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.042.1)
exp(1.81579)
exp(1.81579 - 1.96 * 0.02078)
exp(1.81579 + 1.96 * 0.02078)

fit4.041.2<- gnm(dehydration_electrolyte_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.041.2)
exp(0.81417)
exp(0.81417 - 1.96 * 0.03691)
exp(0.81417 + 1.96 * 0.03691)
fit4.042.2<- gnm(dehydration_electrolyte_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.042.2)
exp(-0.40770)
exp(-0.40770 - 1.96 * 0.04884)
exp(-0.40770 + 1.96 * 0.04884)

fit4.041.3<- gnm(renal_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.041.3)
exp(0.05385)
exp(0.05385 - 1.96 * 0.04837)
exp(0.05385 + 1.96 * 0.04837)
fit4.042.3<- gnm(renal_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.042.3)
exp(0.04205)
exp(0.04205 - 1.96 * 0.04269)
exp(0.04205 + 1.96 * 0.04269)

#ED INFECTIOUS AND PARASITIC BY SEX
fit4.051<- gnm(infectious_parasitic_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.051)
exp(-0.01385)
exp(-0.01385 - 1.96 * 0.04117)
exp(-0.01385 + 1.96 * 0.04117)
fit4.052<- gnm(infectious_parasitic_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.052)
exp(0.008707)
exp(0.008707 - 1.96 * 0.041665)
exp(0.008707 + 1.96 * 0.041665)

fit4.051.1<- gnm(otitis_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.051.1)
exp(-0.07292)
exp(-0.07292 - 1.96 * 0.03500)
exp(-0.07292 + 1.96 * 0.03500)
fit4.052.1<- gnm(otitis_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.052.1)
exp(0.10835)
exp(0.10835 - 1.96 * 0.03257)
exp(0.10835 + 1.96 * 0.03257)

fit4.051.2<- gnm(lower_respiratory_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.051.2)
exp(0.11095)
exp(0.11095 - 1.96 * 0.05136)
exp(0.11095 + 1.96 * 0.05136)
fit4.052.2<- gnm(lower_respiratory_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.052.2)
exp(0.06927)
exp(0.06927 - 1.96 * 0.05322)
exp(0.06927 + 1.96 * 0.05322)

fit4.051.3<- gnm(bacterial_enteritis_male ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.051.3)
exp(-0.12368)
exp(-0.12368 - 1.96 * 0.05407)
exp(-0.12368 + 1.96 * 0.05407)
fit4.052.3<- gnm(bacterial_enteritis_female ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.052.3)
exp(-0.02045)
exp(-0.02045 - 1.96 * 0.05327)
exp(-0.02045 + 1.96 * 0.05327)


########################################################################################################################
########################################################################################################################
########################################################################################################################
#CONDITIONAL POISSON REGRESSION ED 97.5
ED 97.5
#data <- read.dta("df.ad.975")
#summary(data)

# GENERATE DOW AND STRATUM
df.ed.975$month   <- as.factor(months(df.ed.975$date))
df.ed.975$year    <- as.factor(format(df.ed.975$date, format="%Y") )
df.ed.975$dow     <- as.factor(weekdays(df.ed.975$date))
df.ed.975$fsa     <- as.factor(df.ed.975$fsa)
df.ed.975$stratum <- as.factor(df.ed.975$year:df.ed.975$month:df.ed.975$dow:df.ed.975$fsa)

df.ed.975 <- data[order(df.ed.975$date),]

names(df.ed.975)
glimpse(df.ed.975)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_99, data=df.ad.99, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
dim(df.ed.975)

fit5.1<- gnm(allcause ~ hw, data=df.ed.975, family=quasipoisson, eliminate=factor(stratum))
summary(fit5.1)
exp(-0.014550)
exp(-0.014550 - 1.96 * 0.004969 )
exp(-0.014550 + 1.96 * 0.004969 )

fit5.2 <- gnm(respiratory ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.2) 
exp(-0.01301)
exp(-0.01301 - 1.96 * 0.01289)
exp(-0.01301 + 1.96 * 0.01289)
fit5.2.1 <- gnm(asthma ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.2.1) 
exp(0.06970)
exp(0.06970 - 1.96 * 0.02399)
exp(0.06970 + 1.96 * 0.02399)

fit5.3 <- gnm(injury ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.3) 
exp(-0.063377)
exp(-0.063377  - 1.96 * 0.008639)
exp(-0.063377  + 1.96 * 0.008639)
fit5.3.1 <- gnm(drowning ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.3.1) 
exp(-0.15467)
exp(-0.15467  - 1.96 * 0.02481)
exp(-0.15467  + 1.96 * 0.02481)
fit5.3.2 <- gnm(falls ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.3.2) 
exp(-0.09464)
exp(-0.09464  - 1.96 * 0.01473)
exp(-0.09464  + 1.96 * 0.01473)
fit5.3.3 <- gnm(transport_accidents ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.3.3) 
exp(-0.16302 )
exp(-0.16302   - 1.96 * 0.01964)
exp(-0.16302   + 1.96 * 0.01964)

fit5.4 <- gnm(heat ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.4) 
exp(1.15373)
exp(1.15373 - 1.96 * 0.01704)
exp(1.15373 + 1.96 * 0.01704)
fit5.4.1 <- gnm(heatstroke ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.4.1) 
exp(1.95570)
exp(1.95570 - 1.96 * 0.01395)
exp(1.95570 + 1.96 * 0.01395)
fit5.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.4.2) 
exp(0.45274)
exp(0.45274 - 1.96 * 0.02149)
exp(0.45274 + 1.96 * 0.02149)
fit5.4.2 <- gnm(renal ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.4.2) 
exp(0.03874)
exp(0.03874 - 1.96 * 0.02144)
exp(0.03874 + 1.96 * 0.02144)

fit5.5 <- gnm(infectious_parasitic ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.5)
exp(-0.01995)
exp(-0.01995  - 1.96 *  0.01779)
exp(-0.01995  + 1.96 *  0.01779)
fit5.5.1 <- gnm(otitis ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.5.1)
exp(0.07324)
exp(0.07324  - 1.96 *  0.01527)
exp(0.07324  + 1.96 *  0.01527)
fit5.5.2 <- gnm(lower_respiratory ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.5.2)
exp(0.03372)
exp(0.03372  - 1.96 *  0.02415)
exp(0.03372  + 1.96 *  0.02415)
fit5.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ed.975, family=quasipoisson,eliminate=factor(stratum))
summary(fit5.5.3)
exp(-0.02142)
exp(-0.02142   - 1.96 *  0.02351)
exp(-0.02142   + 1.96 *  0.02351)




########################################################################################################################
########################################################################################################################
#CONDITIONAL POISSON REGRESSION ED 95
ED 95

#data <- read.dta("df.ad.975")
#summary(data)

# GENERATE DOW AND STRATUM
df.ed.95$month   <- as.factor(months(df.ed.95$date))
df.ed.95$year    <- as.factor(format(df.ed.95$date, format="%Y") )
df.ed.95$dow     <- as.factor(weekdays(df.ed.95$date))
df.ed.95$fsa     <- as.factor(df.ed.95$fsa)
df.ed.95$stratum <- as.factor(df.ed.95$year:df.ed.95$month:df.ed.95$dow:df.ed.95$fsa)

df.ed.95 <- data[order(df.ed.95$date),]

names(df.ed.95)
glimpse(df.ed.95)

# ALLOW FOR OVERDISPERSION (quasipoisson)
#fit2 <- gnm(allcause ~ tmax_97.5, data=df.ed.95, family=quasipoisson,
#            eliminate=factor(stratum))
#summary(fit2) 

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
library(gnm)
dim(df.ed.95)

fit6.1<- gnm(allcause ~ hw, data=df.ed.95, family=quasipoisson, eliminate=factor(stratum))
summary(fit6.1)
exp(-0.011891)
exp(-0.011891 - 1.96 * 0.003245 )
exp(-0.011891 + 1.96 * 0.003245 )

fit6.2 <- gnm(respiratory ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.2) 
exp(-0.023337)
exp(-0.023337 - 1.96 * 0.008342)
exp(-0.023337 + 1.96 * 0.008342)
fit6.2.1 <- gnm(asthma ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.2.1) 
exp(-0.0001304)
exp(-0.0001304 - 1.96 * 0.0152671)
exp(-0.0001304 + 1.96 * 0.0152671)

fit6.3 <- gnm(injury ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.3) 
exp(-0.051555)
exp(-0.051555  - 1.96 * 0.005609)
exp(-0.051555  + 1.96 * 0.005609)
fit6.3.1 <- gnm(drowning ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.3.1) 
exp(0.26949)
exp(0.26949  - 1.96 * 0.01393)
exp(0.26949  + 1.96 * 0.01393)
fit6.3.2 <- gnm(falls ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.3.2) 
exp(-0.085460)
exp(-0.085460  - 1.96 * 0.009591)
exp(-0.085460  + 1.96 * 0.009591)
fit6.3.3 <- gnm(transport_accidents ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.3.3) 
exp(-0.10494)
exp(-0.10494  - 1.96 * 0.01264)
exp(-0.10494  + 1.96 * 0.01264)

fit6.4 <- gnm(heat ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.4) 
exp(0.96309)
exp(0.96309 - 1.96 * 0.01157)
exp(0.96309 + 1.96 * 0.01157)
fit6.4.1 <- gnm(heatstroke ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.4.1) 
exp(1.644610)
exp(1.644610 - 1.96 * 0.009119)
exp(1.644610 + 1.96 * 0.009119)
fit6.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.4.2) 
exp(0.36612)
exp(0.36612 - 1.96 * 0.01453)
exp(0.36612 + 1.96 * 0.01453)
fit6.4.3 <- gnm(renal ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.4.3) 
exp(0.03298)
exp(0.03298 - 1.96 * 0.01390)
exp(0.03298 + 1.96 * 0.01390)

fit6.5 <- gnm(infectious_parasitic ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.5)
exp(-0.002585)
exp(-0.002585  - 1.96 *  0.011418)
exp(-0.002585  + 1.96 *  0.011418)
fit6.5.1 <- gnm(otitis ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.5.1)
exp(0.05017)
exp(0.05017  - 1.96 *  0.01023)
exp(0.05017  + 1.96 *  0.01023)
fit6.5.2 <- gnm(lower_respiratory ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.5.2)
exp(-0.003039)
exp(-0.003039  - 1.96 *  0.015708)
exp(-0.003039  + 1.96 *  0.015708)
fit6.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ed.95, family=quasipoisson,eliminate=factor(stratum))
summary(fit6.5.3)
exp(0.01634)
exp(0.01634  - 1.96 *  0.01514)
exp(0.01634  + 1.96 *  0.01514)

























########################################################################################################################
########################################################################################################################
########################################################################################################################
#SENSITIVITY ANALYSES WITH 1 AND 2 DAY LAGS

# GENERATE DOW AND STRATUM
df.ad.99$month   <- as.factor(months(df.ad.99$date))
df.ad.99$year    <- as.factor(format(df.ad.99$date, format="%Y") )
df.ad.99$dow     <- as.factor(weekdays(df.ad.99$date))
df.ad.99$fsa     <- as.factor(df.ad.99$fsa)
df.ad.99$stratum <- as.factor(df.ad.99$year:df.ad.99$month:df.ad.99$dow:df.ad.99$fsa)

df.ed.99$month   <- as.factor(months(df.ed.99$date))
df.ed.99$year    <- as.factor(format(df.ed.99$date, format="%Y") )
df.ed.99$dow     <- as.factor(weekdays(df.ed.99$date))
df.ed.99$fsa     <- as.factor(df.ed.99$fsa)
df.ed.99$stratum <- as.factor(df.ed.99$year:df.ed.99$month:df.ed.99$dow:df.ed.99$fsa)


########################################################################################################################
#CONDITIONAL POISSON REGRESSION HOSPITAL 99 lag 1 
#1 day after heatwave

fit7.1<- gnm(allcause ~ hw_lag1, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit7.1)
exp(0.01608)
exp(0.01608 - 1.96 * 0.03506)
exp(0.01608 + 1.96 * 0.03506)

fit7.2 <- gnm(respiratory ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.2) 
exp(0.004572)
exp(0.004572 - 1.96 * 0.056671)
exp(0.004572 + 1.96 * 0.056671)
fit7.2.1 <- gnm(asthma ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.2.1) 
exp(0.0136)
exp(0.0136 - 1.96 * 0.0610)
exp(0.0136 + 1.96 * 0.0610)

fit7.3 <- gnm(injury ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.3) 
exp(-0.11403)
exp(-0.11403 - 1.96 * 0.04771)
exp(-0.11403 + 1.96 * 0.04771)
fit7.3.1 <- gnm(drowning ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.3.1) 
exp(-0.65557)
exp(-0.65557 - 1.96 * 0.04371)
exp(-0.65557 + 1.96 * 0.04371)
fit7.3.2 <- gnm(falls ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.3.2) 
exp(-0.02727)
exp(-0.02727 - 1.96 * 0.05066)
exp(-0.02727 + 1.96 * 0.05066)
fit7.3.3 <- gnm(transport_accidents ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.3.3) 
exp(-0.56201)
exp(-0.56201 - 1.96 * 0.05152)
exp(-0.56201 + 1.96 * 0.05152)

fit7.4 <- gnm(heat ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.4) 
exp(0.62796)
exp(0.62796 - 1.96 * 0.04105)
exp(0.62796 + 1.96 * 0.04105)
fit7.4.1 <- gnm(heatstroke ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.4.1) 
exp(12.382)
exp(12.382 - 1.96 * 3.914)
exp(12.382 + 1.96 * 3.914)
fit7.4.2 <- gnm(dehydration_electrolyte ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.4.2) 
exp(0.49820)
exp(0.49820 - 1.96 * 0.04238)
exp(0.49820 + 1.96 * 0.04238)
fit7.4.3 <- gnm(renal ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.4.3) 
exp(-0.10399)
exp(-0.10399 - 1.96 * 0.05585)
exp(-0.10399 + 1.96 * 0.05585)

fit7.5 <- gnm(infectious_parasitic ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.5)
exp(0.16258)
exp(0.16258  - 1.96 *  0.04765)
exp(0.16258  + 1.96 *  0.04765)
fit7.5.1 <- gnm(otitis ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.5.1)
exp(-0.15005)
exp(-0.15005  - 1.96 *  0.05277)
exp(-0.15005  + 1.96 *  0.05277)
fit7.5.2 <- gnm(lower_respiratory ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.5.2)
exp(0.28125)
exp(0.28125  - 1.96 *  0.05745)
exp(0.28125  + 1.96 *  0.05745)
fit7.5.3 <- gnm(bacterial_enteritis ~ hw_lag1, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit7.5.3)
exp(0.27665)
exp(0.27665  - 1.96 *  0.05118)
exp(0.27665  + 1.96 *  0.05118)


########################################################################################################################
#CONDITIONAL POISSON REGRESSION ED 99 lag 1
#1 day after a heatwave


fit8.1<- gnm(allcause ~ hw_lag1, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit8.1)
exp(-0.002620)
exp(-0.002620 - 1.96 * 0.009628 )
exp(-0.002620 + 1.96 * 0.009628 )

fit8.2 <- gnm(respiratory ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.2) 
exp(-0.05350)
exp(-0.05350 - 1.96 * 0.02486)
exp(-0.05350 + 1.96 * 0.02486)
fit8.2.1 <- gnm(asthma ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.2.1) 
exp(-0.01134)
exp(-0.01134 - 1.96 * 0.04888)
exp(-0.01134 + 1.96 * 0.04888)

fit8.3 <- gnm(injury ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.3) 
exp(-0.07512)
exp(-0.07512  - 1.96 * 0.01724)
exp(-0.07512  + 1.96 * 0.01724)
fit8.3.1 <- gnm(drowning ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.3.1) 
exp(-0.54781)
exp(-0.54781  - 1.96 * 0.04284)
exp(-0.54781  + 1.96 * 0.04284)
fit8.3.2 <- gnm(falls ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.3.2) 
exp(-0.10427)
exp(-0.10427  - 1.96 * 0.02958)
exp(-0.10427  + 1.96 * 0.02958)
fit8.3.3 <- gnm(transport_accidents ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.3.3) 
exp(-0.25488)
exp(-0.25488  - 1.96 * 0.03838)
exp(-0.25488  + 1.96 * 0.03838)

fit8.4 <- gnm(heat ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.4) 
exp(0.89227)
exp(0.89227 - 1.96 * 0.03376)
exp(0.89227 + 1.96 * 0.03376)
fit8.4.1 <- gnm(heatstroke ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.4.1) 
exp(1.47792)
exp(1.47792 - 1.96 * 0.02716)
exp(1.47792 + 1.96 * 0.02716)
fit8.4.2 <- gnm(dehydration_electrolyte ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.4.2) 
exp(0.44292)
exp(0.44292 - 1.96 * 0.04041)
exp(0.44292 + 1.96 * 0.04041)
fit8.4.3 <- gnm(renal ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.4.3) 
exp(0.08148)
exp(0.08148 - 1.96 * 0.04089)
exp(0.08148 + 1.96 * 0.04089)

fit8.5 <- gnm(infectious_parasitic ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.5)
exp(0.003162)
exp(0.003162  - 1.96 *  0.033573)
exp(0.003162  + 1.96 *  0.033573)
fit8.5.1 <- gnm(otitis ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.5.1)
exp(0.17555)
exp(0.17555  - 1.96 *  0.02701)
exp(0.17555  + 1.96 *  0.02701)
fit8.5.2 <- gnm(lower_respiratory ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.5.2)
exp(0.05108)
exp(0.05108  - 1.96 *  0.04753)
exp(0.05108  + 1.96 *  0.04753)
fit8.5.3 <- gnm(bacterial_enteritis ~ hw_lag1, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit8.5.3)
exp(0.09859)
exp(0.09859  - 1.96 *  0.04503)
exp(0.09859  + 1.96 *  0.04503)




########################################################################################################################
#CONDITIONAL POISSON REGRESSION HOSPITAL 99 lag 2
#2 days after heatwave

fit9.1<- gnm(allcause ~ hw_lag2, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit9.1)
exp(0.07387)
exp(0.07387 - 1.96 * 0.03574)
exp(0.07387 + 1.96 * 0.03574)

fit9.2 <- gnm(respiratory ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.2) 
exp(-0.08189)
exp(-0.08189 - 1.96 * 0.05720 )
exp(-0.08189 + 1.96 * 0.05720 )
fit9.2.1 <- gnm(asthma ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.2.1) 
exp(-0.3164)
exp(-0.3164 - 1.96 * 0.0679)
exp(-0.3164 + 1.96 * 0.0679)

fit9.3 <- gnm(injury ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.3) 
exp(-0.24010)
exp(-0.24010 - 1.96 * 0.04882)
exp(-0.24010 + 1.96 * 0.04882)
fit9.3.1 <- gnm(drowning ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.3.1) 
exp(-10.27)
exp(-10.27 - 1.96 * 5.06)
exp(-10.27 + 1.96 * 5.06)
fit9.3.2 <- gnm(falls ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.3.2) 
exp(-0.3728)
exp(-0.3728 - 1.96 * 0.0548)
exp(-0.3728 + 1.96 * 0.0548)
fit9.3.3 <- gnm(transport_accidents ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.3.3) 
exp(-0.85482)
exp(-0.85482 - 1.96 * 0.06016)
exp(-0.85482 + 1.96 * 0.06016)

fit9.4 <- gnm(heat ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.4) 
exp(0.49820 )
exp(0.49820  - 1.96 * 0.05672)
exp(0.49820  + 1.96 * 0.05672)
fit9.4.1 <- gnm(heatstroke ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.4.1) 
exp(12.382 )
exp(12.382  - 1.96 * 3.915)
exp(12.382  + 1.96 * 3.915)
fit9.4.2 <- gnm(dehydration_electrolyte	 ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.4.2) 
exp(0.21800 )
exp(0.21800  - 1.96 * 0.06197)
exp(0.21800  + 1.96 * 0.06197)
fit9.4.3 <- gnm(renal ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.4.3) 
exp(0.44711 )
exp(0.44711  - 1.96 * 0.04763)
exp(0.44711  + 1.96 * 0.04763)

fit9.5 <- gnm(infectious_parasitic ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.5)
exp(0.33042)
exp(0.33042  - 1.96 *  0.05013)
exp(0.33042  + 1.96 *  0.05013)
fit9.5.1 <- gnm(otitis ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.5.1)
exp(0.28768)
exp(0.28768  - 1.96 *  0.05429)
exp(0.28768  + 1.96 *  0.05429)
fit9.5.2 <- gnm(lower_respiratory ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.5.2)
exp(-0.03434)
exp(-0.03434  - 1.96 *  0.06061)
exp(-0.03434  + 1.96 *  0.06061)
fit9.5.3 <- gnm(bacterial_enteritis ~ hw_lag2, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit9.5.3)
exp(0.09594)
exp(0.09594  - 1.96 *  0.05951)
exp(0.09594  + 1.96 *  0.05951)


########################################################################################################################
#CONDITIONAL POISSON REGRESSION ED 99 lag 2
#2 days after a heatwave

fit10.1<- gnm(allcause ~ hw_lag2, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit10.1)
exp(0.011224)
exp(0.011224 - 1.96 * 0.009561)
exp(0.011224 + 1.96 * 0.009561)

fit10.2 <- gnm(respiratory ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.2) 
exp(-0.02691)
exp(-0.02691 - 1.96 * 0.02422)
exp(-0.02691 + 1.96 * 0.02422)
fit10.2.1 <- gnm(asthma ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.2.1) 
exp(-0.10793)
exp(-0.10793 - 1.96 * 0.04906)
exp(-0.10793 + 1.96 * 0.04906)

fit10.3 <- gnm(injury ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.3) 
exp(-0.07834)
exp(-0.07834  - 1.96 * 0.01685)
exp(-0.07834  + 1.96 * 0.01685)
fit10.3.1 <- gnm(drowning ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.3.1) 
exp(0.01633)
exp(0.01633  - 1.96 * 0.03565)
exp(0.01633  + 1.96 * 0.03565)
fit10.3.2 <- gnm(falls ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.3.2) 
exp(-0.10022)
exp(-0.10022  - 1.96 * 0.02946)
exp(-0.10022  + 1.96 * 0.02946)
fit10.3.3 <- gnm(transport_accidents ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.3.3) 
exp(-0.15080)
exp(-0.15080  - 1.96 * 0.03667)
exp(-0.15080  + 1.96 * 0.03667)

fit10.4 <- gnm(heat ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.4) 
exp(0.5787)
exp(0.5787 - 1.96 * 0.0356)
exp(0.5787 + 1.96 * 0.0356)
fit10.4.1 <- gnm(heatstroke ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.4.1) 
exp(0.4258)
exp(0.4258 - 1.96 * 0.0314)
exp(0.4258 + 1.96 * 0.0314)
fit10.4.2 <- gnm(dehydration_electrolyte ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.4.2) 
exp(0.66547)
exp(0.66547 - 1.96 * 0.03813)
exp(0.66547 + 1.96 * 0.03813)
fit10.4.3 <- gnm(renal ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.4.3) 
exp(0.01595)
exp(0.01595 - 1.96 * 0.04110)
exp(0.01595 + 1.96 * 0.04110)

fit10.5 <- gnm(infectious_parasitic ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.5)
exp(-0.02063)
exp(-0.02063  - 1.96 *  0.03239)
exp(-0.02063  + 1.96 *  0.03239)
fit10.5.1 <- gnm(otitis ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.5.1)
exp(0.36759)
exp(0.36759  - 1.96 *  0.02497)
exp(0.36759  + 1.96 *  0.02497)
fit10.5.2 <- gnm(lower_respiratory ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.5.2)
exp(-0.09809)
exp(-0.09809  - 1.96 *  0.04763)
exp(-0.09809  + 1.96 *  0.04763)
fit10.5.3 <- gnm(bacterial_enteritis ~ hw_lag2, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit10.5.3)
exp(-0.04304)
exp(-0.04304  - 1.96 *  0.04558)
exp(-0.04304  + 1.96 *  0.04558)























########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
#CREATING PLOTS

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

########################################################################################################################
#PRIMARY- 99TH % UNSTRATIFIED

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.0498, 1.264188, 1.293524, 0.8727466, 0.930987, 0.8715605, 0.944868, 1.362744, 1.505312, 1.187761),
  boxCILow = c(0.981987, 1.144962, 1.15763, 0.7925941, 0.8361253, 0.7907726, 0.8595059, 1.238658, 1.360516, 1.069835),
  boxCIHigh = c(1.122297, 1.395829, 1.445369, 0.9610047, 1.036611, 0.9606019, 1.038708, 1.49926, 1.665518, 1.318686)
)

dfED <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.981887, 0.965364, 1.17699, 0.9376955, 0.9064495, 0.8680031, 1.044972, 0.9970733, 1.097835, 0.9281425),
  boxCILow = c(0.9632334, 0.9189492, 1.075199, 0.9063621, 0.8544692, 0.8046773, 0.9630935, 0.9313507, 0.9995345, 0.8437124),
  boxCIHigh = c(1.000902, 1.014123, 1.288417, 0.9701121, 0.9615918, 0.9363125, 1.133811, 1.067434, 1.205803, 1.021022)
)

# Plot
combined <- dfAD
combined$Legend <- c("Hospital Admissions")
combined
combined$yAxis <-combined$yAxis-0.1
combined2 <-dfED
combined2$Legend <-c("Emergency Department Visits")
combined2$yAxis <-combined2$yAxis+0.1
comb<-rbind(combined, combined2)

comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.745,0.912)) +
  annotate(geom = "text", y =4, x = 1.6, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("")


########################################################################################################################
#SENS 1- 99TH% UNSTRATFIED LAG1

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADL1 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.01621, 1.004582, 1.013693, 0.8922312, 0.9730985, 0.5700621, 0.9012343, 1.176542, 1.324785, 1.318705),
  boxCILow = c(0.9487238, 0.898972, 0.8994606, 0.812579, 0.8811181, 0.5153087, 0.8077877, 1.071636, 1.183703, 1.19284),
  boxCIHigh = c(1.088497, 1.1226, 1.142433, 0.9796907, 1.074681, 0.6306333, 1.005491, 1.291719, 1.482681, 1.45785)
)

dfEDL1 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9973834, 0.9479059, 0.9887241, 0.9276322, 0.900982, 0.7750095, 1.084892, 1.003167, 1.052407, 1.103614),
  boxCILow = c(0.9787384, 0.9028259, 0.8983956, 0.8968108, 0.8502313, 0.7188484, 1.001336, 0.9392805, 0.9587942, 1.010385),
  boxCIHigh = c(1.016384, 0.995237, 1.088135, 0.9595128, 0.954762, 0.8355583, 1.175419, 1.071399, 1.15516, 1.205445)
)

# Plot
combinedL1 <- dfADL1
combinedL1$Legend <- c("Hospital Admissions")
combinedL1$yAxis <-combinedL1$yAxis-0.15
combined2L1 <-dfEDL1
combined2L1$Legend <-c("ED Visits")
combined2L1$yAxis <-combined2L1$yAxis+0.15
combL1<-rbind(combinedL1, combined2L1)

combL1
str(combL1)
combL1$Legend<-as.factor(combL1$Legend)
fin <- ggplot(combL1, aes(x = boxOdds, y = yAxis, colour=Legend))
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = combL1$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.265,0.9)) +
  annotate(geom = "text", y =4, x = 1.55, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("")


########################################################################################################################
#SENS 2- 99TH% UNSTRATFIED LAG2

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADL2 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.076667, 0.9213733, 0.7287679, 0.7865492, 0.688803, 0.4253597, 1.563786, 1.391552, 0.9662429, 1.100693),
  boxCILow = c(1.003827, 0.8236563, 0.6379573, 0.7147752, 0.6186547, 0.3780482, 1.424406, 1.261328, 0.8580134, 0.9795132),
  boxCIHigh = c(1.154792, 1.030683, 0.832505, 0.8655304, 0.7669053, 0.4785922, 1.716805, 1.535222, 1.088125, 1.236865)
)

dfEDL2 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.011287, 0.9734488, 0.8976904, 0.92465, 0.9046384, 0.8600197, 1.016078, 0.9795813, 0.9065673, 0.9578731),
  boxCILow = c(0.9925126, 0.9283178, 0.8153909, 0.8946113, 0.8538826, 0.8003764, 0.9374367, 0.9193261, 0.8257652, 0.8760109),
  boxCIHigh = c(1.030417, 1.020774, 0.9882966, 0.9556973, 0.9584112, 0.9241076, 1.101316, 1.043786, 0.995276, 1.047385)
)

# Plot
combinedL2 <- dfADL2
combinedL2$Legend <- c("Hospital Admissions")
combinedL2$yAxis <-combinedL2$yAxis-0.15
combined2L2 <-dfEDL2
combined2L2$Legend <-c("ED Visits")
combined2L2$yAxis <-combined2L2$yAxis+0.15
combL2<-rbind(combinedL2, combined2L2)

combL2
str(combL2)
combL2$Legend<-as.factor(combL2$Legend)
fin <- ggplot(combL2, aes(x = boxOdds, y = yAxis, colour=Legend))
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = combL2$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.265,0.89)) +
  annotate(geom = "text", y =4, x = 1.8, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("")



########################################################################################################################
########################################################################################################################
########################################################################################################################
#AD Lag 0,1,2

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADL0 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.0498, 1.264188, 1.293524, 0.8727466, 0.930987, 0.8715605, 0.944868, 1.362744, 1.505312, 1.187761),
  boxCILow = c(0.981987, 1.144962, 1.15763, 0.7925941, 0.8361253, 0.7907726, 0.8595059, 1.238658, 1.360516, 1.069835),
  boxCIHigh = c(1.122297, 1.395829, 1.445369, 0.9610047, 1.036611, 0.9606019, 1.038708, 1.49926, 1.665518, 1.318686)
)

dfADL1 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.01621, 1.004582, 1.013693, 0.8922312, 0.9730985, 0.5700621, 0.9012343, 1.176542, 1.324785, 1.318705),
  boxCILow = c(0.9487238, 0.898972, 0.8994606, 0.812579, 0.8811181, 0.5153087, 0.8077877, 1.071636, 1.183703, 1.19284),
  boxCIHigh = c(1.088497, 1.1226, 1.142433, 0.9796907, 1.074681, 0.6306333, 1.005491, 1.291719, 1.482681, 1.45785)
)

dfADL2 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.076667, 0.9213733, 0.7287679, 0.7865492, 0.688803, 0.4253597, 1.563786, 1.391552, 0.9662429, 1.100693),
  boxCILow = c(1.003827, 0.8236563, 0.6379573, 0.7147752, 0.6186547, 0.3780482, 1.424406, 1.261328, 0.8580134, 0.9795132),
  boxCIHigh = c(1.154792, 1.030683, 0.832505, 0.8655304, 0.7669053, 0.4785922, 1.716805, 1.535222, 1.088125, 1.236865)
)

# Plot
combined <- dfADL0
combined$Legend <- c("Lag0")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfADL1
combined2$Legend <-c("Lag1")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfADL2
combined3$Legend <-c("Lag2")
combined3$yAxis <-combined3$yAxis-0.2

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = ADLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  theme(legend.position = c(01.3,0.87)) +
  annotate(geom = "text", y =4, x = 1.75, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("Hospital Admissions")

########################################################################################################################
#ED Lag 0,1,2

# Create labels
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfEDL0 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.981887, 0.965364, 1.17699, 0.9376955, 0.9064495, 0.8680031, 1.044972, 0.9970733, 1.097835, 0.9281425),
  boxCILow = c(0.9632334, 0.9189492, 1.075199, 0.9063621, 0.8544692, 0.8046773, 0.9630935, 0.9313507, 0.9995345, 0.8437124),
  boxCIHigh = c(1.000902, 1.014123, 1.288417, 0.9701121, 0.9615918, 0.9363125, 1.133811, 1.067434, 1.205803, 1.021022)
)

dfEDL1 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9973834, 0.9479059, 0.9887241, 0.9276322, 0.900982, 0.7750095, 1.084892, 1.003167, 1.052407, 1.103614),
  boxCILow = c(0.9787384, 0.9028259, 0.8983956, 0.8968108, 0.8502313, 0.7188484, 1.001336, 0.9392805, 0.9587942, 1.010385),
  boxCIHigh = c(1.016384, 0.995237, 1.088135, 0.9595128, 0.954762, 0.8355583, 1.175419, 1.071399, 1.15516, 1.205445)
)

dfEDL2 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.011287, 0.9734488, 0.8976904, 0.92465, 0.9046384, 0.8600197, 1.016078, 0.9795813, 0.9065673, 0.9578731),
  boxCILow = c(0.9925126, 0.9283178, 0.8153909, 0.8946113, 0.8538826, 0.8003764, 0.9374367, 0.9193261, 0.8257652, 0.8760109),
  boxCIHigh = c(1.030417, 1.020774, 0.9882966, 0.9556973, 0.9584112, 0.9241076, 1.101316, 1.043786, 0.995276, 1.047385)
)

# Plot
combined <- dfEDL0
combined$Legend <- c("Lag0")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfEDL1
combined2$Legend <-c("Lag1")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfEDL2
combined3$Legend <-c("Lag2")
combined3$yAxis <-combined3$yAxis-0.2

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  theme(legend.position = c(0.87,0.87)) +
  annotate(geom = "text", y =4, x = 1.55, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("ED visits")


########################################################################################################################
#SENS 3- 97.5TH% UNSTRATFIED

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD97.5 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.037517 , 1.178356, 1.081361, 0.9341764, 0.9678966, 0.8772616, 1.057058, 1.102323, 1.33996, 1.182227),
  boxCILow = c(1.003064, 1.119921, 1.01985, 0.8909883, 0.9200607, 0.8352137, 1.008604, 1.051403, 1.271516, 1.123754),
  boxCIHigh = c(1.073153, 1.23984, 1.146581, 0.9794579, 1.01822, 0.9214264, 1.10784, 1.15571, 1.412089, 1.243742)
)

dfED97.5 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9855553, 0.9870743, 1.072186, 0.9385896, 0.9097004, 0.8495742, 1.0395, 0.9802477, 1.034295, 0.9788078),
  boxCILow = c(0.9760034, 0.9624488, 1.022939, 0.9228307, 0.8838121, 0.8174918, 0.996723, 0.946657, 0.9864783, 0.9347281),
  boxCIHigh = c(0.9952008, 1.01233, 1.123805, 0.9546175, 0.9363469, 0.8829157, 1.084113, 1.01503, 1.084429, 1.024966)
)

# Plot
combined97.5 <- dfAD97.5
combined97.5$Legend <- c("Hospital Admissions")
combined97.5$yAxis <-combined97.5$yAxis-0.1
combined297.5 <-dfED97.5
combined297.5$Legend <-c("ED Visits")
combined297.5$yAxis <-combined297.5$yAxis+0.1
comb97.5<-rbind(combined97.5, combined297.5)

comb97.5
str(comb97.5)
comb97.5$Legend<-as.factor(comb97.5$Legend)
fin <- ggplot(comb97.5, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb97.5$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.8,0.92)) +
  annotate(geom = "text", y =4, x = 1.5, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("")

########################################################################################################################
#SENS 4- 95TH% UNSTRATFIED

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD95 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.007021, 1.009708, 0.9648718, 0.9886054, 1.026054, 0.9464662, 1.130274, 1.017084, 1.075085, 1.095795),
  boxCILow = c(0.9847055, 0.9758457, 0.9292729, 0.9588165, 0.9932072, 0.9167963, 1.09598, 0.9846601, 1.036923, 1.05839),
  boxCIHigh = c(1.029841, 1.044745, 1.001834, 1.01932, 1.059986, 0.9770964, 1.165641, 1.050576, 1.114652, 1.134522)
)

dfED95 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9881794, 0.9769332, 0.9998696, 0.9497514, 0.9180899, 0.9003785, 1.03353, 0.9974183, 0.9969656, 1.016474),
  boxCILow = c(0.9819143, 0.9610899, 0.9703932, 0.9393674, 0.9009925, 0.8783463, 1.005752, 0.9753448, 0.966739, 0.9867541),
  boxCIHigh = c(0.9944845, 0.9930377, 1.030241, 0.9602502, 0.9355117, 0.9229635, 1.062074, 1.019991, 1.028137, 1.047089)
)

# Plot
combined95 <- dfAD95
combined95$Legend <- c("Hospital Admissions")
combined95$yAxis <-combined95$yAxis-0.1
combined295 <-dfED95
combined295$Legend <-c("ED Visits")
combined295$yAxis <-combined295$yAxis+0.1
comb95<-rbind(combined95, combined295)

comb95
str(comb95)
comb95$Legend<-as.factor(comb95$Legend)
fin <- ggplot(comb95, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb95$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,2,0.25) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.72,0.915)) +
  annotate(geom = "text", y =4, x = 1.5, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("")

########################################################################################################################
########################################################################################################################
########################################################################################################################
#AD Lag 0 (99%, 97.5%, 95%)

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD99 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.0498, 1.264188, 1.293524, 0.8727466, 0.930987, 0.8715605, 0.944868, 1.362744, 1.505312, 1.187761),
  boxCILow = c(0.981987, 1.144962, 1.15763, 0.7925941, 0.8361253, 0.7907726, 0.8595059, 1.238658, 1.360516, 1.069835),
  boxCIHigh = c(1.122297, 1.395829, 1.445369, 0.9610047, 1.036611, 0.9606019, 1.038708, 1.49926, 1.665518, 1.318686)
)

dfAD975 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.037517, 1.178356, 1.081361, 0.9341764, 0.9678966, 0.8772616, 1.057058, 1.102323, 1.33996, 1.182227),
  boxCILow = c(1.003064, 1.119921, 1.01985, 0.8909883, 0.9200607, 0.8352137, 1.008604, 1.051403, 1.271516, 1.123754),
  boxCIHigh = c(1.073153, 1.23984, 1.146581, 0.9794579, 1.01822, 0.9214264, 1.10784, 1.15571, 1.412089, 1.243742)
)

dfAD95 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.007021, 1.009708, 0.9648718, 0.9886054, 1.026054, 0.9464662, 1.130274, 1.017084, 1.075085, 1.095795),
  boxCILow = c(0.9847055, 0.9758457, 0.9292729, 0.9588165, 0.9932072, 0.9167963, 1.09598, 0.9846601, 1.036923, 1.05839),
  boxCIHigh = c(1.029841, 1.044745, 1.001834, 1.01932, 1.059986, 0.9770964, 1.165641, 1.050576, 1.114652, 1.134522)
)
# Plot
combined <- dfAD99
combined$Legend <- c("99th")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfAD975
combined2$Legend <-c("97.5th")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfAD95
combined3$Legend <-c("95th")
combined3$yAxis <-combined3$yAxis-0.2

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = ADLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  theme(legend.position = c(1.87,0.87)) +
  annotate(geom = "text", y =4, x = 1.75, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("Hospital Admissions")

########################################################################################################################
#ED Lag 0 (99%, 97.5%, 95%)

# Create labels
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfED99 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.981887, 0.965364, 1.17699, 0.9376955, 0.9064495, 0.8680031, 1.044972, 0.9970733, 1.097835, 0.9281425),
  boxCILow = c(0.9632334, 0.9189492, 1.075199, 0.9063621, 0.8544692, 0.8046773, 0.9630935, 0.9313507, 0.9995345, 0.8437124),
  boxCIHigh = c(1.000902, 1.014123, 1.288417, 0.9701121, 0.9615918, 0.9363125, 1.133811, 1.067434, 1.205803, 1.021022)
)

dfED975 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9855553, 0.9870743, 1.072186, 0.9385896, 0.9097004, 0.8495742, 1.0395, 0.9802477, 1.034295, 0.9788078),
  boxCILow = c(0.9760034, 0.9624488, 1.022939, 0.9228307, 0.8838121, 0.8174918, 0.996723, 0.946657, 0.9864783, 0.9347281),
  boxCIHigh = c(0.9952008, 1.01233, 1.123805, 0.9546175, 0.9363469, 0.8829157, 1.084113, 1.01503, 1.084429, 1.024966)
)

dfED95 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9881794, 0.9769332, 0.9998696, 0.9497514, 0.9180899, 0.9003785, 1.03353, 0.9974183, 0.9969656, 1.016474),
  boxCILow = c(0.9819143, 0.9610899, 0.9703932, 0.9393674, 0.9009925, 0.8783463, 1.005752, 0.9753448, 0.966739, 0.9867541),
  boxCIHigh = c(0.9944845, 0.9930377, 1.030241, 0.9602502, 0.9355117, 0.9229635, 1.062074, 1.019991, 1.028137, 1.047089)
)

# Plot
combined <- dfED99
combined$Legend <- c("99th")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfED975
combined2$Legend <-c("97.5th")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfED95
combined3$Legend <-c("95th")
combined3$yAxis <-combined3$yAxis-0.2

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  theme(legend.position = c(0.87,0.87)) +
  annotate(geom = "text", y =4, x = 1.5, label =" ", 
           size = 3.5, hjust = 0) + ggtitle("ED visits")


########################################################################################################################
########################################################################################################################
########################################################################################################################
# Plots for age and sex stratification 99%


########################################################################################################################
# 99 0-4

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD04 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.051892, 1.187381, 1.377858, 1.134668, 1.389939, 0.8364739, 0.8063398, 1.114761, 1.578808, 0.5308159),
  boxCILow = c(0.9729354, 1.071633, 1.240425, 1.035746, 1.267793, 0.7699658, 0.7296499, 1.005382, 1.430611, 0.4557511),
  boxCIHigh = c(1.137255, 1.315632, 1.530517, 1.243038, 1.523854, 0.9087268, 0.8910902, 1.23604, 1.742356, 0.6182443)
)

dfED04 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9742084, 0.9858412, 1.302571, 0.933532, 0.9261121, 0.762121, 1.251534, 0.9902409, 1.139922, 0.8505858),
  boxCILow = c(0.9433729, 0.9231178, 1.178477, 0.8780863, 0.8561439, 0.6823092, 1.143588, 0.9152591, 1.032274, 0.7656403),
  boxCIHigh = c(1.006052, 1.052826, 1.439732, 0.9924788, 1.001798, 0.8512686, 1.369669, 1.071366, 1.258796, 0.9449557)
)

# Plot
combined <- dfAD04
combined$Legend <- c("Hospital Admissions")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfED04
combined2$Legend <-c("ED visits")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.2205,0.9)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("0-4")


########################################################################################################################
# 99 5-12

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD512 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9730401, 0.9341671, 0.8088676, 0.7532938, 0.6182824, 0.7278357, 0.8338681, 3.662222, 0.8022059, 5.038306),
  boxCILow = c(0.8856534, 0.8187884, 0.6923487, 0.6752529, 0.5399465, 0.655059, 0.7375546, 3.329799, 0.7040634, 4.606275),
  boxCIHigh = c(1.069049, 1.065804, 0.9449962, 0.840354, 0.7079833, 0.8086978, 0.9427588, 4.027831, 0.9140288, 5.510858)
)

dfED512 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9557776, 0.8881808, 0.9960628, 0.8752725, 0.8489373, 0.7842558, 0.8113384, 0.9361496, 0.9042495, 0.9205536),
  boxCILow = c(0.9250721, 0.8269409, 0.8967717, 0.8292792, 0.7818825, 0.7157007, 0.7289253, 0.8538146, 0.810951, 0.8230848),
  boxCIHigh = c(0.9875024, 0.9539557, 1.106347, 0.9238167, 0.9217427, 0.8593777, 0.9030693, 1.026424, 1.008282, 1.029565)
)

# Plot
combined <- dfAD512
combined$Legend <- c("Hospital Admissions")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfED512
combined2$Legend <-c("ED visits")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(3,0.87)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("5-12")

########################################################################################################################
# 99 13-18

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD1318 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.102378, 2.192144, 2.23927, 0.8070578, 0.9224519, 0.9735365, 1.370794, 0.6940578, 4.773119, 0.450251),
  boxCILow = c(1.013754, 2.016858, 2.048659, 0.7277419, 0.8321052, 0.8834346, 1.270438, 0.6248423, 4.260217, 0.4071409),
  boxCIHigh = c(1.19875, 2.382664, 2.447616, 0.8950182, 1.022608, 1.072828, 1.479077, 0.7709406, 5.347772, 0.4979259)
)

dfED1318 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.015529, 1.041717, 1.329842, 1.000077, 0.9724856, 0.9439708, 1.044314, 1.124243, 1.433473, 1.274329),
  boxCILow = c(0.9838487, 0.9675742, 1.203669, 0.9509967, 0.8918708, 0.8705692, 0.957482, 1.023558, 1.277683, 1.145516),
  boxCIHigh = c(1.04823, 1.121541, 1.46924, 1.05169, 1.060387, 1.023561, 1.13902, 1.234832, 1.608259, 1.417628)
)

# Plot
combined <- dfAD1318
combined$Legend <- c("Hospital Admissions")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfED1318
combined2$Legend <-c("ED visits")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(3,0.89)) +
  annotate(geom = "text", y =4, x = 3.75, label ="", 
           size = 3.5, hjust = 0) + ggtitle("13-18")

########################################################################################################################
# 99 AD by age
# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfAD04 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.051892, 1.187381, 1.377858, 1.134668, 1.389939, 0.8364739, 0.8063398, 1.114761, 1.578808, 0.5308159),
  boxCILow = c(0.9729354, 1.071633, 1.240425, 1.035746, 1.267793, 0.7699658, 0.7296499, 1.005382, 1.430611, 0.4557511),
  boxCIHigh = c(1.137255, 1.315632, 1.530517, 1.243038, 1.523854, 0.9087268, 0.8910902, 1.23604, 1.742356, 0.6182443)
)
dfAD512 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(0.9730401, 0.9341671, 0.8088676, 0.7532938, 0.6182824, 0.7278357, 0.8338681, 3.662222, 0.8022059, 5.038306),
  boxCILow = c(0.8856534, 0.8187884, 0.6923487, 0.6752529, 0.5399465, 0.655059, 0.7375546, 3.329799, 0.7040634, 4.606275),
  boxCIHigh = c(1.069049, 1.065804, 0.9449962, 0.840354, 0.7079833, 0.8086978, 0.9427588, 4.027831, 0.9140288, 5.510858)
)
dfAD1318 <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.102378, 2.192144, 2.23927, 0.8070578, 0.9224519, 0.9735365, 1.370794, 0.6940578, 4.773119, 0.450251),
  boxCILow = c(1.013754, 2.016858, 2.048659, 0.7277419, 0.8321052, 0.8834346, 1.270438, 0.6248423, 4.260217, 0.4071409),
  boxCIHigh = c(1.19875, 2.382664, 2.447616, 0.8950182, 1.022608, 1.072828, 1.479077, 0.7709406, 5.347772, 0.4979259)
)

# Plot
combined <- dfAD04
combined$Legend <- c("00-4")
combined$yAxis <-combined$yAxis+0.15

combined2 <-dfAD512
combined2$Legend <-c("05-12")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfAD1318
combined3$Legend <-c("113-18")
combined3$yAxis <-combined3$yAxis-0.15

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
library(ggplot2)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = ADLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(1.5,0.88)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("Hospital Admissions")



########################################################################################################################
# 99 ED by age
# Create labels
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.

dfED04 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9742084, 0.9858412, 1.302571, 0.933532, 0.9261121, 0.762121, 1.251534, 0.9902409, 1.139922, 0.8505858),
  boxCILow = c(0.9433729, 0.9231178, 1.178477, 0.8780863, 0.8561439, 0.6823092, 1.143588, 0.9152591, 1.032274, 0.7656403),
  boxCIHigh = c(1.006052, 1.052826, 1.439732, 0.9924788, 1.001798, 0.8512686, 1.369669, 1.071366, 1.258796, 0.9449557)
)
dfED512 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9557776, 0.8881808, 0.9960628, 0.8752725, 0.8489373, 0.7842558, 0.8113384, 0.9361496, 0.9042495, 0.9205536),
  boxCILow = c(0.9250721, 0.8269409, 0.8967717, 0.8292792, 0.7818825, 0.7157007, 0.7289253, 0.8538146, 0.810951, 0.8230848),
  boxCIHigh = c(0.9875024, 0.9539557, 1.106347, 0.9238167, 0.9217427, 0.8593777, 0.9030693, 1.026424, 1.008282, 1.029565)
)
dfED1318 <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.015529, 1.041717, 1.329842, 1.000077, 0.9724856, 0.9439708, 1.044314, 1.124243, 1.433473, 1.274329),
  boxCILow = c(0.9838487, 0.9675742, 1.203669, 0.9509967, 0.8918708, 0.8705692, 0.957482, 1.023558, 1.277683, 1.145516),
  boxCIHigh = c(1.04823, 1.121541, 1.46924, 1.05169, 1.060387, 1.023561, 1.13902, 1.234832, 1.608259, 1.417628)
)

# Plot
combined <- dfED04
combined$Legend <- c("00-4")
combined$yAxis <-combined$yAxis+0.15

combined2 <-dfED512
combined2$Legend <-c("05-12")
combined2$yAxis <-combined2$yAxis-0.0

combined3 <-dfED1318
combined3$Legend <-c("113-18")
combined3$yAxis <-combined3$yAxis-0.15

comb<-rbind(combined, combined2, combined3)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.86,0.88)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("ED visits")






########################################################################################################################
########################################################################################################################
########################################################################################################################
# 99 male

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADm <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.084143, 1.260615, 1.336962, 0.7163259, 0.8608801, 0.6481715, 0.7379494, 1.372824, 1.417224, 1.215676),
  boxCILow = c(1.0043, 1.137572, 1.195357, 0.6453575, 0.7706353, 0.5818417, 0.6638491, 1.25513, 1.279321, 1.10191),
  boxCIHigh = c(1.170334, 1.396968, 1.495342, 0.7950986, 0.961693, 0.7220628, 0.820321, 1.501554, 1.569992, 1.341187)
)

dfEDm <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.976901, 0.9526097, 1.111744, 0.9593973, 0.9433386, 0.8249026, 1.055326, 0.9862455, 1.117339, 0.8836626),
  boxCILow = c(0.9520813, 0.8948901, 1.008218, 0.9192316, 0.8785711, 0.7596718, 0.9598721, 0.9097884, 1.010337, 0.794806),
  boxCIHigh = c(1.002368, 1.014052, 1.2259, 1.001318, 1.012881, 0.8957345, 1.160273, 1.069128, 1.235673, 0.982453)
)

# Plot
combined <- dfADm
combined$Legend <- c("Hospital Admissions")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfEDm
combined2$Legend <-c("ED visits")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.2205,0.87)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("Male")

########################################################################################################################
# 99 female

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADf <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(1.00602, 1.269877, 1.214582, 1.168908, 1.041946, 1.500007, 1.078995, 1.344954, 1.645756, 1.141359),
  boxCILow = c(0.9271991, 1.143484, 1.081097, 1.063076, 0.9364204, 1.377471, 0.9854091, 1.202927, 1.482852, 1.012701),
  boxCIHigh = c(1.091542, 1.410241, 1.364549, 1.285276, 1.159363, 1.633444, 1.181469, 1.503751, 1.826557, 1.286363)
)

dfEDf <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9878939, 0.9797087, 1.295219, 0.9067396, 0.8598907, 0.9538108, 1.042947, 1.008745, 1.071726, 0.9797577),
  boxCILow = c(0.9611358, 0.9199323, 1.172492, 0.8616222, 0.79491, 0.8733564, 0.9592319, 0.9296414, 0.9655656, 0.8826212),
  boxCIHigh = c(1.015397, 1.043369, 1.430792, 0.9542195, 0.9301833, 1.041677, 1.133967, 1.09458, 1.189557, 1.087585)
)

# Plot
combined <- dfADf
combined$Legend <- c("Hospital Admissions")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfEDf
combined2$Legend <-c("ED visits")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(3,0.87)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("Female")




########################################################################################################################
# AD by sex

# Create labels
ADLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfADm <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.084143, 1.260615, 1.336962, 0.7163259, 0.8608801, 0.6481715, 0.7379494, 1.372824, 1.417224, 1.215676),
  boxCILow = c(1.0043, 1.137572, 1.195357, 0.6453575, 0.7706353, 0.5818417, 0.6638491, 1.25513, 1.279321, 1.10191),
  boxCIHigh = c(1.170334, 1.396968, 1.495342, 0.7950986, 0.961693, 0.7220628, 0.820321, 1.501554, 1.569992, 1.341187)
)
dfADf <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(1.00602, 1.269877, 1.214582, 1.168908, 1.041946, 1.500007, 1.078995, 1.344954, 1.645756, 1.141359),
  boxCILow = c(0.9271991, 1.143484, 1.081097, 1.063076, 0.9364204, 1.377471, 0.9854091, 1.202927, 1.482852, 1.012701),
  boxCIHigh = c(1.091542, 1.410241, 1.364549, 1.285276, 1.159363, 1.633444, 1.181469, 1.503751, 1.826557, 1.286363)
)

# Plot
combined <- dfADm
combined$Legend <- c("Male")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfADf
combined2$Legend <-c("Female")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(ADLabels):1, labels = ADLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(01.5,0.89)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("Hospital Admissions")

########################################################################################################################
# ED by sex

# Create labels
EDLabels = c("All Cause", "Respiratory", "Asthma", "Injury", "Falls", "Transport", "Renal", "Inf/Parasitic", "Lower Resp", "Enteritis")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
dfEDm <- data.frame(
  yAxis= length(ADLabels):1,
  boxOdds = c(0.976901, 0.9526097, 1.111744, 0.9593973, 0.9433386, 0.8249026, 1.055326, 0.9862455, 1.117339, 0.8836626),
  boxCILow = c(0.9520813, 0.8948901, 1.008218, 0.9192316, 0.8785711, 0.7596718, 0.9598721, 0.9097884, 1.010337, 0.794806),
  boxCIHigh = c(1.002368, 1.014052, 1.2259, 1.001318, 1.012881, 0.8957345, 1.160273, 1.069128, 1.235673, 0.982453)
)

dfEDf <- data.frame(
  yAxis= length(EDLabels):1,
  boxOdds = c(0.9878939, 0.9797087, 1.295219, 0.9067396, 0.8598907, 0.9538108, 1.042947, 1.008745, 1.071726, 0.9797577),
  boxCILow = c(0.9611358, 0.9199323, 1.172492, 0.8616222, 0.79491, 0.8733564, 0.9592319, 0.9296414, 0.9655656, 0.8826212),
  boxCIHigh = c(1.015397, 1.043369, 1.430792, 0.9542195, 0.9301833, 1.041677, 1.133967, 1.09458, 1.189557, 1.087585)
)

# Plot
combined <- dfEDm
combined$Legend <- c("Male")
combined$yAxis <-combined$yAxis+0.2

combined2 <-dfEDf
combined2$Legend <-c("Female")
combined2$yAxis <-combined2$yAxis-0.0

comb<-rbind(combined, combined2)


comb
str(comb)
comb$Legend<-as.factor(comb$Legend)
fin <- ggplot(comb, aes(x = boxOdds, y = yAxis, colour=Legend))
fin
fin + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .3, color = "gray50") +
  geom_point(size = 2, group = comb$Legend) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(EDLabels):1, labels = EDLabels) +
  scale_x_continuous(breaks = seq(0,4,0.5) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Risk Ratio (log scale)") +
  theme(legend.position = c(0.85,0.9)) +
  annotate(geom = "text", y =4, x = 1.55, label ="", 
           size = 3.5, hjust = 0) + ggtitle("ED visits")







########################################################################################################################
########################################################################################################################
########################################################################################################################
#99, 97.5, 95 percentile temperatures in ontario overall
Ont99 <- quantile(df.ad.warm$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Ont99

Ont97.5 <- quantile(df.ad.warm$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Ont97.5

Ont95 <- quantile(df.ad.warm$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Ont95


#99, 97.5, 95 percentile temperatures in ontario by Koppen zone
df.ad.warm.Dfa<- subset(df.ad.warm, ClimateArea == "Dfa")
df.ad.warm.Dfb<- subset(df.ad.warm, ClimateArea == "Dfb")
df.ad.warm.Dfc<- subset(df.ad.warm, ClimateArea == "Dfc")


#99
Dfa99 <- quantile(df.ad.warm.Dfa$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfa99

Dfb99 <- quantile(df.ad.warm.Dfb$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfb99

Dfc99 <- quantile(df.ad.warm.Dfc$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfc99

#97.5
Dfa97.5 <- quantile(df.ad.warm.Dfa$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfa97.5

Dfb97.5 <- quantile(df.ad.warm.Dfb$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfb97.5

Dfc97.5 <- quantile(df.ad.warm.Dfc$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfc97.5

#95
Dfa95 <- quantile(df.ad.warm.Dfa$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfa95

Dfb95 <- quantile(df.ad.warm.Dfb$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfb95

Dfc95 <- quantile(df.ad.warm.Dfc$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
Dfc95

########################################################################################################################
#99, 97.5, 95 percentile temperatures in ontario by ECCC zone
df.ad.warm$ClimateArea <- ifelse(df.ad.warm$fsa %in% c("P0V","P0T","P5N"), 'Dfc',
                                 ifelse(df.ad.warm$fsa %in% c("N0R","N9V","N9Y","N8H","N0P","N8A","N0N"), 'Dfa', 'Dfb'))

df.ad.warm.N<- subset(df.ad.warm, GeoArea == "Northern")
df.ad.warm.S<- subset(df.ad.warm, GeoArea == "Southern")
df.ad.warm.SW<- subset(df.ad.warm, GeoArea == "Southwestern")


#99
N99 <- quantile(df.ad.warm.N$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
N99

S99 <- quantile(df.ad.warm.S$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
S99

SW99 <- quantile(df.ad.warm.SW$Tmax_avg, 0.99, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
SW99

#97.5
N97.5 <- quantile(df.ad.warm.N$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
N97.5

S97.5 <- quantile(df.ad.warm.S$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
S97.5

SW97.5 <- quantile(df.ad.warm.SW$Tmax_avg, 0.975, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
SW97.5

#95
N95 <- quantile(df.ad.warm.N$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
N95

S95 <- quantile(df.ad.warm.S$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
S95

SW95 <- quantile(df.ad.warm.SW$Tmax_avg, 0.95, na.rm = TRUE) # identify the 99th percentile of temperature for that FSA
SW95

##########################################################################################################
##########################################################################################################
##########################################################################################################
#Controlling for humidex
AD99.hum <- df.ad.99
AD99.hum$humidex<-(AD99.hum$Tmax_avg + (0.5555 * (AD99.hum$vp - 10)))
summary(AD99.hum$humidex)

fit1.1<- gnm(allcause ~ hw, data=df.ad.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.1)
exp(0.04860)
exp(0.04860 - 1.96 * 0.03407)
exp(0.04860 + 1.96 * 0.03407)
#     1.0498 (0.981987, 1.122297)

fit1.1x<- gnm(allcause ~ hw + humidex, data=AD99.hum, family=quasipoisson, eliminate=factor(stratum))
summary(fit1.1x)
exp(0.064403713)
exp(0.064403713 - 1.96 * 0.034213071)
exp(0.064403713 + 1.96 * 0.034213071)
#     1.066523 (0.9973496, 1.140494)

fit1.2 <- gnm(respiratory ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.2) 
exp(0.23443)
exp(0.23443 - 1.96 * 0.05054)
exp(0.23443 + 1.96 * 0.05054)
fit1.2.1 <- gnm(asthma ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.2.1) 
exp(0.25737)
exp(0.25737 - 1.96 * 0.05663)
exp(0.25737 + 1.96 * 0.05663)

fit1.3 <- gnm(injury ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3) 
exp(-0.13611)
exp(-0.13611 - 1.96 * 0.04915)
exp(-0.13611 + 1.96 * 0.04915)
fit1.3.1 <- gnm(drowning ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.1) 
exp(0.06800)
exp(0.06800 - 1.96 * 0.04675)
exp(0.06800 + 1.96 * 0.04675)
fit1.3.2 <- gnm(falls ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.2) 
exp(-0.07151)
exp(-0.07151 - 1.96 * 0.05483)
exp(-0.07151 + 1.96 * 0.05483)
fit1.3.3 <- gnm(transport_accidents ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.3.3) 
exp(-0.13747)
exp(-0.13747 - 1.96 * 0.04963)
exp(-0.13747 + 1.96 * 0.04963)


fit1.4 <- gnm(heat ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4) 
exp(0.36669)
exp(0.36669 - 1.96 * 0.03968)
exp(0.36669 + 1.96 * 0.03968)
fit1.4.1 <- gnm(heatstroke ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.1) 
exp(12.385)
exp(12.385 - 1.96 * 2.739)
exp(12.385 + 1.96 * 2.739)
fit1.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.2) 
exp(0.08057)
exp(0.08057 - 1.96 * 0.04343)
exp(0.08057 + 1.96 * 0.04343)
fit1.4.3 <- gnm(renal ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.4.3)
exp(-0.05671)
exp(-0.05671 - 1.96 * 0.04831)
exp(-0.05671 + 1.96 * 0.04831)

fit1.5 <- gnm(infectious_parasitic ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5)
exp(0.30950)
exp(0.30950  - 1.96 *  0.04871)
exp(0.30950  + 1.96 *  0.04871)
fit1.5.1 <- gnm(otitis ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.1)
exp(-9.271)
exp(-9.271  - 1.96 *  3.906)
exp(-9.271  + 1.96 *  3.906)
fit1.5.2 <- gnm(lower_respiratory ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.2)
exp(0.4090)
exp(0.4090  - 1.96 *  0.0516)
exp(0.4090  + 1.96 *  0.0516)
fit1.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ad.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit1.5.3)
exp(0.17207)
exp(0.17207  - 1.96 *  0.05335)
exp(0.17207  + 1.96 *  0.05335)


##########################################################################################################
ED 99

# GENERATE DOW AND STRATUM
df.ed.99$month   <- as.factor(months(df.ed.99$date))
df.ed.99$year    <- as.factor(format(df.ed.99$date, format="%Y") )
df.ed.99$dow     <- as.factor(weekdays(df.ed.99$date))
df.ed.99$fsa     <- as.factor(df.ed.99$fsa)
df.ed.99$stratum <- as.factor(df.ed.99$year:df.ed.99$month:df.ed.99$dow:df.ed.99$fsa)

df.ed.99 <- data[order(df.ed.99$date),]

fit4.1<- gnm(allcause ~ hw, data=df.ed.99, family=quasipoisson, eliminate=factor(stratum))
summary(fit4.1)
exp(-0.018279)
exp(-0.018279 - 1.96 * 0.009786 )
exp(-0.018279 + 1.96 * 0.009786 )

fit4.2 <- gnm(respiratory ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.2) 
exp(-0.03525)
exp(-0.03525 - 1.96 * 0.02514)
exp(-0.03525 + 1.96 * 0.02514)
fit4.2.1 <- gnm(asthma ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.2.1) 
exp(0.16296)
exp(0.16296 - 1.96 * 0.04615)
exp(0.16296 + 1.96 * 0.04615)

fit4.3 <- gnm(injury ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3) 
exp(-0.06433)
exp(-0.06433  - 1.96 * 0.01734)
exp(-0.06433  + 1.96 * 0.01734)
fit4.3.1 <- gnm(drowning ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.1) 
exp(0.1408)
exp(0.1408  - 1.96 * 0.0461)
exp(0.1408  + 1.96 * 0.0461)
fit4.3.2 <- gnm(falls ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.2) 
exp(-0.09822)
exp(-0.09822  - 1.96 * 0.03013)
exp(-0.09822  + 1.96 * 0.03013)
fit4.3.3 <- gnm(transport_accidents ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.3.3) 
exp(-0.14156)
exp(-0.14156  - 1.96 * 0.03865)
exp(-0.14156  + 1.96 * 0.03865)

fit4.4 <- gnm(heat ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4) 
exp(1.13493)
exp(1.13493 - 1.96 * 0.02955)
exp(1.13493 + 1.96 * 0.02955)
fit4.4.1 <- gnm(heatstroke ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.1) 
exp(1.93202)
exp(1.93202 - 1.96 * 0.02323)
exp(1.93202 + 1.96 * 0.02323)
fit4.4.2 <- gnm(dehydration_electrolyte ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.2) 
exp(0.29969)
exp(0.29969 - 1.96 * 0.03982)
exp(0.29969 + 1.96 * 0.03982)
fit4.4.3 <- gnm(renal ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.4.3) 
exp(0.04399)
exp(0.04399 - 1.96 * 0.04163)
exp(0.04399 + 1.96 * 0.04163)

fit4.5 <- gnm(infectious_parasitic ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5)
exp(-0.002931)
exp(-0.002931  - 1.96 *  0.034790)
exp(-0.002931  + 1.96 *  0.034790)
fit4.5.1 <- gnm(otitis ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.1)
exp(0.01602)
exp(0.01602  - 1.96 *  0.02884)
exp(0.01602  + 1.96 *  0.02884)
fit4.5.2 <- gnm(lower_respiratory ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.2)
exp(0.09334)
exp(0.09334  - 1.96 *  0.04786)
exp(0.09334  + 1.96 *  0.04786)
fit4.5.3 <- gnm(bacterial_enteritis ~ hw, data=df.ed.99, family=quasipoisson,eliminate=factor(stratum))
summary(fit4.5.3)
exp(-0.07457)
exp(-0.07457  - 1.96 *  0.04866)
exp(-0.07457  + 1.96 *  0.04866)

