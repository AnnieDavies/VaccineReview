#include packages
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)


################
# Read in Data
################

## 0.0 Read in functions 
list.files("functions", full.names = TRUE) %>% map(source)

## 0.1 Read in data
df_int <- read_excel("Data//Intervention Level Data.xlsx")
df_outcome <- read_excel("Data//Outcome Level Data.xlsx")
df_study <- read_excel("Data//Study Level Data.xlsx")
df_rob <- read_excel("Data//Overall risk of bias.xlsx")

## 0.2 Rename variables
names(df_study)[names(df_study) == "Author, Year"] <- "StudyName"
names(df_int)[names(df_int) == "IntCont?"] <- "IntLabel"
names(df_outcome)[names(df_outcome) == "Intervention"] <- "IntLabel"
names(df_int)[names(df_int) == "NICE"] <- "IntCategory"
names(df_study)[names(df_study) == "VaccRecipient"] <- "AgeGroup"
names(df_study)[names(df_study) == "Vaccine(s)"] <- "Vaccine"
names(df_study)[names(df_study) == "UnitRandomisation"] <- "UnitRand"

## 0.3 Re-order data
df_study <- df_study[order(df_study$StudyID),] #by study (NB 'alphabetically' rather than numerically)
df_outcome <- df_outcome[order(df_outcome$IntLabel),] #by arm
df_outcome <- df_outcome[order(df_outcome$StudyID),] #by study
df_int <- df_int[order(df_int$IntLabel),] #by arm
df_int <- df_int[order(df_int$StudyID),] #by study
df_rob <- df_rob[order(df_rob$StudyID),] #by study

## Corrections----------------------------
# 1. Study 26390 country = Denmark
ind <- which(df_study$StudyID=="26390")
df_study$Country[ind] <- "Denmark"

# 2. Re-code age category
#11197 - college girls 18+ (mean age 19.1 years) the vaccine is HPV - recode as adolescent. 
ind <- which(df_study$StudyID=="11197")
df_study$AgeGroup[ind] <- "Adolescents"

#3. 13575 - college girls 18+ (mean age 19 years) the vaccine is HPV - recode as adolescents
ind <- which(df_study$StudyID=="13575")
df_study$AgeGroup[ind] <- "Adolescents"

#4. Exclude duplicates: #1859 and #19938 
df_study <- subset(df_study, df_study$StudyID!="1859" & df_study$StudyID!="19938")
df_outcome <- subset(df_outcome, StudyID %in% df_study$StudyID)
df_int <- subset(df_int, StudyID %in% df_study$StudyID)
df_rob <- subset(df_rob, StudyID %in% df_study$StudyID)


## ------------------------------------------

#0.4 Descriptive studies only
df_study.desc <- subset(df_study, df_study$`StudyDescOnly?`=="TRUE")
df_outcome.desc <- subset(df_outcome, StudyID %in% df_study.desc$StudyID)
df_int.desc <- subset(df_int, StudyID %in% df_study.desc$StudyID)

## 0.5 Non-descriptive only
df_study <- subset(df_study, df_study$`StudyDescOnly?`=="FALSE")
df_outcome <- subset(df_outcome, StudyID %in% df_study$StudyID)
df_int <- subset(df_int, StudyID %in% df_study$StudyID)
df_rob <- subset(df_rob, StudyID %in% df_study$StudyID)


##############
# Format data
##############

## Impute missing data (Howell Jones)-------------------------------------------
# Calculate total number of participants per arm. 
# ArmClusters = number of school years
# From paper: median number of participants per year = 42 [IQR = 26-60]
n.per.cluster <- 42 
ind.HJ <- which(df_outcome$StudyID=="18462b")
for(i in ind.HJ[1]:ind.HJ[length(ind.HJ)]){
  print(paste("StudyID = ", df_outcome$StudyID[i], "Arm = ", df_outcome$IntLabel[i]))
  df_outcome$NoPtsAnalysed[i] <- as.numeric(df_outcome$NoClusters[i])*n.per.cluster
}
##------------------------------------------------------------------------------

## merge & output raw data
df_merge <- merge(df_outcome, df_int, by = c("StudyID", "IntLabel"))
df_merge <- merge(df_study, df_merge, by = c("StudyID"))
df_merge <- merge(df_merge, df_rob, by = c("StudyID"))

## Format and merge
df_study <- format_study(df_study, df_int)
df_outcome <- format_outcome(df_outcome)
df_int <- format_int(df_int, df_study)
df_formatted <- merge_all(df_int, df_outcome, df_study)
df_formatted <- fix_cluster(df_formatted)

## Code studies
df_study01 <- code_study_inspect(df_study)
df_int_list <- code_interventions(df_int)
df_int01_all <- df_int_list[[1]]
df_int01 <- df_int01_all[, c("StudyID", "IntLabel", "ControlActive", "RefArm", "ControlType", 
                         "IntCategory","DF_PersonalDelivery", "DF_interaction", "DF_human",
                         "DB_healthcare", "DB_community", "NumberContacts", "Intensity", "MediumIntensity", "HighIntensity",
                         "Access_ExtendedOpportunities", "Access_AppointmentSchedulingHelp",
                         "Access_AppointmentSchedulingOnline", "Access_AcceleratedDosing", 
                         "Affordability_Incentives", "Affordability_CostsCovered", 
                         "Awareness",
                         "Acceptance_VaccineSafety", "Acceptance_DiseasePerceivedRisk",
                         "Acceptance_SocialFactors","Acceptance_DecisionAids", "Acceptance_MotivationalInterviewing",
                         "Activation")]

df01_formatted <- merge01(df_int01, df_outcome, df_study01)
df01_formatted <- fix_cluster(df01_formatted)

data <- df01_formatted

## Preliminary analyses

#Adjust for clustering
data$ActualICC <- ifelse(data$ActualICC=="1.74 × 10−19", 1.74e-19, data$ActualICC)
icc.hh <- 0.7
# icc.hh <- 0 #sensitivity
# icc.hh <- 1 #sensitivity
icc.other <- 0.05
# icc.other <- 0 #sensitivity
# icc.other <- 0.3 #sensitivity 
data <- adj_cluster(data, icc.hh, icc.other, adjust = TRUE) #okay if NAs are introduced
# data <- adj_cluster(data, icc.hh, icc.other, adjust = FALSE)

## ROB
# format
df_rob <- format_rob(df_rob)
# High ROB indicator 
highrob <- df_rob$HighROB
# Merge
data <- merge(data, df_rob, by = c("StudyID"))


## Exclude High ROB
# data <- subset(data, HighROB==0) #sensitivity

## Now run either MA/NMA or CNMA section:=======================================

##############
# For aggregate analysis (MA and NMA)-------------------------------------------
##############

## Exclude NMA outliers
# outliers <- c("133", "3877", "7646", "7651", "8010")
# data <- subset(data, !(data$StudyID %in% outliers)) #sensitivity

## Create data for MA/NMA
ma_dat <- data[c("StudyID", "StudyName", "StudyDesign", "UnitRand",
                  "ActualICC", "AssumedICC","IntCategory", "NoArmsAnalysed", "NoClusters", "ArmClusters",
                  "Country", "AgeGroup", "VaccineType", "PostCovid", "ROB", "HighROB",
                  "n", "r", "n_adj", "r_adj")]
## Combine arms
ma_dat <- combine_arms_fornma(ma_dat) 
## Label treatments
ma_dat <- label_trts(ma_dat)


#save data-------------
# write.csv(ma_dat, "Data_MA_NMA.csv")
# data.drop <- subset(data, !(StudyID %in%ma_dat$StudyID))
# write.csv(data.drop, "Data_IdenticalArmGroup.csv")
#----------------------

#treatments
trts <- c("Control", "Access", "Affordability", "Education", "Reminder", "Education and Reminder", "Multicomponent")
M <- length(trts)

#number of studies
studies <- unique(ma_dat$StudyID)
N <- length(studies)

#number of arms per study
a <- create_a_ma(ma_dat)
sum_a <- sum(a) 

#arm-level data
r.adj <- round(ma_dat$r_adj) #events
n.adj <- round(ma_dat$n_adj) #observations
t <- ma_dat$Trt #treatment indicator

#create arm.ind - index where new study begins in arm level vectors
arm.ind <- create_armind(a, N)
#create cont.ind - index where new study begins in contrast level vectors
cont.ind <- create_contind(a, N)

#Create S (for Sigma - 1s on diag and 0.5 off diag)
S <- create_S(a, N)


#save JAGS data
tokeep <- c("ma_dat", "studies", "N", "M", "a", "r", "n", "r.adj", "n.adj", "t", 
            "arm.ind", "cont.ind", "S", "trts")
rm(list = setdiff(ls(), tokeep))
save.image("MA_NMA_DATA.RData")
# save.image("MA_NMA_DATA_ROB.RData")
# save.image("MA_NMA_DATA_Outliers.RData")
# save.image("MA_NMA_DATA_ICC0.RData") #sensitivity
# save.image("MA_NMA_DATA_ICC-high.RData") #sensitivity

##############
# For complex analysis (CNMA)---------------------------------------------------
##############

## Exclude CNMA outliers
# outliers <- c("13114", "133", "137", "15043", "27", "27023", "3877", "48", "7646", "7651", "8010")
# data <- subset(data, !(data$StudyID %in% outliers)) #sensitivity

#number of studies
studies <- unique(data$StudyID)
N <- length(studies)

#number of arms per study
a <- create_a_tot(data)
sum_a <- sum(a) 
A <- max(a)

## Choose covariates
# Intervention
covs_x <- c("DF_PersonalDelivery", "DF_interaction", "DF_human",
            "DB_healthcare","DB_community", "MediumIntensity", "HighIntensity",
            "Access_ExtendedOpportunities", "Access_AppointmentSchedulingHelp",
            "Access_AppointmentSchedulingOnline", "Access_AcceleratedDosing", 
            "Affordability_Incentives", "Affordability_CostsCovered", 
            "Acceptance_VaccineSafety", "Acceptance_DiseasePerceivedRisk",
            "Acceptance_SocialFactors","Acceptance_DecisionAids", "Acceptance_MotivationalInterviewing",
            "Activation")
m<-length(covs_x)
# Study
# (remove adults - set these as reference age group)
# (remove healthcare - set this as reference setting)
covs_z <- c("PostCovid", "YoungChildren", "Children", "Adolescents", 
            "OlderAdults", "PregWomen",  
            "Education", "CommunityOther",
            "Online", "PropMale", "SpecificPopUnderserved")
q<-length(covs_z)

# Other necessary columns
initial_cols <- c("StudyID", "IntLabel", "ControlActive", "RefArm", "ControlType",
                  "NoArmsAnalysed", "event", "ROB", "HighROB")
data_cols <- c("n", "r", "n_adj", "r_adj", "UsedICC", "MeanClusterSize", "DesignEffect")

data <- data[,c(initial_cols, data_cols, covs_z, covs_x)]

## Centre data
covs <- c(covs_z, covs_x, covs_J)
centred_data <- centre_covs(data, covs, standardise = FALSE)
data <- centred_data[[1]]
centred_vals <- centred_data[[2]]

## Get contrast level covariates (and calculate active comparisons)
covs_arm <- covs_x #and any interactions that depend on arms
data_cont <- get_contrast_covs(data, covs, covs_arm) ##use this to get x and J vecs

#number of active comparison arms
identical_arms <- check_active_comp(data, data_cont, covs_arm)
  

## Create data for JAGS----------------------------------------------
#arm-level data
r.adj <- round(data$r_adj) #events
n.adj <- round(data$n_adj) #observations

#create arm.ind - index where new study begins in arm level vectors
arm.ind <- create_armind(a, N)
#create cont.ind - index where new study begins in contrast level vectors
cont.ind <- create_contind(a, N)

#Create S (for Sigma - 1s on diag and 0.5 off diag)
S <- create_S(a, N)

# Covariates
#Intervention level
x <- create_armlevel_vec(data_cont, cont.ind, covs_x, a, N, A)
#Study level
z <- create_studylevel_vec(data_cont, cont.ind, N, covs_z)

#Reference arm indicator
ref_arm <- data[!duplicated(data$StudyID), ]$ControlActive
ref_arm <- ifelse(ref_arm == "Control", 1, 0)

#save JAGS data
tokeep <- c("data", "data_cont", "studies", "N", "a", "r", "n", "r.adj", "n.adj", 
            "arm.ind", "cont.ind", "S", "x", "z", "J", "ref_arm", "covs_x",
            "covs_z", "covs_J", "covs", "m", "q", "l")
rm(list = setdiff(ls(), tokeep))
save.image("CNMA_DATA.RData")
# save.image("CNMA_DATA_ROB.RData")
# save.image("CNMA_DATA_Outliers.RData")
# save.image("CNMA_DATA_ICC0.RData") #sensitivity
# save.image("CNMA_DATA_ICC-high.RData") #sensitivity
