check_dup <- function(df_study){
  duplicated_ids <- df_study$StudyID[duplicated(df_study$StudyID)]
  ind <- which(df_study$StudyID %in% duplicated_ids)
  if(length(ind)>0){
    for(i in 1:length(ind)){
      print(paste("Study = ", df_study$`StudyName`[ind[i]], ", ID = ", df_study$StudyID[ind[i]], ", UniqueID = ", df_study$UniqueID[ind[i]]))
    }
  }
  duplicated_ids
}

check_studies <- function(df_int, df_outcome, df_study){
  
  # Extract unique StudyIDs from each dataframe
  ids_int <- unique(df_int$StudyID)
  ids_outcome <- unique(df_outcome$StudyID)
  ids_study <- unique(df_study$StudyID)
  
  # Find missing StudyIDs
  missing_out_vs_int <- setdiff(ids_int, ids_outcome)#in int but not in outcome
  missing_out_vs_study <- setdiff(ids_study, ids_outcome)#in study but not in outcome
  
  missing_study_vs_int <- setdiff(ids_int, ids_study)#in int but not in study
  missing_study_vs_out <- setdiff(ids_outcome, ids_study)#in outcome but not in study
  
  missing_int_vs_out <- setdiff(ids_outcome, ids_int)#in outcome but not in int
  missing_int_vs_study <- setdiff(ids_study, ids_int)#in outcome but not in int
  
  #compile all missing studyIDs and de-duplicate
  missing <- c(missing_out_vs_int, missing_out_vs_study, missing_study_vs_int,
               missing_study_vs_out, missing_int_vs_out, missing_int_vs_study)
  missing <- unique(missing)
  
  if(length(missing)>0){
    #create dataframe of missing outcomes
    df_miss <- data.frame(StudyID = missing)
    
    # Add empty columns
    df_miss$name <- NA
    df_miss$study <- 0
    df_miss$outcome <- 0
    df_miss$int <- 0
    
    if(length(missing)>0){
      for(i in 1:length(missing)){
        if(missing[i]%in%df_study$StudyID){
          ind <- which(df_study$StudyID %in% missing[i])
          df_miss$name[i] <- df_study$`StudyName`[ind[1]]
          df_miss$study[i] <- 1
        }
        if(missing[i]%in%df_outcome$StudyID){
          df_miss$outcome[i] <- 1
        }
        if(missing[i]%in%df_int$StudyID){
          df_miss$int[i] <- 1
        }
      }
    }
    
  }else{
    print("no mis-matched data")
    df_miss <- missing
  }
  df_miss
}

check_arms <- function(df_int, df_outcome){
  df_int$StudyArm <- paste(df_int$StudyID, df_int$IntLabel)
  df_outcome$StudyArm <- paste(df_outcome$StudyID, df_outcome$IntLabel)
  
  ids_int <- unique(df_int$StudyArm)
  ids_outcome <- unique(df_outcome$StudyArm)
  
  missing_out_vs_int <- setdiff(ids_int, ids_outcome)#in int but not in outcome
  missing_int_vs_out <- setdiff(ids_outcome, ids_int)#in outcome but not in int
  
  missing <- c(missing_out_vs_int, missing_int_vs_out)
  #create dataframe of missing outcomes
  if(length(missing)>0){
    df_miss <- data.frame(StudyArm = missing)
    
    df_miss$outcome <- c(rep(0, length(missing_out_vs_int)), rep(1, length(missing_int_vs_out)))
    df_miss$int <- c(rep(1, length(missing_out_vs_int)), rep(0, length(missing_int_vs_out)))
    
    df_miss$StudyID <- NA
    if(nrow(df_miss)>0){
      for(i in 1:nrow(df_miss)){
        out_split <- strsplit(df_miss$StudyArm[i], split = " ")
        df_miss$StudyID[i] <- out_split[[1]][1]
      }
    }
    
  }else{
    df_miss <- NULL
  }
  df_miss
}

check_arm_dup <- function(df){
  df$StudyArm <- paste(df$StudyID, df$IntLabel)
  duplicated_arms <- df$StudyArm[duplicated(df$StudyArm)]
  ind <- which(df$StudyArm %in% duplicated_arms)
  if(length(ind)>0){
    for(i in 1:length(ind)){
      print(paste("StudyArm = ", df$StudyArm[ind[i]], ", UniqueID = ", df$UniqueID[ind[i]]))
    }
  }
  duplicated_arms
}

check_study_arms <- function(df_int, df_outcome, df_study){
 
  df_mismatch <- df_study[c("StudyID", "StudyName", "NoArms")]
  df_mismatch$ArmsInt <- NA
  df_mismatch$ArmsOutcome <- NA
  for(i in 1:nrow(df_study)){
    df_mismatch$ArmsInt[i] <- sum(df_int$StudyID == df_study$StudyID[i])
    df_mismatch$ArmsOutcome[i] <- sum(df_outcome$StudyID == df_study$StudyID[i])
    
    if(!is.na(df_mismatch$NoArms[i])){
      if(df_mismatch$NoArms[i] != df_mismatch$ArmsInt[i] || 
         df_mismatch$NoArms[i] != df_mismatch$ArmsOutcome[i]){
       print(paste("Mismatched arms (study): Study:", df_mismatch$`StudyName`[i], "ID:", df_mismatch$StudyID[i], "Row:", i)) 
      }else if(df_mismatch$ArmsInt[i] != df_mismatch$ArmsOutcome[i]){
        print(paste("Mismatched arms (int/out): Study:", df_mismatch$`StudyName`[i], "ID:", df_mismatch$StudyID[i], "Row:", i)) 
      }
    }else{
      print(paste("NA No. of arms: Study:", df_mismatch$`StudyName`[i], "ID:", df_mismatch$StudyID[i], "Row:", i)) 
    }
  }
  df_mismatch
  
}

check_int <- function(df_int){
  binary_ints <- c("AccessVaccClinicsCommSettingsYN", "AccessDedicatedClinicsYN",
                   "AccessExtendedHoursClinicYN",	"AccessOutreachMobileClinicsYN",
                   "AccessOppurtunisticVaccsYN", "AccessAppointmentSchedulingYN",
                   "AccessAcceleratedScheduleYN",
                   "AffordabilityFinancialIncenYN", "AffordabilityPaymentCoverCostsYN",
                   "AffordabilityTimeCostsYN", "AwarenessKnowledgeVaccsYN",
                   "AcceptanceVaccSafetyEfficacyAttitudeYN", "AcceptanceDiseasePerceivedSeverityRiskYN",
                   "AcceptanceIndFactorsYN", "AcceptanceSocialInfluenceYN",
                   "AcceptanceDesicionAidsHelpYN", "AcceptanceAltProvisionYN",	
                   "ActivationPromptsRemindersYN", "ActivationMandatoryPolYN",
                   "StandardCareYN", "NoInterventionYN", "AttentionPlaceboYN")
  
  other_ints <- c("DeliveryFormat", "PersonalisedTailoredInteractive",
                  "InteractiveType", "DeliveredBy?", "NumberContacts")
  
  no_int_covs <- df_int %>%
    rowwise() %>%
    filter(
      # Check if all columns from binary_ints are FALSE
      all(c_across(all_of(binary_ints)) == FALSE)
    ) %>%
    ungroup()
  
  no_int_covs <- no_int_covs[c("StudyID", "IntLabel", "DeliveryFormat", "PersonalisedTailoredInteractive",
                     "InteractiveType", "DeliveredBy?", "NumberContacts",
                     "AccessVaccClinicsCommSettingsYN", "AccessDedicatedClinicsYN",
                     "AccessExtendedHoursClinicYN",	"AccessOutreachMobileClinicsYN",
                     "AccessOppurtunisticVaccsYN", "AccessAppointmentSchedulingYN",
                     "AccessAcceleratedScheduleYN",
                     "AffordabilityFinancialIncenYN", "AffordabilityPaymentCoverCostsYN",
                     "AffordabilityTimeCostsYN", "AwarenessKnowledgeVaccsYN",
                     "AcceptanceVaccSafetyEfficacyAttitudeYN", "AcceptanceDiseasePerceivedSeverityRiskYN",
                     "AcceptanceIndFactorsYN", "AcceptanceSocialInfluenceYN",
                     "AcceptanceDesicionAidsHelpYN", "AcceptanceAltProvisionYN",	
                     "ActivationPromptsRemindersYN", "ActivationMandatoryPolYN",
                     "StandardCareYN", "NoInterventionYN", "AttentionPlaceboYN")]
  
  no_int_covs
  
}


check_nopts <- function(df_study, df_outcome){
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$NoPtsTotal[i])){
      print(paste("NA No. of pts: Study:", df_study$`StudyName`[i], "ID:", df_study$StudyID[i], "Row:", i)) 
    }
  }
}

check_gender <- function(df_study, type){
  
  if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsRandomised"] <- "NoPtsTotal"
  }
  
  df_study$`GenderFemale%`<- as.numeric(df_study$`GenderFemale%`)
  df_study$`GenderMale%`<- as.numeric(df_study$`GenderMale%`)
  df_study$`GenderFemaleN`<- as.numeric(df_study$`GenderFemaleN`)
  df_study$`GenderMaleN`<- as.numeric(df_study$`GenderMaleN`)
  df_study$NoPtsTotal <- as.numeric(df_study$NoPtsTotal)
  
  df_study$GenderMalePercN <- NA
  df_study$GenderFemalePercN <- NA
  
  conflict.perc.ids <- c()
  conflict.MFperc.ids <- c()
  conflict.MFn.ids <- c()
  for(i in 1:nrow(df_study)){
    #Check N and % agree for males
    if(!is.na(df_study$`GenderMale%`[i]) && !is.na(df_study$`GenderMaleN`[i]) && !is.na(df_study$NoPtsTotal[i])){
      
      df_study$GenderMalePercN[i] <- (df_study$`GenderMaleN`[i]/df_study$NoPtsTotal[i])*100
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$GenderMalePercN[i] - df_study$`GenderMale%`[i])>1){
        conflict.perc.ids <- append(conflict.perc.ids, df_study$StudyID[i])
      }
    }
    #Check N and % agree for females
    if(!is.na(df_study$`GenderFemale%`[i]) && !is.na(df_study$`GenderFemaleN`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$GenderFemalePercN[i] <- (df_study$`GenderFemaleN`[i]/df_study$NoPtsTotal[i])*100
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$GenderFemalePercN[i] - df_study$`GenderFemale%`[i])>1){
        conflict.perc.ids <- append(conflict.perc.ids, df_study$StudyID[i])
      }
    }
    #check males & females agree (%)
    if(!is.na(df_study$`GenderMale%`[i]) && !is.na(df_study$`GenderFemale%`[i])){
      if(df_study$`GenderMale%`[i] + df_study$`GenderFemale%`[i] != 100){
        conflict.MFperc.ids <- append(conflict.MFperc.ids, df_study$StudyID[i])
      }
    }
    #check males & females agree (N)
    if(!is.na(df_study$`GenderMaleN`[i]) && !is.na(df_study$`GenderFemaleN`[i]) && !is.na(df_study$NoPtsTotal[i])){
      if(df_study$`GenderMaleN`[i] + df_study$`GenderFemaleN`[i] != df_study$NoPtsTotal[i]){
        conflict.MFn.ids <- append(conflict.MFn.ids, df_study$StudyID[i])
      }
    }
  }
  
  
  if(type=="study"){
    df_conflict.perc <- subset(df_study, StudyID %in% conflict.perc.ids)
    df_conflict.MFperc <- subset(df_study, StudyID %in% conflict.MFperc.ids)
    df_conflict.MFn <- subset(df_study, StudyID %in% conflict.MFn.ids)
    
    df_conflict.perc <- df_conflict.perc[c("StudyID", "StudyName", "NoPtsTotal",
                                   "GenderMaleN", "GenderFemaleN",
                                   "GenderMale%", "GenderMalePercN", 
                                   "GenderFemale%","GenderFemalePercN")]
    df_conflict.MFperc <- df_conflict.MFperc[c("StudyID", "StudyName", 
                                           "GenderMale%", "GenderFemale%")]
    df_conflict.MFn <- df_conflict.MFn[c("StudyID", "StudyName", "NoPtsTotal",
                                           "GenderMaleN", "GenderFemaleN",
                                           "GenderMalePercN", "GenderFemalePercN")]
  }else if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsTotal"] <- "NoPtsRandomised"
    df_conflict.perc <- subset(df_study, StudyID %in% conflict.perc.ids)
    df_conflict.MFperc <- subset(df_study, StudyID %in% conflict.MFperc.ids)
    df_conflict.MFn <- subset(df_study, StudyID %in% conflict.MFn.ids)
    
    df_conflict.perc <- df_conflict.perc[c("StudyID", "IntLabel", "NoPtsRandomised",
                                           "GenderMaleN", "GenderFemaleN",
                                           "GenderMale%", "GenderMalePercN", 
                                           "GenderFemale%","GenderFemalePercN")]
    df_conflict.MFperc <- df_conflict.MFperc[c("StudyID", "IntLabel",
                                               "GenderMale%", "GenderFemale%")]
    df_conflict.MFn <- df_conflict.MFn[c("StudyID", "IntLabel", "NoPtsRandomised",
                                         "GenderMaleN", "GenderFemaleN",
                                         "GenderMalePercN", "GenderFemalePercN")]
  }
  res <- list(df_conflict.perc, df_conflict.MFperc, df_conflict.MFn)
  res
}


check_gender2 <- function(df_study, type){
  
  if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsRandomised"] <- "NoPtsTotal"
  }
  
  df_study$`GenderFemale%`<- as.numeric(df_study$`GenderFemale%`)
  df_study$`GenderMale%`<- as.numeric(df_study$`GenderMale%`)
  df_study$`GenderFemaleN`<- as.numeric(df_study$`GenderFemaleN`)
  df_study$`GenderMaleN`<- as.numeric(df_study$`GenderMaleN`)
  df_study$NoPtsTotal <- as.numeric(df_study$NoPtsTotal)
  
  df_study$SumMF <- df_study$GenderFemaleN + df_study$GenderMaleN
  df_study$GenderMalePercN <- (df_study$GenderMaleN/df_study$SumMF)*100
  df_study$GenderFemalePercN <- (df_study$GenderFemaleN/df_study$SumMF)*100
  
  conflict.perc.ids <- c()
  conflict.MFperc.ids <- c()
  conflict.MFn.ids <- c()
  for(i in 1:nrow(df_study)){
    
    #Check N and % agree for males
    if(!is.na(df_study$`GenderMale%`[i]) && !is.na(df_study$GenderMalePercN[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$GenderMalePercN[i] - df_study$`GenderMale%`[i])>1){
        conflict.perc.ids <- append(conflict.perc.ids, df_study$StudyID[i])
      }
    }
    #Check N and % agree for females
    if(!is.na(df_study$`GenderFemale%`[i]) && !is.na(df_study$GenderFemalePercN[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$GenderFemalePercN[i] - df_study$`GenderFemale%`[i])>1){
        conflict.perc.ids <- append(conflict.perc.ids, df_study$StudyID[i])
      }
    }
    #check males & females agree (%)
    if(!is.na(df_study$`GenderMale%`[i]) && !is.na(df_study$`GenderFemale%`[i])){
      if(df_study$`GenderMale%`[i] + df_study$`GenderFemale%`[i] != 100){
        conflict.MFperc.ids <- append(conflict.MFperc.ids, df_study$StudyID[i])
      }
    }
    #check males & females agree (N)
    if(!is.na(df_study$SumMF[i]) && !is.na(df_study$NoPtsTotal[i])){
      if(df_study$SumMF[i] != df_study$NoPtsTotal[i]){
        conflict.MFn.ids <- append(conflict.MFn.ids, df_study$StudyID[i])
      }
    }
  }
  
  
  if(type=="study"){
    df_conflict.perc <- subset(df_study, StudyID %in% conflict.perc.ids)
    df_conflict.MFperc <- subset(df_study, StudyID %in% conflict.MFperc.ids)
    df_conflict.MFn <- subset(df_study, StudyID %in% conflict.MFn.ids)
    
    df_conflict.perc <- df_conflict.perc[c("StudyID", "StudyName", "NoPtsTotal",
                                           "SumMF",
                                           "GenderMaleN", "GenderFemaleN",
                                           "GenderMale%", "GenderMalePercN", 
                                           "GenderFemale%","GenderFemalePercN")]
    df_conflict.MFperc <- df_conflict.MFperc[c("StudyID", "StudyName", 
                                               "GenderMale%", "GenderFemale%",
                                               "GenderMalePercN", "GenderFemalePercN",
                                               "GenderMaleN", "GenderFemaleN")]
    df_conflict.MFn <- df_conflict.MFn[c("StudyID", "StudyName", "NoPtsTotal",
                                         "SumMF")]
  }else if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsTotal"] <- "NoPtsRandomised"
    df_conflict.perc <- subset(df_study, StudyID %in% conflict.perc.ids)
    df_conflict.MFperc <- subset(df_study, StudyID %in% conflict.MFperc.ids)
    df_conflict.MFn <- subset(df_study, StudyID %in% conflict.MFn.ids)
    
    df_conflict.perc <- df_conflict.perc[c("StudyID", "IntLabel", "NoPtsRandomised",
                                           "SumMF",
                                           "GenderMaleN", "GenderFemaleN",
                                           "GenderMale%", "GenderMalePercN", 
                                           "GenderFemale%","GenderFemalePercN")]
    df_conflict.MFperc <- df_conflict.MFperc[c("StudyID", "IntLabel",
                                               "GenderMale%", "GenderFemale%",
                                               "GenderMalePercN", "GenderFemalePercN",
                                               "GenderMaleN", "GenderFemaleN")]
    df_conflict.MFn <- df_conflict.MFn[c("StudyID", "IntLabel", "NoPtsRandomised",
                                         "SumMF")]
  }
  res <- list(df_conflict.perc, df_conflict.MFperc, df_conflict.MFn)
  res
}


check_gender_int <- function(df_study, df_int){
  
  miss.study.gender.id <- c()
  
  for(i in 1:nrow(df_study)){
   if(is.na(df_study$`GenderMale%`[i]) && is.na(df_study$`GenderFemale%`[i]) && 
      is.na(df_study$`GenderMaleN`[i]) && is.na(df_study$`GenderFemaleN`[i])){
     miss.study.gender.id <- append(miss.study.gender.id, df_study$StudyID[i])
   } 
  }
  
  #df_int - keep only studies with missing gender
  df_int <- subset(df_int, StudyID %in% miss.study.gender.id)
  #and only keep those with information on intervention level
  miss.int.gender.id <- c()
  if(nrow(df_int)>0){
    for(i in 1:nrow(df_int)){
      if(is.na(df_int$`GenderMale%`[i]) && is.na(df_int$`GenderFemale%`[i]) && 
         is.na(df_int$`GenderMaleN`[i]) && is.na(df_int$`GenderFemaleN`[i])){
        miss.int.gender.id <- append(miss.int.gender.id, df_int$StudyID[i])
      } 
    }
  }
  df_int <- subset(df_int, !(StudyID %in% miss.int.gender.id))
  df_int <- df_int[c("StudyID", "IntLabel", "NoPtsRandomised",
                     "GenderMaleN", "GenderMale%", "GenderFemaleN", "GenderFemale%")]
  
  df_int
}




check_ethnicity <- function(df_study, type){
  
  if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsRandomised"] <- "NoPtsTotal"
  }
  
  df_study$`White/Caucasian (%)` <- as.numeric(df_study$`White/Caucasian (%)`)
  df_study$`White/Caucasian (N)` <- as.numeric(df_study$`White/Caucasian (N)`)
  df_study$`Black or African-American (%)` <- as.numeric(df_study$`Black or African-American (%)`)
  df_study$`Black or African-American (N)` <- as.numeric(df_study$`Black or African-American (N)`)
  df_study$`Hispanic or Latinx (%)` <- as.numeric(df_study$`Hispanic or Latinx (%)`)
  df_study$`Hispanic or Latinx (N)` <- as.numeric(df_study$`Hispanic or Latinx (N)`)
  df_study$`Asian (%)` <- as.numeric( df_study$`Asian (%)`)
  df_study$`Asian (N)` <- as.numeric(df_study$`Asian (N)`)
  df_study$`American Indian or Pacific Islander (%)` <- as.numeric(df_study$`American Indian or Pacific Islander (%)`)
  df_study$`American Indian or Pacific Islander (N)` <- as.numeric(df_study$`American Indian or Pacific Islander (N)`)
  df_study$`Multiracial (%)` <- as.numeric(df_study$`Multiracial (%)`)
  df_study$`Multiracial (N)` <- as.numeric(df_study$`Multiracial (N)`)
  df_study$`Other (%)` <- as.numeric(df_study$`Other (%)`)
  df_study$`Other (N)` <- as.numeric(df_study$`Other (N)`)
  df_study$NoPtsTotal <- as.numeric(df_study$NoPtsTotal)
  
  df_study$WhitePercN <- NA
  df_study$BlackPercN <- NA
  df_study$HispPercN <- NA
  df_study$AsianPercN <- NA
  df_study$AmIndPercN <- NA
  df_study$MultiPercN <- NA
  df_study$OtherPercN <- NA
  
  conflict.PvsN.ids <- c()
  conflict.Ptot.ids <- c()
  conflict.Ntot.ids <- c()
  for(i in 1:nrow(df_study)){
    #Check N and % agree--------------------------------------------------------
    #White
    if(!is.na(df_study$`White/Caucasian (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$WhitePercN[i] <- (df_study$`White/Caucasian (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`White/Caucasian (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$WhitePercN[i] - df_study$`White/Caucasian (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #Black
    if(!is.na(df_study$`Black or African-American (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$BlackPercN[i] <- (df_study$`Black or African-American (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`Black or African-American (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$BlackPercN[i] - df_study$`Black or African-American (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #Hispanic
    if(!is.na(df_study$`Hispanic or Latinx (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$HispPercN[i] <- (df_study$`Hispanic or Latinx (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`Hispanic or Latinx (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$HispPercN[i] - df_study$`Hispanic or Latinx (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #Asian
    if(!is.na(df_study$`Asian (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$AsianPercN[i] <- (df_study$`Asian (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`Asian (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$AsianPercN[i] - df_study$`Asian (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #American indian
    if(!is.na(df_study$`American Indian or Pacific Islander (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$AmIndPercN[i] <- (df_study$`American Indian or Pacific Islander (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`American Indian or Pacific Islander (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$AmIndPercN[i] - df_study$`American Indian or Pacific Islander (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #Multi-racial
    if(!is.na(df_study$`Multiracial (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$MultiPercN[i] <- (df_study$`Multiracial (N)`[i]/df_study$NoPtsTotal[i])*100
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(!is.na(df_study$`Multiracial (%)`[i])){
        if(abs(df_study$MultiPercN[i] - df_study$`Multiracial (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    #Other
    if(!is.na(df_study$`Other (N)`[i]) && !is.na(df_study$NoPtsTotal[i])){
      df_study$OtherPercN[i] <- (df_study$`Other (N)`[i]/df_study$NoPtsTotal[i])*100
      if(!is.na(df_study$`Other (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$OtherPercN[i] - df_study$`Other (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
      }
    }
    
    #---------------------------------------------------------------------------
    
    #check percentages add to 100
    vec_perc <- c(df_study$`White/Caucasian (%)`[i], df_study$`Black or African-American (%)`[i],
                  df_study$`Hispanic or Latinx (%)`[i], df_study$`Asian (%)`[i],
                  df_study$`American Indian or Pacific Islander (%)`[i], df_study$`Multiracial (%)`[i],
                  df_study$`Other (%)`[i])
    sum_perc <- sum(vec_perc, na.rm = TRUE)
    if(!is.na(sum_perc) && sum_perc != 100 && sum_perc != 0){
      conflict.Ptot.ids <- append(conflict.Ptot.ids, df_study$StudyID[i])
    }
    
    
    #check numbers add to nTOT
    vec_num <- c(df_study$`White/Caucasian (N)`[i], df_study$`Black or African-American (N)`[i],
                  df_study$`Hispanic or Latinx (N)`[i], df_study$`Asian (N)`[i],
                  df_study$`American Indian or Pacific Islander (N)`[i], df_study$`Multiracial (N)`[i],
                  df_study$`Other (N)`[i])
    sum_num <- sum(vec_num, na.rm = TRUE)
    if(!is.na(sum_num) && !is.na(df_study$NoPtsTotal[i])){
      if(sum_num != df_study$NoPtsTotal[i] && sum_num !=0){
        conflict.Ntot.ids <- append(conflict.Ntot.ids, df_study$StudyID[i])
      }
    }
  }
  
  
  if(type=="study"){
    df_conflict.PvsN <- subset(df_study, StudyID %in% conflict.PvsN.ids)
    df_conflict.Ptot <- subset(df_study, StudyID %in% conflict.Ptot.ids)
    df_conflict.Ntot <- subset(df_study, StudyID %in% conflict.Ntot.ids)
    
    df_conflict.PvsN <- df_conflict.PvsN[c("StudyID", "StudyName", "NoPtsTotal",
                                           "White/Caucasian (N)", "White/Caucasian (%)", "WhitePercN",
                                           "Black or African-American (N)", "Black or African-American (%)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "Hispanic or Latinx (%)", "HispPercN",
                                           "Asian (N)","Asian (%)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)","American Indian or Pacific Islander (%)", "AmIndPercN",
                                           "Multiracial (N)","Multiracial (%)", "MultiPercN",
                                           "Other (N)", "Other (%)", "OtherPercN")]
    df_conflict.Ptot <- df_conflict.Ptot[c("StudyID", "StudyName",
                                          "White/Caucasian (%)","Black or African-American (%)",
                                           "Hispanic or Latinx (%)","Asian (%)", 
                                           "American Indian or Pacific Islander (%)",
                                           "Multiracial (%)","Other (%)")]
    df_conflict.Ntot <- df_conflict.Ntot[c("StudyID", "StudyName", "NoPtsTotal",
                                           "White/Caucasian (N)", "WhitePercN",
                                           "Black or African-American (N)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "HispPercN",
                                           "Asian (N)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)", "AmIndPercN",
                                           "Multiracial (N)", "MultiPercN",
                                           "Other (N)", "OtherPercN")]
  }else if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsTotal"] <- "NoPtsRandomised"
    df_conflict.PvsN <- subset(df_study, StudyID %in% conflict.PvsN.ids)
    df_conflict.Ptot <- subset(df_study, StudyID %in% conflict.Ptot.ids)
    df_conflict.Ntot <- subset(df_study, StudyID %in% conflict.Ntot.ids)
    
    df_conflict.PvsN <- df_conflict.PvsN[c("StudyID", "IntLabel", "NoPtsRandomised",
                                           "White/Caucasian (N)", "White/Caucasian (%)", "WhitePercN",
                                           "Black or African-American (N)", "Black or African-American (%)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "Hispanic or Latinx (%)", "HispPercN",
                                           "Asian (N)","Asian (%)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)","American Indian or Pacific Islander (%)", "AmIndPercN",
                                           "Multiracial (N)","Multiracial (%)", "MultiPercN",
                                           "Other (N)", "Other (%)", "OtherPercN")]
    df_conflict.Ptot <- df_conflict.Ptot[c("StudyID", "IntLabel",
                                           "White/Caucasian (%)","Black or African-American (%)",
                                           "Hispanic or Latinx (%)","Asian (%)", 
                                           "American Indian or Pacific Islander (%)",
                                           "Multiracial (%)","Other (%)")]
    df_conflict.Ntot <- df_conflict.Ntot[c("StudyID", "IntLabel", "NoPtsRandomised",
                                           "White/Caucasian (N)", "WhitePercN",
                                           "Black or African-American (N)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "HispPercN",
                                           "Asian (N)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)", "AmIndPercN",
                                           "Multiracial (N)", "MultiPercN",
                                           "Other (N)", "OtherPercN")]
  }
  res <- list(df_conflict.PvsN, df_conflict.Ptot, df_conflict.Ntot)
  res
}


check_ethnicity2 <- function(df_study, type){
  
  if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsRandomised"] <- "NoPtsTotal"
  }
  
  df_study$`White/Caucasian (%)` <- as.numeric(df_study$`White/Caucasian (%)`)
  df_study$`White/Caucasian (N)` <- as.numeric(df_study$`White/Caucasian (N)`)
  df_study$`Black or African-American (%)` <- as.numeric(df_study$`Black or African-American (%)`)
  df_study$`Black or African-American (N)` <- as.numeric(df_study$`Black or African-American (N)`)
  df_study$`Hispanic or Latinx (%)` <- as.numeric(df_study$`Hispanic or Latinx (%)`)
  df_study$`Hispanic or Latinx (N)` <- as.numeric(df_study$`Hispanic or Latinx (N)`)
  df_study$`Asian (%)` <- as.numeric( df_study$`Asian (%)`)
  df_study$`Asian (N)` <- as.numeric(df_study$`Asian (N)`)
  df_study$`American Indian or Pacific Islander (%)` <- as.numeric(df_study$`American Indian or Pacific Islander (%)`)
  df_study$`American Indian or Pacific Islander (N)` <- as.numeric(df_study$`American Indian or Pacific Islander (N)`)
  df_study$`Multiracial (%)` <- as.numeric(df_study$`Multiracial (%)`)
  df_study$`Multiracial (N)` <- as.numeric(df_study$`Multiracial (N)`)
  df_study$`Other (%)` <- as.numeric(df_study$`Other (%)`)
  df_study$`Other (N)` <- as.numeric(df_study$`Other (N)`)
  df_study$NoPtsTotal <- as.numeric(df_study$NoPtsTotal)
  
  df_study$SumEth <- rowSums(df_study[c("White/Caucasian (N)", "Black or African-American (N)",
    "Hispanic or Latinx (N)","Asian (N)", "American Indian or Pacific Islander (N)",
    "Multiracial (N)", "Other (N)")], na.rm=TRUE)
  
  df_study$WhitePercN <- (df_study$`White/Caucasian (N)`/df_study$SumEth)*100
  df_study$BlackPercN <- (df_study$`Black or African-American (N)`/df_study$SumEth)*100
  df_study$HispPercN <- (df_study$`Hispanic or Latinx (N)`/df_study$SumEth)*100
  df_study$AsianPercN <- (df_study$`Asian (N)`/df_study$SumEth)*100
  df_study$AmIndPercN <- (df_study$`American Indian or Pacific Islander (N)`/df_study$SumEth)*100
  df_study$MultiPercN <- (df_study$`Multiracial (N)`/df_study$SumEth)*100
  df_study$OtherPercN <- (df_study$`Other (N)`/df_study$SumEth)*100
  
  conflict.PvsN.ids <- c()
  conflict.Ptot.ids <- c()
  conflict.Ntot.ids <- c()
  for(i in 1:nrow(df_study)){
    #Check N and % agree--------------------------------------------------------
    #White
    if(!is.na(df_study$WhitePercN[i]) && !is.na(df_study$`White/Caucasian (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$WhitePercN[i] - df_study$`White/Caucasian (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    #Black
    if(!is.na(df_study$BlackPercN[i]) && !is.na(df_study$`Black or African-American (%)`[i])){
        #if they disagree (by more than 1%) then add to list of conflicting values
        if(abs(df_study$BlackPercN[i] - df_study$`Black or African-American (%)`[i])>1){
          conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
        }
    }
    #Hispanic
    if(!is.na(df_study$HispPercN[i]) && !is.na(df_study$`Hispanic or Latinx (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$HispPercN[i] - df_study$`Hispanic or Latinx (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    #Asian
    if(!is.na(df_study$AsianPercN[i]) && !is.na(df_study$`Asian (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$AsianPercN[i] - df_study$`Asian (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    #American indian
    if(!is.na(df_study$AmIndPercN[i]) && !is.na(df_study$`American Indian or Pacific Islander (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$AmIndPercN[i] - df_study$`American Indian or Pacific Islander (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    #Multi-racial
    if(!is.na(df_study$MultiPercN[i]) && !is.na(df_study$`Multiracial (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$MultiPercN[i] - df_study$`Multiracial (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    #Other
    if(!is.na(df_study$OtherPercN[i]) && !is.na(df_study$`Other (%)`[i])){
      #if they disagree (by more than 1%) then add to list of conflicting values
      if(abs(df_study$OtherPercN[i] - df_study$`Other (%)`[i])>1){
        conflict.PvsN.ids <- append(conflict.PvsN.ids, df_study$StudyID[i])
      }
    }
    
    #---------------------------------------------------------------------------
    
    #check percentages add to 100
    vec_perc <- c(df_study$`White/Caucasian (%)`[i], df_study$`Black or African-American (%)`[i],
                  df_study$`Hispanic or Latinx (%)`[i], df_study$`Asian (%)`[i],
                  df_study$`American Indian or Pacific Islander (%)`[i], df_study$`Multiracial (%)`[i],
                  df_study$`Other (%)`[i])
    sum_perc <- sum(vec_perc, na.rm = TRUE)
    if(!is.na(sum_perc) && sum_perc != 100 && sum_perc != 0){
      conflict.Ptot.ids <- append(conflict.Ptot.ids, df_study$StudyID[i])
    }
    
    #check numbers add to nTOT
    vec_num <- c(df_study$`White/Caucasian (N)`[i], df_study$`Black or African-American (N)`[i],
                 df_study$`Hispanic or Latinx (N)`[i], df_study$`Asian (N)`[i],
                 df_study$`American Indian or Pacific Islander (N)`[i], df_study$`Multiracial (N)`[i],
                 df_study$`Other (N)`[i])
    sum_num <- sum(vec_num, na.rm = TRUE)
    if(!is.na(sum_num) && !is.na(df_study$NoPtsTotal[i])){
      if(sum_num != df_study$NoPtsTotal[i] && sum_num !=0){
        conflict.Ntot.ids <- append(conflict.Ntot.ids, df_study$StudyID[i])
      }
    }
  }
  
  
  if(type=="study"){
    df_conflict.PvsN <- subset(df_study, StudyID %in% conflict.PvsN.ids)
    df_conflict.Ptot <- subset(df_study, StudyID %in% conflict.Ptot.ids)
    df_conflict.Ntot <- subset(df_study, StudyID %in% conflict.Ntot.ids)
    
    df_conflict.PvsN <- df_conflict.PvsN[c("StudyID", "StudyName", "NoPtsTotal","SumEth",
                                           "White/Caucasian (N)", "White/Caucasian (%)", "WhitePercN",
                                           "Black or African-American (N)", "Black or African-American (%)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "Hispanic or Latinx (%)", "HispPercN",
                                           "Asian (N)","Asian (%)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)","American Indian or Pacific Islander (%)", "AmIndPercN",
                                           "Multiracial (N)","Multiracial (%)", "MultiPercN",
                                           "Other (N)", "Other (%)", "OtherPercN")]
    df_conflict.Ptot <- df_conflict.Ptot[c("StudyID", "StudyName",
                                           "White/Caucasian (%)","Black or African-American (%)",
                                           "Hispanic or Latinx (%)","Asian (%)", 
                                           "American Indian or Pacific Islander (%)",
                                           "Multiracial (%)","Other (%)")]
    df_conflict.Ntot <- df_conflict.Ntot[c("StudyID", "StudyName", "NoPtsTotal", "SumEth")]
  }else if(type=="int"){
    names(df_study)[names(df_study) == "NoPtsTotal"] <- "NoPtsRandomised"
    df_conflict.PvsN <- subset(df_study, StudyID %in% conflict.PvsN.ids)
    df_conflict.Ptot <- subset(df_study, StudyID %in% conflict.Ptot.ids)
    df_conflict.Ntot <- subset(df_study, StudyID %in% conflict.Ntot.ids)
    
    df_conflict.PvsN <- df_conflict.PvsN[c("StudyID", "IntLabel", "NoPtsRandomised", "SumEth",
                                           "White/Caucasian (N)", "White/Caucasian (%)", "WhitePercN",
                                           "Black or African-American (N)", "Black or African-American (%)", "BlackPercN",
                                           "Hispanic or Latinx (N)", "Hispanic or Latinx (%)", "HispPercN",
                                           "Asian (N)","Asian (%)", "AsianPercN",
                                           "American Indian or Pacific Islander (N)","American Indian or Pacific Islander (%)", "AmIndPercN",
                                           "Multiracial (N)","Multiracial (%)", "MultiPercN",
                                           "Other (N)", "Other (%)", "OtherPercN")]
    df_conflict.Ptot <- df_conflict.Ptot[c("StudyID", "IntLabel",
                                           "White/Caucasian (%)","Black or African-American (%)",
                                           "Hispanic or Latinx (%)","Asian (%)", 
                                           "American Indian or Pacific Islander (%)",
                                           "Multiracial (%)","Other (%)")]
    df_conflict.Ntot <- df_conflict.Ntot[c("StudyID", "IntLabel", "NoPtsRandomised","SumEth")]
  }
  res <- list(df_conflict.PvsN, df_conflict.Ptot, df_conflict.Ntot)
  res
}


check_ethnic_int <- function(df_study, df_int){
  miss.study.eth.id <- c()
  
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$`White/Caucasian (N)`[i]) &&
       is.na(df_study$`White/Caucasian (%)`[i]) &&
       is.na(df_study$`Black or African-American (N)`[i]) &&
       is.na(df_study$`Black or African-American (%)`[i]) &&
       is.na(df_study$`Hispanic or Latinx (N)`[i]) &&
       is.na(df_study$`Hispanic or Latinx (%)`[i]) &&
       is.na(df_study$`Asian (N)`[i]) &&
       is.na(df_study$`Asian (%)`[i]) &&
       is.na(df_study$`American Indian or Pacific Islander (N)`[i]) &&
       is.na(df_study$`American Indian or Pacific Islander (%)`[i]) &&
       is.na(df_study$`Multiracial (N)`[i]) &&
       is.na(df_study$`Multiracial (%)`[i]) &&
       is.na(df_study$`Other (N)`[i]) &&
       is.na(df_study$`Other (%)`[i])){
      miss.study.eth.id <- append(miss.study.eth.id, df_study$StudyID[i])
    } 
  }
  
  #df_int - keep only studies with missing eth
  df_int <- subset(df_int, StudyID %in% miss.study.eth.id)
  
  miss.int.eth.id <- c()
  if(nrow(df_int)>0){
    for(i in 1:nrow(df_int)){
      if(is.na(df_int$`White/Caucasian (N)`[i]) &&
         is.na(df_int$`White/Caucasian (%)`[i]) &&
         is.na(df_int$`Black or African-American (N)`[i]) &&
         is.na(df_int$`Black or African-American (%)`[i]) &&
         is.na(df_int$`Hispanic or Latinx (N)`[i]) &&
         is.na(df_int$`Hispanic or Latinx (%)`[i]) &&
         is.na(df_int$`Asian (N)`[i]) &&
         is.na(df_int$`Asian (%)`[i]) &&
         is.na(df_int$`American Indian or Pacific Islander (N)`[i]) &&
         is.na(df_int$`American Indian or Pacific Islander (%)`[i]) &&
         is.na(df_int$`Multiracial (N)`[i]) &&
         is.na(df_int$`Multiracial (%)`[i]) &&
         is.na(df_int$`Other (N)`[i]) &&
         is.na(df_int$`Other (%)`[i])){
        miss.int.eth.id <- append(miss.int.eth.id, df_int$StudyID[i])
      } 
    }
  }
  
  df_int <- subset(df_int, !(StudyID %in% miss.int.eth.id))
  df_int <- df_int[c("StudyID", "IntLabel", "NoPtsRandomised",
                     "White/Caucasian (N)", "White/Caucasian (%)",
                     "Black or African-American (N)", "Black or African-American (%)",
                     "Hispanic or Latinx (N)", "Hispanic or Latinx (%)",
                     "Asian (N)","Asian (%)", 
                     "American Indian or Pacific Islander (N)","American Indian or Pacific Islander (%)", 
                     "Multiracial (N)","Multiracial (%)",
                     "Other (N)", "Other (%)")]
  
  df_int
}



check_missing_sex <- function(df_study, df_int){
  miss.study.gender.id <- c()
  
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$`GenderMale%`[i]) && is.na(df_study$`GenderFemale%`[i]) && 
       is.na(df_study$`GenderMaleN`[i]) && is.na(df_study$`GenderFemaleN`[i])){
      miss.study.gender.id <- append(miss.study.gender.id, df_study$StudyID[i])
    } 
  }
  
  #df_int - keep only studies with missing gender
  df_int <- subset(df_int, StudyID %in% miss.study.gender.id)
  
  miss.int.gender.id <- c()
  if(nrow(df_int)>0){
    for(i in 1:nrow(df_int)){
      if(is.na(df_int$`GenderMale%`[i]) && is.na(df_int$`GenderFemale%`[i]) && 
         is.na(df_int$`GenderMaleN`[i]) && is.na(df_int$`GenderFemaleN`[i])){
        miss.int.gender.id <- append(miss.int.gender.id, df_int$StudyID[i])
      } 
    }
  }
  
  #list(miss.study.gender.id, miss.int.gender.id)
  df.study.miss.sex <- subset(df_study, StudyID %in% miss.study.gender.id)
  df.study.miss.sex <- df.study.miss.sex[c("StudyID", "StudyName")]
  
  
  df.int.miss.sex <- subset(df_int, StudyID %in% miss.int.gender.id)
  df.int.miss.sex <- df.int.miss.sex[c("StudyID", "IntLabel", "GenderMale%", "GenderFemale%", 
                                       "GenderMaleN", "GenderFemaleN")]
  names_sex <- c()
  for(i in 1:nrow(df.int.miss.sex)){
    ind <- which(df_study$StudyID == df.int.miss.sex$StudyID[i])
    if(length(ind)>0){
      names_sex <- c(names_sex, df_study$`StudyName`[ind[1]])
    }else{
      names_sex <- c(names_sex, NA)
    }
  }
  df.int.miss.sex$`StudyName` <- names_sex
  
  list(df.study.miss.sex, df.int.miss.sex)
}

check_missing_age <- function(df_study, df_int){
  miss.study.age.id <- c()
  
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$AgeMean[i]) && is.na(df_study$AgeMedian[i])){
      if(is.na(df_study$AgeLowRange[i]) || is.na(df_study$AgeUpRange[i])){
        miss.study.age.id <- append(miss.study.age.id, df_study$StudyID[i])
      }
    } 
  }
  
  #df_int - keep only studies with missing age
  df_int <- subset(df_int, StudyID %in% miss.study.age.id)
  
  miss.int.age.id <- c()
  if(nrow(df_int)>0){
    for(i in 1:nrow(df_int)){
      if(is.na(df_int$AgeMean[i]) && is.na(df_int$AgeMedian[i])){
        if(is.na(df_int$AgeLowRange[i]) || is.na(df_int$AgeUpRange[i])){
          miss.int.age.id <- append(miss.int.age.id, df_int$StudyID[i])
        }
      } 
    }
  }
  
  #list(miss.study.age.id, miss.int.age.id)
  df.study.miss.age <- subset(df_study, StudyID %in% miss.study.age.id)
  df.study.miss.age <- df.study.miss.age[c("StudyID", "StudyName", "AgeMean",
                                           "AgeSD",	"AgeMedian",	"AgeIQR",	"AgeLowRange",
                                           "AgeUpRange", "AgeOtherData")]
  
  
  df.int.miss.age <- subset(df_int, StudyID %in% miss.int.age.id)
  df.int.miss.age <- df.int.miss.age[c("StudyID", "IntLabel", "AgeMean", "AgeSD",	"AgeMedian",	
                                       "AgeIQR",	"AgeLowRange", "AgeUpRange", "AgeOther")]
  names_age <- c()
  for(i in 1:nrow(df.int.miss.age)){
    ind <- which(df_study$StudyID == df.int.miss.age$StudyID[i])
    if(length(ind)>0){
      names_age <- c(names_age, df_study$`StudyName`[ind[1]])
    }else{
      names_age <- c(names_age, NA)
    }
  }
  df.int.miss.age$`StudyName` <- names_age
  
  list(df.study.miss.age, df.int.miss.age)
  
}

check_missing_ethnic <- function(df_study, df_int){
  miss.study.eth.id <- c()
  
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$`White/Caucasian (N)`[i]) &&
       is.na(df_study$`White/Caucasian (%)`[i]) &&
       is.na(df_study$`Black or African-American (N)`[i]) &&
       is.na(df_study$`Black or African-American (%)`[i]) &&
       is.na(df_study$`Hispanic or Latinx (N)`[i]) &&
       is.na(df_study$`Hispanic or Latinx (%)`[i]) &&
       is.na(df_study$`Asian (N)`[i]) &&
       is.na(df_study$`Asian (%)`[i]) &&
       is.na(df_study$`American Indian or Pacific Islander (N)`[i]) &&
       is.na(df_study$`American Indian or Pacific Islander (%)`[i]) &&
       is.na(df_study$`Multiracial (N)`[i]) &&
       is.na(df_study$`Multiracial (%)`[i]) &&
       is.na(df_study$`Other (N)`[i]) &&
       is.na(df_study$`Other (%)`[i])){
      miss.study.eth.id <- append(miss.study.eth.id, df_study$StudyID[i])
    } 
  }
  
  #df_int - keep only studies with missing eth
  df_int <- subset(df_int, StudyID %in% miss.study.eth.id)
  
  miss.int.eth.id <- c()
  if(nrow(df_int)>0){
    for(i in 1:nrow(df_int)){
      if(is.na(df_int$`White/Caucasian (N)`[i]) &&
         is.na(df_int$`White/Caucasian (%)`[i]) &&
         is.na(df_int$`Black or African-American (N)`[i]) &&
         is.na(df_int$`Black or African-American (%)`[i]) &&
         is.na(df_int$`Hispanic or Latinx (N)`[i]) &&
         is.na(df_int$`Hispanic or Latinx (%)`[i]) &&
         is.na(df_int$`Asian (N)`[i]) &&
         is.na(df_int$`Asian (%)`[i]) &&
         is.na(df_int$`American Indian or Pacific Islander (N)`[i]) &&
         is.na(df_int$`American Indian or Pacific Islander (%)`[i]) &&
         is.na(df_int$`Multiracial (N)`[i]) &&
         is.na(df_int$`Multiracial (%)`[i]) &&
         is.na(df_int$`Other (N)`[i]) &&
         is.na(df_int$`Other (%)`[i])){
        miss.int.eth.id <- append(miss.int.eth.id, df_int$StudyID[i])
      } 
    }
  }
  
  #list(miss.study.eth.id, miss.int.eth.id)
  df.study.miss.eth <- subset(df_study, StudyID %in% miss.study.eth.id)
  df.study.miss.eth <- df.study.miss.eth[c("StudyID", "StudyName", "Ethnicity - Other",
                                           "%MajorityEthnicGroup")]
  
  
  df.int.miss.eth <- subset(df_int, StudyID %in% miss.int.eth.id)
  df.int.miss.eth <- df.int.miss.eth[c("StudyID", "IntLabel", "Ethnicity - Other",
                                       "%MajorityEthnicGroup")]
  names_eth <- c()
  for(i in 1:nrow(df.int.miss.eth)){
    ind <- which(df_study$StudyID == df.int.miss.eth$StudyID[i])
    if(length(ind)>0){
      names_eth <- c(names_eth, df_study$`StudyName`[ind[1]])
    }else{
      names_eth <- c(names_eth, NA)
    }
  }
  df.int.miss.eth$`StudyName` <- names_eth
  
  list(df.study.miss.eth, df.int.miss.eth)
}

inspect_ICC <- function(df){
  #df is merged study/outcome/int
  df <- subset(df, StudyDesign=="Cluster RCT")
  
  df <- subset(df, !(is.na(ActualICC) & is.na(AssumedICC)))
  df <- subset(df, !(ActualICC=="Not reported" & AssumedICC=="Not reported"))
  df <- df[c("StudyID", "StudyName", "UnitRand", "IntLabel", "ActualICC", "AssumedICC")]
  df
}

inspect_unitrand <- function(df){
  #one row per study
  df <- df %>% distinct(StudyID, .keep_all = TRUE)
  unit.rand <- unique(df$UnitRand)
  unit.rand <- unit.rand[!is.na(unit.rand)]
  counts <- numeric(length = 0L)
  for(i in 1:length(unit.rand)){
    count <- sum(df$UnitRand==unit.rand[i], na.rm = TRUE)
    counts <- append(counts, count)
  }
  df.unitrand <- as.data.frame(cbind(unit.rand, counts))
  
  df.unitrand
}

fix_cluster <- function(df){
  #set NR & Not reported (non-numeric) to NA 
  #check non numeric
  for(i in 1:nrow(df)){
    if(!is.na(df$NoClusters[i])){
      if(df$NoClusters[i]=="NR" || df$NoClusters[i]=="Not reported"){
        df$NoClusters[i]<- NA
      }else if(is.na(as.numeric(df$NoClusters[i]))){
        df$NoClusters[i] <- as.numeric(str_extract(df$NoClusters[i], "\\d+"))
        print(paste("NoClusters not numeric", df$StudyID[i], df$StudyName[i], df$IntLabel[i]))
      }
    }
    if(!is.na(df$ArmClusters[i])){
      if(df$ArmClusters[i]=="NR" || df$ArmClusters[i]=="Not reported"){
        df$ArmClusters[i]<- NA
      }else if(is.na(as.numeric(df$ArmClusters[i]))){
        df$ArmClusters[i] <- as.numeric(str_extract(df$ArmClusters[i], "\\d+"))
        print(paste("ArmClusters not numeric", df$StudyID[i], df$StudyName[i], df$IntLabel[i]))
      }
    }
    
  }
  
  df$NoClusters <- as.numeric(df$NoClusters)
  df$ArmClusters <- as.numeric(df$ArmClusters)
  
  df
}

check_cluster <- function(df){
  df <- subset(df, StudyDesign=="Cluster RCT")
  df <- df[c("StudyID", "StudyName", "UnitRand","NoPtsTotal", "NoClusters",
             "IntLabel", "ArmClusters", "n", "r")]
  
  #check for missing data
  missing_arm_nc <- c()
  missing_all_nc <- c()
  for(i in 1:nrow(df)){
    if(is.na(df$ArmClusters[i])){
      missing_arm_nc <- c(missing_arm_nc, df$StudyID[i])
      if(is.na(df$NoClusters[i])){
        missing_all_nc <- c(missing_all_nc, df$StudyID[i])
      }
    }
  }
  
  #set NR & Not reported (non-numeric) to NA 
  #check non numeric
  # for(i in 1:nrow(df)){
  #   if(!is.na(df$NoClusters[i])){
  #     if(df$NoClusters[i]=="NR" || df$NoClusters[i]=="Not reported"){
  #       df$NoClusters[i]<- NA
  #     }else if(is.na(as.numeric(df$NoClusters[i]))){
  #       df$NoClusters[i] <- as.numeric(str_extract(df$NoClusters[i], "\\d+"))
  #       print(paste("NoClusters not numeric", df$StudyID[i], df$StudyName[i], df$IntLabel[i]))
  #     }
  #   }
  #   if(!is.na(df$ArmClusters[i])){
  #     if(df$ArmClusters[i]=="NR" || df$ArmClusters[i]=="Not reported"){
  #       df$ArmClusters[i]<- NA
  #     }else if(is.na(as.numeric(df$ArmClusters[i]))){
  #       df$ArmClusters[i] <- as.numeric(str_extract(df$ArmClusters[i], "\\d+"))
  #       print(paste("ArmClusters not numeric", df$StudyID[i], df$StudyName[i], df$IntLabel[i]))
  #     }
  #   }
  #   
  # }
  
  #check for disagreement
  disagree_nc <- c()
  sum_arm_nc <- c()
  df_uniq_study <- df %>% distinct(StudyID, .keep_all = TRUE)
  for(i in 1:nrow(df_uniq_study)){
    df_i <- subset(df, StudyID==df_uniq_study$StudyID[i])
    #print(df_uniq_study$StudyID[i])
    
    if(!is.na(df_uniq_study$NoClusters[i]) && all(!is.na(df_i$ArmClusters))){
      
      # if(any(is.na(as.numeric(df_i$ArmClusters)))){
      #   df_i$ArmClusters <- as.numeric(str_extract(df_i$ArmClusters, "\\d+"))
      #   print(paste("ArmClusters not numeric", df_uniq_study$StudyID[i], df_uniq_study$StudyName[i]))
      # }
      # if(is.na(as.numeric(df_uniq_study$NoClusters[i]))){
      #   df_uniq_study$NoClusters[i] <- as.numeric(str_extract(df_uniq_study$NoClusters[i], "\\d+"))
      #   print(paste("NoClusters not numeric", df_uniq_study$StudyID[i], df_uniq_study$StudyName[i]))
      # }
      
      if(sum(as.numeric(df_i$ArmClusters)) != as.numeric(df_uniq_study$NoClusters[i])){
        disagree_nc <- c(disagree_nc, df_uniq_study$StudyID[i])
        sum_arm_nc <- c(sum_arm_nc, sum(as.numeric(df_i$ArmClusters)))
      }
    }
  }
  
  df.miss.arm.nc <- subset(df, StudyID %in% missing_arm_nc)
  df.miss.all.nc <- subset(df, StudyID %in% missing_all_nc)
  df.disagree.nc <- subset(df, StudyID %in% disagree_nc)
  
  df.sum.arm.nc <- as.data.frame(cbind(disagree_nc, sum_arm_nc))
  names(df.sum.arm.nc)[names(df.sum.arm.nc) == "disagree_nc"] <- "StudyID"
  
  df.disagree.nc <- merge(df.disagree.nc, df.sum.arm.nc, by = c("StudyID")) 
  
  list(df.miss.arm.nc, df.miss.all.nc, df.disagree.nc)
}

check_control <- function(df_int){
  missing_cont_ids <- c()
  for(i in 1:nrow(df_int)){
    out_split <- strsplit(df_int$IntLabel[i], split = " ")
    if(out_split[[1]][1] == "Control"){
      if(df_int$StandardCareYN[i]=="FALSE" && df_int$NoInterventionYN[i]=="FALSE"
         && df_int$AttentionPlaceboYN[i]=="FALSE"){
        print(df_int$StudyID[i])
        missing_cont_ids <- c(missing_cont_ids, df_int$StudyID[i])
      }
    }
  }
  missing_cont_ids
}

check_underserved <- function(df_study){
  
  df_under_mult <- data.frame("StudyID" = character(0), "StudyName" = character(0), "UnderservedCat" = character(0))
  df_under_other <- data.frame("StudyID" = character(0), "StudyName" = character(0), "UnderservedCat" = character(0))
  underserved <- c("Gypsy, Roma or Traveller communities", "Homeless", 
                   "Minority Ethnic Groups", "People w/ Substance Abuse Disorders",
                   "Sex Workers", "Socially Economically Deprived",
                   "Vulnerable Migrants", "Other")
  for(i in 1:nrow(df_study)){
    if(df_study$SpecificPopUnderserved[i]=="TRUE"){
      if(is.na(df_study$SpecificPopUnderservedCategory[i])){
        print(paste("Study:",df_study$StudyID[i], df_study$StudyName[i], "Underserved category: NA"))
      }else if(df_study$SpecificPopUnderservedCategory[i]=="Other"){
        #print(paste("Study:",df_study$StudyID[i], df_study$`Author, Year`[i], "Other underserved:", df_study$SpecificPopUnderservedOther[i]))
        row <- c(df_study$StudyID[i], df_study$StudyName[i], df_study$SpecificPopUnderservedOther[i])
        df_under_other <- rbind(df_under_other, row)
      }else if(!(df_study$SpecificPopUnderservedCategory[i] %in% underserved)){
        #print(paste("Study:",df_study$StudyID[i], df_study$`Author, Year`[i], "Multiple:", df_study$SpecificPopUnderservedCategory[i]))
        row <- c(df_study$StudyID[i], df_study$StudyName[i], df_study$SpecificPopUnderservedCategory[i])
        df_under_mult <- rbind(df_under_mult, row)
      }
    }
  }
  colnames(df_under_mult) <- c("StudyID", "StudyName", "Underserved Multiple")
  colnames(df_under_other) <- c("StudyID", "StudyName", "Underserved Other")
  
  print(paste("number underserved = ", sum(df_study$SpecificPopUnderserved=="TRUE")))
  df_under_count <- data.frame("Category" = character(0), "Count" = numeric(0))
  for(i in 1:length(underserved)){
    num <- sum(df_study$SpecificPopUnderservedCategory==underserved[i], na.rm = TRUE)
    print(paste(underserved[i], num))
    row <- c(underserved[i], num)
    df_under_count <- rbind(df_under_count, row)
  }
  df_under_count <- rbind(df_under_count, c("Multiple", nrow(df_under_mult)))
  colnames(df_under_count) <- c("Category", "Count")
  ls <- list(df_under_mult, df_under_other, df_under_count)
  ls
}