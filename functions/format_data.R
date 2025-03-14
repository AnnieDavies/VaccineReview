format_study <- function(df_study, df_int){

  #2.1a Categorise setting
  df_study <- setting_CAT(df_study)
  
  #2.1b calculate proportion male 
  df_study <- calcPropMale(df_study)
  
  #2.1c re-define environment setting
  NR.env <- 0
  names(df_study)[names(df_study) == "Environment setting"] <- "Environment"
  env <- c("Urban", "Rural", "Mixed")
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$Environment[i]) || df_study$Environment[i]=="Not reported"){
      df_study$Environment[i] <- "Mixed"
      NR.env <- NR.env + 1
    }else if(!(df_study$Environment[i] %in% env)){
      print(paste("Environment incorrect: Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Row:", i, "Environment:", df_study$Environment[i]))
    }
  }
  print(paste("Number of studies that don't report environment:", NR.env))
  
  #2.1d Age
  #vaccine categories
  df_study <- vacc_CAT(df_study)
  #age categories 
  df_study <- age_CAT(df_study)
  
  #**ROB is missing
  
  
  #2.1e Ethnicity - calculate proportion
  df_study <- calcPropEthnicity(df_study)
  
  #2.1f Underserved population
  #identify multiple
  for(i in 1:nrow(df_study)){
    underserved <- c("Gypsy, Roma or Traveller communities", "Homeless", 
                     "Minority Ethnic Groups", "People w/ Substance Abuse Disorders",
                     "Sex Workers", "Socially Economically Deprived",
                     "Vulnerable Migrants", "Other")
    if(df_study$SpecificPopUnderserved[i]=="TRUE"){
      if(is.na(df_study$SpecificPopUnderservedCategory[i])){
        print(paste("Study:",df_study$StudyID[i], df_study$StudyName[i], "Underserved category: NA"))
      }else if(df_study$SpecificPopUnderservedCategory[i]=="Other"){
        print(paste("Study:",df_study$StudyID[i], df_study$StudyName[i], "Other underserved:", df_study$SpecificPopUnderservedOther[i]))
      }else if(!(df_study$SpecificPopUnderservedCategory[i] %in% underserved)){
        print(paste("Study:",df_study$StudyID[i], df_study$StudyName[i], "Multiple:", df_study$SpecificPopUnderservedCategory[i]))
      }
    }
  }
  
  #2.1g Pre-post covid indicator
  df_study <- covid_ind(df_study)
  print(paste("Number of post covid studies = ", sum(df_study$Covid=="Post")))
  
  
  #calculate number of arms from rows of df_int
  df_study$NoArmsAnalysed <- NA
  for(i in 1:nrow(df_study)){
    df_study$NoArmsAnalysed[i] <- sum(df_int$StudyID == df_study$StudyID[i])
  }
  
  #Select relevant columns only
  df_study <- df_study[c("StudyID", "StudyName", "Year", "Covid", "StudyDesign", "NoPtsTotal", 
                         "NoArms", "NoArmsAnalysed", "NoClusters", "UnitRand", "ActualICC", "AssumedICC", 
                         "VaccineType", "AgeGroup", "OtherAge", "Setting", "MultiSetting",
                         "Environment", "PropMale", "SpecificPopUnderserved", "SpecificPopUnderservedCategory", "SpecificPopUnderservedOther",
                         "PropWhite", "PropBlack", "PropHisp", "PropAsian", "PropAmInd", 
                         "PropMulti", "PropOther", "Ethnicity - Other", "Country")]
  df_study
}


#Categorise setting
setting_CAT <- function(df_study){
  #checked manually 15/02/24
  
  df_study$MultiSetting <- NA
  for(i in 1:nrow(df_study)){
    health <- c("Pharmacy", "Primary care", "Clinic", "Hospital outpatient clinic", "Healthcare - other")
    educ <- c("School", "University", "Education - other")
    comm <- c("Community", "Mass vaccination centre", "Other", "Workplace", "Community - other")
    
    if(!is.na(df_study$Setting[i])){
      if(df_study$Setting[i] %in% health){ df_study$Setting[i] <- "Healthcare" }
      else if(df_study$Setting[i] %in% educ){ df_study$Setting[i] <- "Education" }
      else if(df_study$Setting[i] %in% comm){ df_study$Setting[i]<- "Community/Other"}
      else if(df_study$Setting[i]=="Online"){ df_study$Setting[i] <- "Online" }
      else{
        
        #categorize multiple
        if(grepl(",", df_study$Setting[i])){
          out_split <- strsplit(df_study$Setting[i], split = ", ")
        }else if(grepl(";", df_study$Setting[i])){
          out_split <- strsplit(df_study$Setting[i], split = ";")
        }else{
          print(paste("Incorrect setting format - Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Row:", i, "Setting:", df_study$Setting[i]))
          out_split <- strsplit(df_study$Setting[i], split = ", ") #so code continues
        }
        vec_split <- out_split[[1]]
        #If all elements are from the same category - set equal to that category
        if(all(vec_split %in% health)){ df_study$Setting[i] <- "Healthcare" }
        else if(all(vec_split %in% educ)){ df_study$Setting[i] <- "Education" }
        else if(all(vec_split %in% comm)){ df_study$Setting[i] <- "Community/Other" }
        else{
          df_study$Setting[i] <- "Multiple"
          for(j in 1:length(vec_split)){
            if(vec_split[j] %in% health){ vec_split[j] <- "Healthcare" }
            else if(vec_split[j] %in% educ){ vec_split[j] <- "Education" }
            else if(vec_split[j] %in% comm){ vec_split[j]<- "Community/Other" }
          }
          #now remove duplicates
          vec_split <- unique(vec_split)
          #now order alphabetically
          vec_split <- sort(vec_split)
          
          df_study$MultiSetting[i] <- paste(vec_split, collapse = ", ")
          print(paste("Multi-setting:",  df_study$StudyID[i], df_study$StudyName[i], 
                      df_study$MultiSetting[i]))
        }
      }
    }else{
      print(paste("Setting NR: Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Row:", i))
    }
  }
  df_study
}


#Calculate proportion male
calcPropMale <- function(df_study){
  #(checked 15/02/24)
  NR.sex <- 0
  df_study$PropMale <- NA
  for(i in 1:nrow(df_study)){
    if(!is.na(df_study$`GenderMaleN`[i]) && !is.na(df_study$`GenderFemaleN`[i])){
      nMF <- df_study$`GenderMaleN`[i] + df_study$`GenderFemaleN`[i]
      df_study$PropMale[i] <- df_study$`GenderMaleN`[i]/nMF
    }else if(!is.na(df_study$`GenderMale%`[i])){
      df_study$PropMale[i] <- df_study$`GenderMale%`[i]/100
      
      if(!is.na(df_study$`GenderFemale%`[i])){
        percTOT <- df_study$`GenderMale%`[i] + df_study$`GenderFemale%`[i]
        if(percTOT > 100.2 || percTOT < 99.8){
          print(paste("Study", df_study$StudyID[i], df_study$StudyName[i], ": Perc M/F sums to", percTOT))
        }
      }
      
    }else if(!is.na(df_study$`GenderFemale%`[i])){
      df_study$PropMale[i] <- 1 - (df_study$`GenderFemale%`[i]/100)
    }else if(!is.na(df_study$`GenderMaleN`[i])){
      df_study$PropMale[i] <- df_study$`GenderMaleN`[i]/df_study$NoPtsTotal[i]
    }else if(!is.na(df_study$`GenderFemaleN`[i])){
      df_study$PropMale[i] <- 1 - (df_study$`GenderFemaleN`[i]/df_study$NoPtsTotal[i])
    }else{
      #use intervention level information
      df_int_i <- subset(df_int, StudyID==df_study$StudyID[i])
      
      if(all(!is.na(df_int_i$GenderMaleN)) && all(!is.na(df_int_i$GenderFemaleN))){
        nmale <- sum(as.numeric(df_int_i$GenderMaleN))
        nfemale <- sum(as.numeric(df_int_i$GenderFemaleN))
        ntot <- nmale + nfemale
        df_study$PropMale[i] <- nmale/ntot
      }else if(all(!is.na(df_int_i$`GenderMale%`))){
        df_study$PropMale[i] <- mean(as.numeric(df_int_i$`GenderMale%`))/100
        
        if(all(!is.na(df_int_i$`GenderFemale%`))){
          percTOT <- mean(as.numeric(df_int_i$`GenderMale%`)) + mean(as.numeric(df_int_i$`GenderFemale%`))
          if(percTOT > 100.2 || percTOT < 99.8){
            print(paste("Study", df_study$StudyID[i], df_study$StudyName[i], ": intervention Perc M/F sums to", percTOT))
          }
        }
        
      }else if(all(!is.na(df_int_i$`GenderFemale%`))){
        df_study$PropMale[i] <- 1 - mean(as.numeric(df_int_i$`GenderFemale%`))/100
      }else if(all(!is.na(df_int_i$GenderMaleN))){
        nmale <- sum(as.numeric(df_int_i$GenderMaleN))
        ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
        df_study$PropMale[i] <- nmale/ntot
      }
      else if(all(!is.na(df_int_i$GenderFemaleN))){
        nfemale <- sum(as.numeric(df_int_i$GenderFemaleN))
        ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
        df_study$PropMale[i] <- 1-nfemale/ntot
      }else{
        #or assume 50/50
        df_study$PropMale[i] <- 0.5
        NR.sex <- NR.sex + 1
      }
    }
  }
  
  
  print(paste("Number of studies that don't report sex:", NR.sex))
  df_study
}


#Categorise vaccine
vacc_CAT <- function(df_study){
  df_study$VaccineType <- df_study$Vaccine
  
  vacc <- c("Childhood Vaccines", "BCG Tuberculosis", "COVID-19", "Hepatitis A", 
            "Hepatitis B", "HPV", "Influenza", "MenACWY", "Pneumo", "Shingles")
  
  childhood <- c("Childhood Vaccines", "Measles", "Rubella", "Diptheria",
                 "Tetanus", "Pertussis", "Polio", "Rotavirus",
                 "Haemophilus influenzae type B (Hib B)", "MenB", "MenC")
  pneumo <- c("PCV13", "PCV23/PPV")
  
  
  for(i in 1:nrow(df_study)){
    if(is.na(df_study$Vaccine[i])){
      print(paste("NA vaccine:", df_study$Vaccine[i], "StudyID:", df_study$StudyID[i], df_study$StudyName[i], "Row", i))
    }
    
    if(grepl(",", df_study$Vaccine[i])){
      out_split <- strsplit(df_study$Vaccine[i], split = ", ")
    }else if(grepl(";", df_study$Vaccine[i])){
      out_split <- strsplit(df_study$Vaccine[i], split = ";")
    }else{
      #print(paste(i, "Vaccine not split:", df_study$Vaccine[i]))
      out_split <- strsplit(df_study$Vaccine[i], split = ", ") #so code continues
    }
    vec_split <- out_split[[1]]
    
    #remove duplicates
    vec_split <- unique(vec_split)
    #order alphabetically
    vec_split <- sort(vec_split)
    df_study$Vaccine[i] <- paste(vec_split, collapse = ", ")
    
    if(length(vec_split)>0){
      for(j in 1:length(vec_split)){
        if(vec_split[j]%in%childhood){
          vec_split[j] <- "Childhood Vaccines"
        }else if(vec_split[j]%in%pneumo){
          vec_split[j] <- "Pneumo"
        }
      }
    }
    
    #remove duplicates
    vec_split <- unique(vec_split)
    #order alphabetically
    vec_split <- sort(vec_split)
    df_study$VaccineType[i] <- paste(vec_split, collapse = ", ")
  }
  
  df_study
}


#Categorise age
age_CAT <- function(df_study){
  
  df_study$OtherAge <- NA
  ages <- c("Young children", "Children", "Adolescents", "Adults", "Older adults", "Pregnant women")
  adults <- c("Adolescents", "Adults", "Older adults")
  children <- c("Young children", "Adolescents")
  preg_adults <- c("Adults", "Adolescents")
  young_adults <- c("Adolescents", "Adults")
  
  seasonal <- c("Influenza", "COVID-19")
  
  for(i in 1:nrow(df_study)){
    if(grepl(",", df_study$AgeGroup[i])){
      out_split <- strsplit(df_study$AgeGroup[i], split = ", ")
    }else if(grepl(";", df_study$AgeGroup[i])){
      out_split <- strsplit(df_study$AgeGroup[i], split = ";")
    }else{
      out_split <- strsplit(df_study$AgeGroup[i], split = ", ") #so code continues
    }
    age_split <- out_split[[1]]
    #remove duplicates
    age_split <- unique(age_split)
    #order alphabetically
    age_split <- sort(age_split)
    
    vac_split_out <- strsplit(df_study$VaccineType[i], split = ", ")
    vac_split <- vac_split_out[[1]]
    
    #define pregnant women
    if(df_study$SpecificPopPregnant[i]=="TRUE"){
      if(all(age_split %in% preg_adults)){ 
        df_study$AgeGroup[i] <- "Pregnant women" 
      }else if(df_study$AgeGroup[i] == "Young children"){
        df_study$AgeGroup[i] <- df_study$AgeGroup[i]
      }else{
          print(paste("Incorrect pregnant age group - Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Row:", i, "Age group:", df_study$AgeGroup[i]))
      }
    }
    
    #Redefine children based on vaccine group
    if(df_study$AgeGroup[i]=="Young children"){
      if(vac_split=="Childhood Vaccines"){
        df_study$AgeGroup[i] <- "Young children"
      }else if(all(vac_split %in% seasonal)){
        df_study$AgeGroup[i] <- "Children"
      }else{
        print(paste("Young children - not childhood or seasonal. Study:", df_study$StudyID[i], df_study$StudyName[i],
              "VaccineType:", df_study$VaccineType[i]))
      }
    }else if(length(age_split)>1 && all(age_split %in% children)){
      if(all(vac_split %in% seasonal)){
        df_study$AgeGroup[i] <- "Children"
      }else{
        print(paste("Young children & Adolescents - not seasonal. Study:", df_study$StudyID[i], df_study$StudyName[i],
              "VaccineType:", df_study$VaccineType[i], "Age:", df_study$AgeGroup[i]))
      }
    }
    
    
    #categorize multiple groups
    if(!(df_study$AgeGroup[i] %in% ages)){
      if(identical(age_split, young_adults) && vac_split=="HPV"){
        df_study$AgeGroup[i] <- "Adolescents"
      }else if(all(age_split %in% adults)){ 
        df_study$AgeGroup[i] <- "Adults" 
      }else if(df_study$StudyID[i]=="11765"){
        df_study$AgeGroup[i] <- "Adults" 
      }else{
        df_study$AgeGroup[i] <- "Uncategorized"
        df_study$OtherAge[i] <- paste(age_split, collapse = ", ")
        print(paste("Mixed/Uncategorized age group - Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], 
                    "Row:", i, "Age group:", df_study$OtherAge[i], "Vaccine:", df_study$VaccineType[i]))
      }
    }
    
  }
  df_study
}

calcPropEthnicity <- function(df_study){
  df_study$PropWhite <- NA
  df_study$PropBlack <- NA
  df_study$PropHisp <- NA
  df_study$PropAsian <- NA
  df_study$PropAmInd <- NA
  df_study$PropMulti <- NA
  df_study$PropOther <- NA
  
  #make sure everything is stored as numeric:
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
  
  for(i in 1:nrow(df_study)){
    # intervention level information (if needed)
    df_int_i <- subset(df_int, StudyID==df_study$StudyID[i])
    
    
    if(!is.na(df_study$`White/Caucasian (%)`[i])){
      df_study$PropWhite[i] <- df_study$`White/Caucasian (%)`[i]/100
    }else if(!is.na(df_study$`White/Caucasian (N)`[i])){
      df_study$PropWhite[i] <- df_study$`White/Caucasian (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`White/Caucasian (%)`))){
      df_study$PropWhite[i] <- mean(as.numeric(df_int_i$`White/Caucasian (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`White/Caucasian (N)`))){
      n <- sum(as.numeric(df_int_i$`White/Caucasian (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropWhite[i] <- n/ntot
    }
    
    if(!is.na(df_study$`Black or African-American (%)`[i])){
      df_study$PropBlack[i] <- df_study$`Black or African-American (%)`[i]/100
    }else if(!is.na(df_study$`Black or African-American (N)`[i])){
      df_study$PropBlack[i] <- df_study$`Black or African-American (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`Black or African-American (%)`))){
      df_study$PropBlack[i] <- mean(as.numeric(df_int_i$`Black or African-American (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`Black or African-American (N)`))){
      n <- sum(as.numeric(df_int_i$`Black or African-American (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropBlack[i] <- n/ntot
    }
    
    if(!is.na(df_study$`Hispanic or Latinx (%)`[i])){
      df_study$PropHisp[i] <- df_study$`Hispanic or Latinx (%)`[i]/100
    }else if(!is.na(df_study$`Hispanic or Latinx (N)`[i])){
      df_study$PropHisp[i] <- df_study$`Hispanic or Latinx (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`Hispanic or Latinx (%)`))){
      df_study$PropHisp[i] <- mean(as.numeric(df_int_i$`Hispanic or Latinx (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`Hispanic or Latinx (N)`))){
      n <- sum(as.numeric(df_int_i$`Hispanic or Latinx (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropHisp[i] <- n/ntot
    }
    
    if(!is.na(df_study$`Asian (%)`[i])){
      df_study$PropAsian[i] <- df_study$`Asian (%)`[i]/100
    }else if(!is.na(df_study$`Asian (N)`[i])){
      df_study$PropAsian[i] <- df_study$`Asian (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`Asian (%)`))){
      df_study$PropAsian[i] <- mean(as.numeric(df_int_i$`Asian (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`Asian (N)`))){
      n <- sum(as.numeric(df_int_i$`Asian (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropAsian[i] <- n/ntot
    }
    
    if(!is.na(df_study$`American Indian or Pacific Islander (%)`[i])){
      df_study$PropAmInd[i] <- df_study$`American Indian or Pacific Islander (%)`[i]/100
    }else if(!is.na(df_study$`American Indian or Pacific Islander (N)`[i])){
      df_study$PropAmInd[i] <- df_study$`American Indian or Pacific Islander (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`American Indian or Pacific Islander (%)`))){
      df_study$PropAmInd[i] <- mean(as.numeric(df_int_i$`American Indian or Pacific Islander (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`American Indian or Pacific Islander (N)`))){
      n <- sum(as.numeric(df_int_i$`American Indian or Pacific Islander (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropAmInd[i] <- n/ntot
    }
    
    if(!is.na(df_study$`Multiracial (%)`[i])){
      df_study$PropMulti[i] <- df_study$`Multiracial (%)`[i]/100
    }else if(!is.na(df_study$`Multiracial (N)`[i])){
      df_study$PropMulti[i] <- df_study$`Multiracial (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`Multiracial (%)`))){
      df_study$PropMulti[i] <- mean(as.numeric(df_int_i$`Multiracial (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`Multiracial (N)`))){
      n <- sum(as.numeric(df_int_i$`Multiracial (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropMulti[i] <- n/ntot
    }
    
    if(!is.na(df_study$`Other (%)`[i])){
      df_study$PropOther[i] <- df_study$`Other (%)`[i]/100
    }else if(!is.na(df_study$`Other (N)`[i])){
      df_study$PropOther[i] <- df_study$`Other (N)`[i]/df_study$NoPtsTotal[i]
    }else if(all(!is.na(df_int_i$`Other (%)`))){
      df_study$PropOther[i] <- mean(as.numeric(df_int_i$`Other (%)`[i]))/100
    }else if(all(!is.na(df_int_i$`Other (N)`))){
      n <- sum(as.numeric(df_int_i$`Other (N)`))
      ntot <- sum(as.numeric(df_int_i$NoPtsRandomised))
      df_study$PropOther[i] <- n/ntot
    }
  }
  df_study
  
}


covid_ind <- function(df_study){
  
  df_study$Year <- NA
  df_study$Covid <- NA
  
  for(i in 1:nrow(df_study)){
    if(!is.na(df_study$StudyEndDate[i])){
      df_study$Year[i] <- as.numeric(format(df_study$StudyEndDate[i], "%Y"))
    }else{
      df_study$Year[i] <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df_study$StudyName[i])) - 1
    }
    
    if(df_study$Year[i]<2020){
      df_study$Covid[i] <- "Pre"
    }else{
      df_study$Covid[i] <- "Post"
    }
    
  }
  df_study
}

############
# checks
############
# settings <- c("Healthcare", "Education", "Community/Other", "Workplace", "Online", "Multiple")
# for(i in 1:nrow(df_study)){
#   if(!(df_study$Setting[i] %in% settings)){
#     print(paste("Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Row:", i, "Setting:", df_study$Setting[i]))
#   }
# }
# multi_set <- df_study[!(is.na(df_study$MultiSetting)),]
# write_xlsx(multi_set, "MultipleSettings.xlsx")
# 
# multi_age <- df_study[!(is.na(df_study$OtherAge)),]
# write_xlsx(multi_age, "OtherAgeGroups.xlsx")
#############


#checked 16/02/24
format_outcome <- function(df_outcome){
  
  #rename no. of pts/clusters per arm (to be different from study level)
  names(df_outcome)[names(df_outcome) == "NoPtsAnalysed"] <- "n"
  names(df_outcome)[names(df_outcome) == "NoClusters"] <- "ArmClusters"
  
  df_outcome$n <- as.numeric(df_outcome$n)
  
  #create column of no. of events
  df_outcome$r <- NA
  #create column for type of event
  df_outcome$event <- NA
  #create column for perc
  df_outcome$perc <- NA
  
  NR.outcome <- 0
  for(i in 1:nrow(df_outcome)){
    n <- df_outcome$n[i] #total no. of participants
    
    #pref 1: series completion
    if(!is.na(df_outcome$VaccUptakeVaccSeriesCompN[i])){
      df_outcome$r[i] <- as.numeric(df_outcome$VaccUptakeVaccSeriesCompN[i])
      df_outcome$event[i] <- "Series"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeVaccSeriesComp%`[i])/100
    }else if(!is.na(df_outcome$`VaccUptakeVaccSeriesComp%`[i])){
      df_outcome$r[i] <- round(n*as.numeric(df_outcome$`VaccUptakeVaccSeriesComp%`[i])/100, digits=0)
      df_outcome$event[i] <- "Series"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeVaccSeriesComp%`[i])/100
    }
    #pref 2: timely completion
    else if(!is.na(df_outcome$VaccUptakeTimelyDoseN[i])){
      df_outcome$r[i] <- as.numeric(df_outcome$VaccUptakeTimelyDoseN[i])
      df_outcome$event[i] <- "Timely"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeTimelyDose%`[i])/100
    }else if(!is.na(df_outcome$`VaccUptakeTimelyDose%`[i])){
      df_outcome$r[i] <- round(n*as.numeric(df_outcome$`VaccUptakeTimelyDose%`)[i]/100, digits=0)
      df_outcome$event[i] <- "Timely"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeTimelyDose%`[i])/100
    }
    #pref 3: second dose
    else if(!is.na(df_outcome$VaccUptakeSecondDoseN[i])){
      df_outcome$r[i] <- as.numeric(df_outcome$VaccUptakeSecondDoseN[i])
      df_outcome$event[i] <- "Second"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeSecondDose%`[i])/100
    }else if(!is.na(df_outcome$`VaccUptakeSecondDose%`[i])){
      df_outcome$r[i] <- round(n*as.numeric(df_outcome$`VaccUptakeSecondDose%`[i])/100, digits=0)
      df_outcome$event[i] <- "Second"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeSecondDose%`[i])/100
    }
    #pref 4: any dose
    else if(!is.na(df_outcome$VaccUptakeAnyDoseN[i])){
      df_outcome$r[i] <- as.numeric(df_outcome$VaccUptakeAnyDoseN[i])
      df_outcome$event[i] <- "Any"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeAnyDose%`[i])/100
    }else if(!is.na(df_outcome$`VaccUptakeAnyDose%`[i])){
      df_outcome$r[i] <- round(n*as.numeric(df_outcome$`VaccUptakeAnyDose%`[i])/100, digits=0)
      df_outcome$event[i] <- "Any"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeAnyDose%`[i])/100
    }
    #pref 5: first dose
    else if(!is.na(df_outcome$VaccUptakeFirstDoseN[i])){
      df_outcome$r[i] <- as.numeric(df_outcome$VaccUptakeFirstDoseN[i])
      df_outcome$event[i] <- "First"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeFirstDose%`[i])/100
    }else if(!is.na(df_outcome$`VaccUptakeFirstDose%`[i])){
      df_outcome$r[i] <- round(n*as.numeric(df_outcome$`VaccUptakeFirstDose%`[i])/100, digits=0)
      df_outcome$event[i] <- "First"
      df_outcome$perc[i] <- as.numeric(df_outcome$`VaccUptakeFirstDose%`[i])/100
    }
    #else
    # else{
    #   print(paste("No outcome data: StudyID:", df_outcome$StudyID[i], "Row:", i))
    #   NR.outcome <- NR.outcome + 1
    # }
    
    if(is.na(df_outcome$n[i])){
      if(is.na(df_outcome$r[i])){
        print(paste("No outcome data r or n: StudyID:", df_outcome$StudyID[i], "check perc = ",
                    df_outcome$perc[i], "Row:", i))
        NR.outcome <- NR.outcome + 1
      }else if(!is.na(df_outcome$perc[i])){
        p <- df_outcome$perc[i]
        r <- df_outcome$r[i]
        df_outcome$n[i] <- r/p
        print(paste("Calculate n from p and r: StudyID:", df_outcome$StudyID[i], "perc = ",
                    df_outcome$perc[i], "r = ", df_outcome$r[i], "n = ", df_outcome$n[i], "Row:", i))
      }
        
    }
  }
  
  print(paste("Number of arms with no outcome data = ", NR.outcome))
  
  df_outcome <- df_outcome[c("StudyID", "IntLabel", "ArmClusters", "n", "r", "event")]
  
}

#checked 16/02/24
format_int <- function(df_int, df_study){
  
  df_int$ControlActive <- NA
  df_int$RefArm <- NA #need to enter manually for active comparison? Or choose randomly (first one)?
  df_int$ControlType <- NA
  
  sum_arms <- 0
  for(i in 1:nrow(df_study)){
    n_arms <- sum(df_int$StudyID == df_study$StudyID[i])
    cont_act <- 0
    for(j in 1:n_arms){
      ind <- sum_arms + j
      if(df_int$StandardCareYN[ind] == TRUE || df_int$NoInterventionYN[ind] == TRUE || 
         df_int$AttentionPlaceboYN[ind] == TRUE){
        cont_act <- cont_act + 1
        df_int$RefArm[ind] <- "Y"
        
        if(!is.na(df_int$IntCategory[ind])){
          print(paste(df_int$StudyID[ind], "Control group has an int category:", df_int$IntCategory[ind]))
        }
        df_int$IntCategory[ind] <- "Control"
        
        #set features = NA
        df_int$DeliveryFormat[ind] <- df_int$PersonalisedTailoredInteractive[ind] <- NA
        df_int$InteractiveType[ind] <- df_int$`DeliveredBy?`[ind] <- df_int$NumberContacts[ind] <- NA
        df_int$AccessVaccClinicsCommSettingsYN[ind] <- df_int$AccessDedicatedClinicsYN[ind] <- NA
        df_int$AccessExtendedHoursClinicYN[ind] <- df_int$AccessOutreachMobileClinicsYN[ind] <- NA
        df_int$AccessOppurtunisticVaccsYN[ind] <- df_int$AccessAppointmentSchedulingYN[ind] <- NA
        df_int$AccessAcceleratedScheduleYN[ind] <- NA
        df_int$AffordabilityFinancialIncenYN[ind] <- df_int$AffordabilityPaymentCoverCostsYN[ind] <- NA
        df_int$AffordabilityTimeCostsYN[ind] <- df_int$AwarenessKnowledgeVaccsYN[ind] <- NA
        df_int$AcceptanceVaccSafetyEfficacyAttitudeYN[ind] <- df_int$AcceptanceDiseasePerceivedSeverityRiskYN[ind] <- NA
        df_int$AcceptanceIndFactorsYN[ind] <- df_int$AcceptanceSocialInfluenceYN[ind] <- NA
        df_int$AcceptanceDesicionAidsHelpYN[ind] <- df_int$AcceptanceAltProvisionYN[ind] <- NA
        df_int$ActivationPromptsRemindersYN[ind] <- df_int$ActivationMandatoryPolYN[ind] <- NA
        
        if(df_int$StandardCareYN[ind] == TRUE){
          df_int$ControlType[ind] <- "Standard care"
        }else if(df_int$NoInterventionYN[ind] == TRUE){
          df_int$ControlType[ind] <- "No intervention"
        }else if(df_int$AttentionPlaceboYN[ind] == TRUE){
          df_int$ControlType[ind] <- "Attention placebo"
        }
        
      }
    }
    
    if(cont_act > 0){
      for(j in 1:n_arms){
        ind <- sum_arms + j
        df_int$ControlActive[ind] <- "Control"
        if(is.na(df_int$RefArm[ind])){
          df_int$RefArm[ind] <- "N"
        }
      }
      if(cont_act>1){
        print(paste(">1 control arm. Study:", df_study$StudyName[i], "ID:", df_study$StudyID[i], "Int Row:", ind))
      }
      
    }else{
      for(j in 1:n_arms){
        ind <- sum_arms + j
        df_int$ControlActive[ind] <- "Active"
        if(j==1){
          df_int$RefArm[ind] <- "Y"
        }else{
          df_int$RefArm[ind] <- "N"
        }
        
      }
    }
    sum_arms <- sum_arms + n_arms
  }
  
  #check int category
  for(i in 1:nrow(df_int)){
    
    if(!is.na(df_int$IntCategory[i])){
      #categorize multiple
      if(grepl(",", df_int$IntCategory[i])){
        out_split <- strsplit(df_int$IntCategory[i], split = ", ")
      }else if(grepl(";", df_int$IntCategory[i])){
        out_split <- strsplit(df_int$IntCategory[i], split = ";")
      }else{
        out_split <- strsplit(df_int$IntCategory[i], split = ", ") #so code continues
      }
      vec_split <- out_split[[1]]
      
      #now remove duplicates
      vec_split <- unique(vec_split)
      #now order alphabetically
      vec_split <- sort(vec_split)
      
      df_int$IntCategory[i] <- paste(vec_split, collapse = " and ")
      
    }else{
      print(paste(df_int$StudyID[i], "Int Category missing"))
    }
  }
  
  
  
  df_int <- df_int[c("StudyID", "IntLabel", "ControlActive", "RefArm", "ControlType", 
                     "IntCategory", "DeliveryFormat", "PersonalisedTailoredInteractive",
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
                     "AccessAppointmentScheduling", "AcceptanceDecisionAidsHelp")]
  df_int
}



merge_all <- function(df_int, df_outcome, df_study){
  
  df_formatted <- merge(df_outcome, df_int, by = c("StudyID", "IntLabel"))
  df_formatted <- merge(df_study, df_formatted, by = c("StudyID"))
  
  df_formatted <- df_formatted[c("StudyID", "StudyName", "StudyDesign", "UnitRand", "ActualICC", 
                     "AssumedICC", "ControlActive", "NoArms", "NoArmsAnalysed", "NoPtsTotal", "NoClusters", 
                     "IntLabel", "IntCategory", "RefArm", "ControlType", "ArmClusters", "event", "n", "r",
                     "Covid", "VaccineType","AgeGroup", "OtherAge", "Setting", "MultiSetting","Environment", "PropMale", 
                     "SpecificPopUnderserved", "SpecificPopUnderservedCategory",
                     "SpecificPopUnderservedOther", "PropWhite", "PropBlack", "PropHisp", 
                     "PropAsian", "PropAmInd", "PropMulti", "PropOther", "Ethnicity - Other",
                     "DeliveryFormat", "PersonalisedTailoredInteractive",
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
                     "ActivationPromptsRemindersYN", "ActivationMandatoryPolYN"
  )]
  df_formatted
}

format_rob <- function(df_rob){
  df_rob$HighROB <- NA
  for(i in 1:nrow(df_rob)){
    if(df_rob$ROB[i]=="High"){
      df_rob$HighROB[i] <- 1
    }else if(df_rob$ROB[i]=="Some concerns" || df_rob$ROB[i]=="Low"){
      df_rob$HighROB[i] <- 0
    }else{
      print(paste("Invalid ROB", df_rob$StudyID[i], df_rob$ROB[i]))
    }
  }
  df_rob
}


add_target_age <- function(data, df_study_orig){
  
  adolescents <- c("Adolescents", "Adolescent")
  adults <- c("Adults","Adult","Parent of adolescents", "Parent of adolescent")
  
  data$TargetAge <- NA
  for(i in 1:nrow(df_study_orig)){
    data_i <- subset(data, StudyID==df_study_orig$StudyID[i])
    rws <- which(data$StudyID==df_study_orig$StudyID[i])
    
    if(data_i$AgeGroup[1]=="Adolescents"){
      if(grepl(",", df_study_orig$IntRecipientIntervention[i])){
        out_split <- strsplit(df_study_orig$IntRecipientIntervention[i], split = ", ")
      }else{
        out_split <- strsplit(df_study_orig$IntRecipientIntervention[i], split = ";")
      }
      target_split <- out_split[[1]]
      if(all(target_split %in% adolescents)){
        data$TargetAge[rws] <- "Adolescents"
      }else if(all(target_split %in% adults)){
        data$TargetAge[rws] <- "Parents"
      }else{
        print(paste0("Check both: ", df_study_orig$StudyID[i], " ", df_study_orig$IntRecipientIntervention[i]))
        data$TargetAge[rws] <- "Both"
      }
    }
  }
  data
}

    
   