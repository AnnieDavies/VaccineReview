code_study_inspect <- function(df_study){
  
  #Age group
  df_study$YoungChildren <- 0
  df_study$Children <- 0
  df_study$Adolescents <- 0
  df_study$Adults <- 0
  df_study$PregWomen <- 0
  df_study$OlderAdults <- 0
  
  for(i in 1:nrow(df_study)){
    if(!is.na(df_study$AgeGroup[i])){
      if(df_study$AgeGroup[i]=="Young children"){
        df_study$YoungChildren[i] <- 1
      }else if(df_study$AgeGroup[i]=="Children"){ 
        df_study$Children[i] <- 1
      }else if(df_study$AgeGroup[i]=="Adolescents"){
        df_study$Adolescents[i] <- 1
      }else if(df_study$AgeGroup[i]=="Adults"){
        df_study$Adults[i] <- 1
      }else if(df_study$AgeGroup[i]=="Pregnant women"){
        df_study$PregWomen[i] <- 1
      }else if(df_study$AgeGroup[i]=="Older adults"){
        df_study$OlderAdults[i] <- 1
      }else{
        print(paste("Invalid Age Group:", df_study$AgeGroup[i], "StudyID:", df_study$StudyID[i], df_study$StudyName[i]))
      }
    }else{
      print(paste("NA Age Group. StudyID:", df_study$StudyID[i], df_study$StudyName[i]))
    }
  }
  
  #Setting
  df_study$Healthcare <- 0
  df_study$Education <- 0
  df_study$CommunityOther <- 0
  df_study$Online <- 0
  for(i in 1:nrow(df_study)){
    if(!is.na(df_study$Setting[i])){
      if(df_study$Setting[i]=="Healthcare"){
        df_study$Healthcare[i] <- 1
      }else if(df_study$Setting[i]=="Education"){ 
        df_study$Education[i] <- 1
      }else if(df_study$Setting[i]=="Community/Other"){
        df_study$CommunityOther[i] <- 1
      }else if(df_study$Setting[i]=="Online"){
        df_study$Online[i] <- 1
      }else{
        print(paste("Invalid Setting:", df_study$Setting[i], "StudyID:", df_study$StudyID[i], df_study$StudyName[i]))
      }
    }else{
      print(paste("NA Setting. StudyID:", df_study$StudyID[i], df_study$StudyName[i]))
    }
  }
  
  #Environment Setting
  df_study$Urban <- 0
  df_study$Rural <- 0
  df_study$MixedEnvironment <- 0
  for(i in 1:nrow(df_study)){
    if(df_study$Environment[i]=="Urban"){
      df_study$Urban[i] <- 1
    }else if(df_study$Environment[i]=="Rural"){ 
      df_study$Rural[i] <- 1
    }else if(df_study$Environment[i]=="Mixed"){
      df_study$MixedEnvironment[i] <- 1
    }else{
      print(paste("Invalid environment:", df_study$Environment[i], "StudyID:", df_study$StudyID[i], df_study$StudyName[i]))
    }
  }
  
  #underserved pop
  df_study$SpecificPopUnderserved <- df_study$SpecificPopUnderserved*1
  
  
  #Covid (pre = 0, post = 1)
  df_study$PostCovid <- ifelse(df_study$Covid=="Post",1,0)
  
  #Select relevant columns only
  df_study <- df_study[c("StudyID", "StudyName", "StudyDesign", "NoPtsTotal", 
                         "NoArms", "NoArmsAnalysed", "NoClusters", "UnitRand", "ActualICC", "AssumedICC", 
                         "PostCovid", "VaccineType", "AgeGroup", "YoungChildren", "Children", "Adolescents", 
                         "Adults", "OlderAdults", "PregWomen",  
                         "Setting", "Healthcare", "Education", "CommunityOther",
                         "Online", "Environment", "Urban", "Rural", "MixedEnvironment",
                         "PropMale", "SpecificPopUnderserved", 
                         "SpecificPopUnderservedCategory", "SpecificPopUnderservedOther",
                         "PropWhite", "PropBlack", "PropHisp", "PropAsian", "PropAmInd", 
                         "PropMulti", "PropOther", "Ethnicity - Other", "Country")]
  df_study
  
}

plot_study_info_corr <- function(df_study01, txtsize = 20){
  dat <- df_study01[, c("YoungChildren", "Children", "Adolescents", "Adults", 
                        "OlderAdults", "PregWomen", 
                        "Healthcare", "Education", "CommunityOther", "Online", 
                         "Urban", "Rural", "MixedEnvironment")]
  
  colnames(dat) <- c("Young children", "Children", "Adolescents/Young adults", "Adults", 
                     "Older Adults", "Pregnant Women",   
                     "Healthcare", "Education", "Community/Other", "Online", 
                      "Urban", "Rural", "Mixed environment")
  
  
  counts <- data.frame(matrix(nrow = ncol(dat), ncol = 2))
  counts[1] <- colnames(dat)
  for(i in 1:ncol(dat)){
    counts[i,2] <- sum(dat[i], na.rm=TRUE)/nrow(dat)
  }
  cols <- c("Study-level covariate", "Proportion of studies coded as 1")
  colnames(counts) <- cols
  
  counts$`Study-level covariate` <- factor(counts$`Study-level covariate`, levels = unique(counts$`Study-level covariate`), ordered = TRUE)
  
  subset1 <- c(0, 6.5)    # Adjust the range for the first subset
  subset2 <- c(6.5, 10.5)   # Adjust the range for the second subset
  subset3 <- c(10.5, 13.5) 
  p <- ggplot(counts, aes(x=`Study-level covariate`, y=`Proportion of studies coded as 1`, 
                     fill=`Study-level covariate`)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
              legend.position = "none", text = element_text(size=txtsize)) +
    geom_hline(yintercept = 0.2) +
    geom_segment(aes(x = subset1[1], xend = subset1[2], y = 1/6, yend = 1/6),
                 linetype = "dashed", color = "black", linewidth = 1)+
    geom_segment(aes(x = subset2[1], xend = subset2[2], y = 1/4, yend = 1/4),
                 linetype = "dashed", color = "black", linewidth = 1) +
    geom_segment(aes(x = subset3[1], xend = subset3[2], y = 1/3, yend = 1/3),
                 linetype = "dashed", color = "black", linewidth = 1)
  #print(p)
  
  
  
  ## Correlation plot
  #pearson correlation= binomial correlation in 2x2 case
  dat_cor <- cor(dat,use="everything",method="pearson") 
  
  #c <- corrplot.mixed(dat_cor, lower = "number", tl.pos = "lt", tl.col = "black",
  #               number.cex=1, tl.cex=1.1, cl.cex=1.1)
  #corrplot(dat_cor, tl.col = "black",method="ellipse", tl.cex=1.1, cl.cex = 1.2)
  
  list(counts, dat_cor, p)
} 

code_interventions <- function(df_int){
  
  ## Delivery Format--------------------------
  codeDF <- code_delivery_format(df_int)
  df_int <- codeDF[[1]]
  df_issues <- codeDF[[2]]
  
  ## Interaction-------------------------------
  codeINT <- code_DF_interaction(df_int)
  df_int <- codeINT[[1]]
  PTI_issues <- codeINT[[2]]
  
  ## Delivered by-----------------------------------
  codeDB <- code_delivered_by(df_int)
  df_int <- codeDB[[1]]
  db_issues <- codeDB[[2]]
  
  ## Intensity-------------------------------------
  df_int <- code_intensity(df_int)
  
  ## Binary -----------------------
  df_int <- code_binary_covs(df_int)
  
  ##check binary
  ind0 <- which(colnames(df_int) == "Access_ExtendedOpportunities")#first cov
  ind1 <- which(colnames(df_int) == "Activation")#last cov
  bin_covs <- colnames(df_int)[ind0:ind1] #define a vector of binary covariates
  
  bin_issues <- data.frame("StudyID" = character(), "IntLabel" = character(), 
                           "Covariate" = character(), "Issue" = character(), 
                           "Covariate value" = character(), "Row" = numeric())
  df_bin <- df_int[,c(ind0:ind1)]

  for(i in 1:nrow(df_bin)){
    for(j in 1:ncol(df_bin)){
      #[row,col] = [i,j]
      if(!is.na(df_bin[[i,j]])){
        if(df_bin[[i,j]] != 1 && df_bin[[i,j]] != 0){
          print(paste("Invalid", bin_covs[j], ": ", df_int[[i,j]], "Study:ID", 
                      df_int$StudyID[i], "Row:", i))
          row <- c(df_int$StudyID[i], df_int$IntLabel[i],  bin_covs[j],
                   "Invalid", df_int[[i,j]], i)
          bin_issues <- rbind(bin_issues, row)
        }
      }else if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
        print(paste("Missing", bin_covs[j], ": ", df_int[[i,j]], "Study:ID", 
                    df_int$StudyID[i], df_int$IntLabel[i], "Row:", i))
        row <- c(df_int$StudyID[i], df_int$IntLabel[i],  bin_covs[j],
                 "Missing", df_int[[i,j]], i)
        bin_issues <- rbind(bin_issues, row)
      }
    }
  }
  colnames(bin_issues) <- c("StudyID", "IntLabel","Covariate", "Issue", "Covariate value", "Row")
  
  ls <- list(df_int, df_issues, PTI_issues, db_issues, bin_issues)
  ls
}


code_delivery_format <- function(df_int){
  
  #delivery format **must be split by colons
  DF_personal <- c("Letters",  "Postcards", "Text messages", "Email", "Phone calls", "Face-to-face",
                   "Letter",  "Postcard", "Text message", "Emails", "Phone call", "Face-to-Face")
  DF_other <- c("Online (social media/apps/pop-ups)", "Printed Materials (E.g. Leaflets)", "Posters",
                "Online information exchange", "Multi-media campaigns","Electronic invitations (via apps)",
                "TV advertising", "Radio campaigns", "Electronic device (Tablets/iPads)", "Videos", "Other - specify",
                "Online including social media", "Electronic device (Tablets)")
  
  df_int$DF_PersonalDelivery <- NA
  
  df_issues <- data.frame("StudyID" = character(), "IntLabel" = character(), 
                          "Issue" = character(), "Detail" = character(), "Row" = numeric())
  
  for(i in 1:nrow(df_int)){
    if(grepl(",", df_int$DeliveryFormat[i])){
      out_split <- strsplit(df_int$DeliveryFormat[i], split = ", ")
    }else{
      out_split <- strsplit(df_int$DeliveryFormat[i], split = ";")
    }
    
    if(all(!is.na(out_split[[1]]))){
      if(any(out_split[[1]] %in% DF_personal)){
        df_int$DF_PersonalDelivery[i] <- 1
      }else{
        df_int$DF_PersonalDelivery[i] <- 0
        if(any(!(out_split[[1]] %in% DF_other))){
          print(paste(df_int$StudyID[i], "Invalid delivery format:", df_int$DeliveryFormat[i], "Row:", i))
          row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Invalid", df_int$DeliveryFormat[i], i)
          df_issues <- rbind(df_issues, row)
        }
        
      }
    }else{
      if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
        print("Missing delivery format", paste(df_int$StudyID[i], df_int$IntLabel[i], "NA","Row:", i))
        row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Missing", df_int$DeliveryFormat[i], i)
        df_issues <- rbind(df_issues, row)
      }
    }
  }
  list(df_int, df_issues)
}


code_DF_interaction <- function(df_int){
  df_int$DF_interaction <- NA
  df_int$DF_human <- NA
  
  PTI_issues <- data.frame("StudyID" = character(), "IntLabel" = character(), 
                           "Issue" = character(), "PersonalisedTailoredInteractive" = character(), 
                           "InteractiveType" = character(), "Row" = numeric())
  no_int <- c("No interactive elements", "Personalised/Tailored")
  
  for(i in 1:nrow(df_int)){
    if(!is.na(df_int$IntCategory[i]) && df_int$IntCategory[i] !="Control"){
      if(!is.na(df_int$PersonalisedTailoredInteractive[i])){
        #split
        if(grepl(";", df_int$PersonalisedTailoredInteractive[i])){
          out_split <- strsplit(df_int$PersonalisedTailoredInteractive[i], split = ";")
        }else{
          out_split <- strsplit(df_int$PersonalisedTailoredInteractive[i], split = ", ")
        }
        vec_split <- out_split[[1]]
        
        if(all(vec_split %in% no_int)){
          df_int$DF_interaction[i] <- 0
          df_int$DF_human[i] <- 0
          
          if(!is.na(df_int$InteractiveType[i])){
            print(paste("Interaction type specified when 'non-interactive':", df_int$StudyID[i], df_int$IntLabel[i], df_int$PersonalisedTailoredInteractive[i],
                        df_int$InteractiveType[i], "Row:", i))
            row <- c(df_int$StudyID[i], df_int$IntLabel[i], "IT specified for non-interactive", 
                     df_int$PersonalisedTailoredInteractive[i],
                     df_int$InteractiveType[i], i)
            PTI_issues <- rbind(PTI_issues, row)
          }
          
          
        }else if(any(vec_split == "No interactive elements")){
          print(paste("Interactions specified in PTI with 'non-interactive':", df_int$StudyID[i], df_int$IntLabel[i], df_int$PersonalisedTailoredInteractive[i],
                      df_int$InteractiveType[i], "Row:", i))
          row <- c(df_int$StudyID[i], df_int$IntLabel[i], "PTI interactions with non-interactive", 
                   df_int$PersonalisedTailoredInteractive[i],
                   df_int$InteractiveType[i], i)
          PTI_issues <- rbind(PTI_issues, row)
        }else{
          df_int$DF_interaction[i] <- 1
          
          if(!is.na(df_int$InteractiveType[i])){
            if(df_int$InteractiveType[i]=="Human Dialogue" || df_int$InteractiveType[i]== "Combination (automated & human)"){
              df_int$DF_human[i] <- 1
            }else{
              df_int$DF_human[i] <- 0
              
              if(df_int$InteractiveType[i]!="Automated"){
                print(paste("Invalid IT", df_int$StudyID[i], df_int$IntLabel[i], df_int$PersonalisedTailoredInteractive[i],
                            df_int$InteractiveType[i], "Row:", i))
                row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Invalid IT", 
                         df_int$PersonalisedTailoredInteractive[i],
                         df_int$InteractiveType[i], i)
                PTI_issues <- rbind(PTI_issues, row)
              }
              
            }
            
          }else{  
            print(paste("No interactive (from missing IT): ", df_int$StudyID[i], df_int$IntLabel[i], 
                        df_int$PersonalisedTailoredInteractive[i],
                        df_int$InteractiveType[i], "Row:", i))
            row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Check no interactive - missing IT", 
                     df_int$PersonalisedTailoredInteractive[i],
                     df_int$InteractiveType[i], i)
            PTI_issues <- rbind(PTI_issues, row)
          }
        }
      }else{  
        print(paste("Missing PTI: ", df_int$StudyID[i], df_int$IntLabel[i], 
                    df_int$PersonalisedTailoredInteractive[i],
                    df_int$InteractiveType[i], "Row:", i))
        row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Missing PTI", 
                 df_int$PersonalisedTailoredInteractive[i],
                 df_int$InteractiveType[i], i)
        PTI_issues <- rbind(PTI_issues, row)
      }
    }#end if not equal to control
    
    if(!is.na(df_int$DF_interaction[i]) && !is.na(df_int$DF_human[i])){
      if(df_int$DF_interaction[i] == 0 &&  df_int$DF_human[i]==1){
        print(paste("No interaction and human dialogue: ", df_int$StudyID[i], df_int$IntLabel[i], 
                    df_int$PersonalisedTailoredInteractive[i],
                    df_int$InteractiveType[i], "Row:", i))
      }
    }
  }
  colnames(PTI_issues) <- c("StudyID", "IntLabel", "Issue", 
                            "PersonalisedTailoredInteractive", "InteractiveType", "Row")
  list(df_int, PTI_issues)
}


code_delivered_by <- function(df_int){
  community <- c("Community leaders",  "Peers",  "Religious leaders", "Social workers", 
                 "Teachers", "Other - specify", "Social Workers")
  healthcare <- c("GP", "Health visitors", "Midwives","Nurse", "Pharmacist", 
                  "Practitioners","Researchers")
  df_int$DB_healthcare <- NA
  df_int$DB_community <- NA
  
  
  db_issues <- data.frame("StudyID" = character(), "IntLabel" = character(), 
                          "Issue" = character(), "DeliveredBy" = character(), "Row" = numeric())
  for(i in 1:nrow(df_int)){
    
    if(grepl(",", df_int$`DeliveredBy?`[i])){
      out_split <- strsplit(df_int$`DeliveredBy?`[i], split = ", ")
    }else{
      out_split <- strsplit(df_int$`DeliveredBy?`[i], split = ";")
    }
    
    
    if(any(!(out_split[[1]] %in% community) & !(out_split[[1]] %in% healthcare))){
      
      if(any(is.na(out_split))){
        if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
          print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Delivered by - Missing:",df_int$`DeliveredBy?`[i], "Row:", i))
          row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Missing", df_int$`DeliveredBy?`[i], i)
          db_issues <- rbind(db_issues, row)
        }
      }else{
        print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Delivered by - Invalid: ", df_int$`DeliveredBy?`[i], "Row:", i))
        row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Invalid", df_int$`DeliveredBy?`[i], i)
        db_issues <- rbind(db_issues, row)
      }
    }else if(all(out_split[[1]] %in% healthcare)){
      df_int$DB_healthcare[i] <- 1
      df_int$DB_community[i] <- 0
    }else if(all(out_split[[1]] %in% community)){
      df_int$DB_healthcare[i] <- 0
      df_int$DB_community[i] <- 1
    }else{
      print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Delivered by - healthcare and community:", df_int$`DeliveredBy?`[i], "Row:", i))
      row <- c(df_int$StudyID[i], df_int$IntLabel[i], "Healthcare and community", df_int$`DeliveredBy?`[i], i)
      db_issues <- rbind(db_issues, row)
      df_int$DB_healthcare[i] <- 1
      df_int$DB_community[i] <- 1
    }
  }
  colnames(db_issues) <- c("StudyID", "IntLabel", "Issue", "DeliveredBy", "Row")
  list(df_int, db_issues)
}


code_intensity <- function(df_int){
  df_int$Intensity <- NA
  df_int$HighIntensity <- NA
  df_int$MediumIntensity <-NA
  for(i in 1:nrow(df_int)){
    if(!is.na(df_int$NumberContacts[i])){
      if(df_int$NumberContacts[i]==1){
        df_int$Intensity[i] <- "Low"
        df_int$HighIntensity[i] <- 0
        df_int$MediumIntensity[i] <- 0
      }else if(df_int$NumberContacts[i]<=4){
        df_int$Intensity[i] <- "Medium"
        df_int$HighIntensity[i] <- 0
        df_int$MediumIntensity[i] <- 1
      }else if(df_int$NumberContacts[i]>=5){
        df_int$Intensity[i] <- "High"
        df_int$HighIntensity[i] <- 1
        df_int$MediumIntensity[i] <- 0
      }else{
        print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Invalid number of contacts:",df_int$NumberContacts[i]))
      }
    }else if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
      print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Missing number of contacts:",df_int$NumberContacts[i]))
    }
  }
  df_int
}


code_binary_covs <- function(df_int){
  df_int$Access_ExtendedOpportunities <- NA
  df_int$Access_AppointmentSchedulingHelp <- NA 
  df_int$Access_AppointmentSchedulingOnline <- NA ##NB: can't be both online and help
  df_int$Access_AcceleratedDosing <- df_int$AccessAcceleratedScheduleYN*1
  
  df_int$Affordability_Incentives <- df_int$AffordabilityFinancialIncenYN*1
  df_int$Affordability_CostsCovered <- df_int$AffordabilityPaymentCoverCostsYN*1
  
  df_int$Awareness <- df_int$AwarenessKnowledgeVaccsYN*1 #**may change
  
  df_int$Acceptance_VaccineSafety <- df_int$AcceptanceVaccSafetyEfficacyAttitudeYN*1
  df_int$Acceptance_DiseasePerceivedRisk <- df_int$AcceptanceDiseasePerceivedSeverityRiskYN*1
  df_int$Acceptance_SocialFactors <- df_int$AcceptanceSocialInfluenceYN*1
  df_int$Acceptance_DecisionAids <- NA
  df_int$Acceptance_MotivationalInterviewing <- NA
  
  df_int$Activation <- NA
  
  for(i in 1:nrow(df_int)){
    #Access----------------------------
    if(!is.na(df_int$AccessVaccClinicsCommSettingsYN[i]) &&
       !is.na(df_int$AccessDedicatedClinicsYN[i]) &&
       !is.na(df_int$AccessExtendedHoursClinicYN[i]) &&
       !is.na(df_int$AccessOutreachMobileClinicsYN[i]) &&
       !is.na(df_int$AccessOppurtunisticVaccsYN[i]) &&
       !is.na(df_int$AccessAppointmentSchedulingYN[i]) &&
       !is.na(df_int$AccessAcceleratedScheduleYN[i])){
      
      #Access extended opportunities
      if(df_int$AccessVaccClinicsCommSettingsYN[i]=="TRUE" || 
         df_int$AccessDedicatedClinicsYN[i]=="TRUE" ||
         df_int$AccessExtendedHoursClinicYN[i]=="TRUE" ||
         df_int$AccessOutreachMobileClinicsYN[i]=="TRUE" ||
         df_int$AccessOppurtunisticVaccsYN[i]=="TRUE"){
        df_int$Access_ExtendedOpportunities[i] <- 1
      }else{
        df_int$Access_ExtendedOpportunities[i] <- 0
      }
      
      #Access appointment scheduling
      if(df_int$AccessAppointmentSchedulingYN[i]=="TRUE"){
          if(startsWith(df_int$AccessAppointmentScheduling[i], "ONLINE")){
            df_int$Access_AppointmentSchedulingHelp[i] <- 0
            df_int$Access_AppointmentSchedulingOnline[i] <- 1
          }else if(startsWith(df_int$AccessAppointmentScheduling[i], "HELP") ||
                   startsWith(df_int$AccessAppointmentScheduling[i], "SCHEDULED")){
            df_int$Access_AppointmentSchedulingHelp[i] <- 1
            df_int$Access_AppointmentSchedulingOnline[i] <- 0
          }else{
            print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Invalid appointment scheduling",
                        df_int$AccessAppointmentSchedulingYN[i], df_int$AccessAppointmentScheduling[i]))
          }
      }else{
        df_int$Access_AppointmentSchedulingHelp[i] <- 0
        df_int$Access_AppointmentSchedulingOnline[i] <- 0
      }
    }else if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
      print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Missing Access coding"))
    }
    
    ##Decision Aids------------------
    if(!is.na(df_int$AcceptanceDesicionAidsHelpYN[i])){
      if(df_int$AcceptanceDesicionAidsHelpYN[i]=="TRUE"){
        
        if(startsWith(df_int$AcceptanceDecisionAidsHelp[i], "DECISION AID")){
          df_int$Acceptance_DecisionAids[i] <- 1
          df_int$Acceptance_MotivationalInterviewing[i] <- 0
        }else if(startsWith(df_int$AcceptanceDecisionAidsHelp[i], "MOTIVATIONAL INTERVIEWING")){
          df_int$Acceptance_DecisionAids[i] <- 0
          df_int$Acceptance_MotivationalInterviewing[i] <- 1
        }else{
          print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Invalid Decision aids", 
                      df_int$AcceptanceDesicionAidsHelpYN[i], df_int$AcceptanceDecisionAidsHelp[i]))
        }
      }else{
        df_int$Acceptance_DecisionAids[i] <- 0
        df_int$Acceptance_MotivationalInterviewing[i] <- 0
      }
    }else if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
      print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Missing Decision Aid coding"))
    }
    
    ##Activation-----------------------
    if(!is.na(df_int$ActivationPromptsRemindersYN[i]) &&
       !is.na(df_int$ActivationMandatoryPolYN[i])){
      if(df_int$ActivationPromptsRemindersYN[i]=="TRUE"||
         df_int$ActivationMandatoryPolYN[i]=="TRUE"){
        df_int$Activation[i]<-1
      }else{
        df_int$Activation[i]<-0
      }
    }else if(is.na(df_int$IntCategory[i]) || df_int$IntCategory[i] !="Control"){
      print(paste(df_int$StudyID[i], df_int$IntLabel[i], "Missing Activation coding"))
    }
    
  }
  
  df_int 
  
}




plot_int_info_corr <- function(df_int01, txtsize = 20){
  df_int01 <- subset(df_int01, IntCategory != "Control")
  
  ##Categorical covariates
  dat <- df_int01[, c("DF_PersonalDelivery", "DF_interaction", "DF_human",
                      "DB_healthcare", "DB_community", "MediumIntensity", "HighIntensity",
                      "Access_ExtendedOpportunities", "Access_AppointmentSchedulingHelp",
                      "Access_AppointmentSchedulingOnline", "Access_AcceleratedDosing", 
                      "Affordability_Incentives", "Affordability_CostsCovered", 
                      "Awareness",
                      "Acceptance_VaccineSafety", "Acceptance_DiseasePerceivedRisk",
                      "Acceptance_SocialFactors","Acceptance_DecisionAids", "Acceptance_MotivationalInterviewing",
                      "Activation")]
  
  colnames(dat) <- c("DF_Personal", "DF_interaction", "DF_human",
                     "DB_healthcare", "DB_community", "MediumIntensity", "HighIntensity",
                     "Access_ExtendedOpp", "Access_ApptScheduleHelp",
                     "Access_ApptScheduleOnline", "Access_AcceleratedDosing", 
                     "Affordability_Incentives", "Affordability_Costs", 
                     "Awareness",
                     "Acceptance_VaccineSafety", "Acceptance_DiseaseRisk",
                     "Acceptance_Social","Acceptance_DecisionAids", "Acceptance_MotInterviewing",
                     "Activation")
  
  
  counts <- data.frame(matrix(nrow = ncol(dat), ncol = 2))
  counts[1] <- colnames(dat)
  for(i in 1:ncol(dat)){
    counts[i,2] <- sum(dat[i], na.rm=TRUE)/nrow(dat)
  }
  cols <- c("Intervention covariate", "Proportion of arms coded as 1")
  colnames(counts) <- cols
  
  counts$`Intervention covariate` <- factor(counts$`Intervention covariate`, 
                                            levels = unique(counts$`Intervention covariate`), 
                                            ordered = TRUE)
  
  p <- ggplot(counts, aes(x=`Intervention covariate`, y=`Proportion of arms coded as 1`, 
                          fill=`Intervention covariate`)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          legend.position = "none", text = element_text(size=txtsize)) +
    geom_hline(yintercept = 0.2) +
    geom_hline(yintercept = 0.8)
  
  ##Correlations
  
  dat_cor <- cor(dat,use="pairwise.complete.obs",method="pearson") 
  
  list(counts, dat_cor, p)
}



merge01 <- function(df_int01, df_outcome, df_study01){
  
  df <- merge(df_outcome, df_int01, by = c("StudyID", "IntLabel"))
  df <- merge(df_study01, df, by = c("StudyID"))
  
  df <- df[c("StudyID", "StudyName", "StudyDesign", "UnitRand", "ActualICC", 
             "AssumedICC", "ControlActive", "NoArms", "NoArmsAnalysed", "NoPtsTotal", "NoClusters", 
             "IntLabel", "IntCategory", "RefArm", "ControlType", "ArmClusters", "event", "n", "r",
             "AgeGroup", "VaccineType", "PostCovid", "YoungChildren", "Children", "Adolescents", 
             "Adults", "OlderAdults", "PregWomen",  
             "Setting", "Healthcare", "Education", "CommunityOther",
             "Online", "Environment", "Urban", "Rural", "MixedEnvironment",
             "PropMale", "SpecificPopUnderserved", 
             "SpecificPopUnderservedCategory", "SpecificPopUnderservedOther",
             "PropWhite", "PropBlack", "PropHisp", "PropAsian", "PropAmInd", 
             "PropMulti", "PropOther", "Ethnicity - Other", "Country",
             "DF_PersonalDelivery", "DF_interaction", "DF_human",
             "DB_healthcare", "DB_community", "NumberContacts", "Intensity", "MediumIntensity", "HighIntensity",
             "Access_ExtendedOpportunities", "Access_AppointmentSchedulingHelp",
             "Access_AppointmentSchedulingOnline", "Access_AcceleratedDosing", 
             "Affordability_Incentives", "Affordability_CostsCovered", 
             "Awareness",
             "Acceptance_VaccineSafety", "Acceptance_DiseasePerceivedRisk",
             "Acceptance_SocialFactors","Acceptance_DecisionAids", "Acceptance_MotivationalInterviewing",
             "Activation"
  )]
  df
}





plot_countries <- function(df_study.full){
  
  df_study.full$Country <- ifelse(df_study.full$Country=="United States;United States", 
                                  "United States", df_study.full$Country)
  
  countries <- unique(df_study.full$Country)
  
  counts <- data.frame(matrix(nrow = length(countries), ncol = 2))
  counts[1] <- countries
  for(i in 1:length(countries)){
    if(!is.na(countries[i])){
      counts[i,2] <- sum(df_study.full$Country==countries[i], na.rm=TRUE)
    }else{
      counts[i,2] <- sum(is.na(df_study.full$Country))
    }
    
  }
  cols <- c("Country", "Counts")
  colnames(counts) <- cols
  
  counts$`Country` <- factor(counts$`Country`, levels = unique(counts$`Country`), ordered = TRUE)
  
  p <- ggplot(counts, aes(x=`Country`, y=`Counts`, 
                          fill=`Country`)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          legend.position = "none", text = element_text(size=18)) 
  list(p, counts)
}


