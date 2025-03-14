rm_missing_outcome <- function(df){
  
  df_new <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(df_new) <- colnames(df)
  
  studies <- unique(df$StudyID)
  
  count <- 0
  for(i in 1:length(studies)){
    dfi <- subset(df, StudyID==studies[i])
    
    if(any(is.na(dfi$r) | is.na(dfi$n))){
      print(paste("Study removed - missing data:", dfi$StudyID[1], dfi$StudyName[1],
                  dfi$IntCategory[1]))
      count <- count + 1
    }else{
      df_new <- rbind(df_new, dfi)  
    }
  }
  print(paste(count, "studies dropped for missing outcome data"))
  df_new
}

rm_missing_int <- function(df){
  df_new <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(df_new) <- colnames(df)
  
  studies <- unique(df$StudyID)
  
  count <- 0
  for(i in 1:length(studies)){
    dfi <- subset(df, StudyID==studies[i])
    
    if(any(is.na(dfi$IntCategory))){
      print(paste("Study removed - missing int category:", dfi$StudyID[1], dfi$StudyName[1],
                  dfi$IntCategory[1]))
      count <- count + 1
    }else{
      df_new <- rbind(df_new, dfi)  
    }
  }
  print(paste(count, "studies dropped for missing intervention data"))
  
  df_new
}


adj_cluster <- function(data, icc.hh = 0.7, icc.other = 0.05, adjust = TRUE){
  
  if(adjust==FALSE){
    data$r_adj <- data$r
    data$n_adj <- data$n
    
    #so columns align
    data$UsedICC <- NA
    data$MeanClusterSize <- NA
    data$DesignEffect <- NA
    
  }else{
    data$r_adj <- NA
    data$n_adj <- NA
    
    class20 <- c("Italy", "Finland", "Germany", "Sweden", "United States", "Spain",
                 "Netherlands", "France")
    class25 <- c("Australia", "Brazil", "Ireland", "New Zealand", "United Kingdom",
                 "Canada", "Israel")
    class30 <- c("Japan", "Guatemala")
    class35 <- c("Singapore")
    class40 <- c("China")
    
    hh <- c("Households", "Household", "Household (no info on no. of clusters)",
            "Household/parent", "Families", "Family", 
            "households - siblings allocated to the same groups")
    classes <- c("School class", "Classes")
    
    missing.cluster.studies <- c()
    
    data$UsedICC <- NA
    data$MeanClusterSize <- NA
    data$DesignEffect <- NA
    
    #adjust all that you can
    for(i in 1:nrow(data)){
      
      if(data$StudyDesign[i]=="Cluster RCT"){
        
        ##mean cluster size
        armcluster <- subset(data$ArmClusters, data$StudyID == data$StudyID[i])
        ns <- subset(data$n, data$StudyID == data$StudyID[i])
        
        if(all(!is.na(armcluster))){
          #m <- data$n[i]/data$ArmClusters[i]
          m <- sum(ns)/sum(armcluster)
        }else if(!is.na(data$NoClusters[i])){
          m <- sum(ns)/data$NoClusters[i]
        }else if(data$UnitRand[i] %in% hh){
          m <- 2
          print(paste(data$StudyID[i], " - Assuming m=2 for unit rand:", data$UnitRand[i]))
        }else if(data$UnitRand[i] %in% classes){
          if(data$Country[i] %in% class20){
            m <- 20
            print(paste(data$StudyID[i], " - Assuming m=20 for unit rand:", data$UnitRand[i], 
                  "Country = ", data$Country[i]))
          }else if(data$Country[i] %in% class25){
            m <- 25
            print(paste(data$StudyID[i], " - Assuming m=25 for unit rand:", data$UnitRand[i], 
                  "Country = ", data$Country[i]))
          }else if(data$Country[i] %in% class30){
            m <- 30
            print(paste(data$StudyID[i], " - Assuming m=30 for unit rand:", data$UnitRand[i], 
                  "Country = ", data$Country[i]))
          }else if(data$Country[i] %in% class35){
            m <- 35
            print(paste(data$StudyID[i], " - Assuming m=35 for unit rand:", data$UnitRand[i], 
                  "Country = ", data$Country[i]))
          }else if(data$Country[i] %in% class40){
            m <- 40
            print(paste(data$StudyID[i], " - Assuming m=40 for unit rand:", data$UnitRand[i], 
                  "Country = ", data$Country[i]))
          }else{
            print(paste(data$StudyID[i], " - Country needed for cluster size:", data$Country[i]))
          }
        }else{
          print(paste(data$StudyID[i], " - Unit of randomization needed:", data$UnitRand[i]))
          missing.cluster.studies <- c(missing.cluster.studies, data$StudyID[i])
          m <- NA
        }
        
        data$MeanClusterSize[i] <- m
        
        
        #ICC
        if(!is.na(as.numeric(data$ActualICC[i]))){
          icc <- as.numeric(data$ActualICC[i])
        }else if(data$UnitRand[i] %in% hh){
          icc <- icc.hh
        }else{
          icc <- icc.other
        }
        data$UsedICC[i] <- icc
        
        
        #Design effect
        des <- 1 + (m-1)*icc
        
        data$DesignEffect[i] <- des
        data$r_adj[i] <- data$r[i]/des
        data$n_adj[i] <- data$n[i]/des
        
        
      }else{
        data$r_adj[i] <- data$r[i]
        data$n_adj[i] <- data$n[i]
      }
    }
    #calc mean design effect (with one design effect per study)
    mean.des <- data %>%
      group_by(StudyID) %>%
      slice(1) %>%
      ungroup()
    
    mean.des <- mean(mean.des$DesignEffect, na.rm = TRUE)
    print(paste("Mean design effect: ", mean.des))
    print("For studies:")
    print(missing.cluster.studies)
    
    #adjust rest with mean design effect
    for(i in 1:nrow(data)){
      if(data$StudyID[i] %in% missing.cluster.studies){
        data$DesignEffect[i] <- mean.des
        data$r_adj[i] <- data$r[i]/mean.des
        data$n_adj[i] <- data$n[i]/mean.des
      }
    }
  }
  data
}

combine_arms_fornma <- function(df){
  df$NoArmsNMA <- NA #redefine number of arms
  
  df_new <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(df_new) <- colnames(df)
  
  studies <- unique(df$StudyID)
  
  df$ArmClusters <- as.numeric(df$ArmClusters)
  
  count <- 0
  for(i in 1:length(studies)){
    dfi <- subset(df, StudyID==studies[i])
    
    dfi_combined <- dfi %>%
      group_by(StudyID, StudyName, StudyDesign, UnitRand,
               ActualICC, AssumedICC, IntCategory, NoArmsAnalysed, NoArmsNMA,
               NoClusters, Country, AgeGroup, VaccineType, PostCovid, ROB, HighROB) %>%
      summarize(ArmClusters = sum(ArmClusters), n = sum(n), r = sum(r), 
                n_adj = sum(n_adj), r_adj = sum(r_adj), .groups="keep")
    
    for(k in 1:nrow(dfi_combined)){
      dfi_combined$NoArmsNMA[k] <- nrow(dfi_combined)
    }
    
    if(nrow(dfi_combined)>1){
      df_new <- rbind(df_new, dfi_combined)  
    }else{
      print(paste("Study removed - identical arms:", dfi_combined$StudyID[1], dfi_combined$StudyName[1],
                  dfi_combined$IntCategory[1]))
      count <- count + 1
    }
  }
  print(paste(count, "studies dropped for identical arms"))
  
  df_new
}

# 1 = Control, 2 = Access, 3 = Affordability, 4 = Education, 5 = Reminder,
# 6 = Education and Reminder, 7 = Multicomponent              
label_trts <- function(df){
  df$Trt <- NA
  for(i in 1:nrow(df)){
    if(df$IntCategory[i]=="Control"){
      df$Trt[i] <- 1
    }else if(df$IntCategory[i]=="Access"){
      df$Trt[i] <- 2
    }else if(df$IntCategory[i]=="Affordability"){
      df$Trt[i] <- 3
    }else if(df$IntCategory[i]=="Education"){
      df$Trt[i] <- 4
    }else if(df$IntCategory[i]=="Reminder"){
      df$Trt[i] <- 5
    }else if(df$IntCategory[i]=="Education and Reminder"){
      df$Trt[i] <- 6
    }else if(df$IntCategory[i]=="Multicomponent"){
      df$Trt[i] <- 7
    }else{
      print(paste("Invalid treatment group:", df$StudyID[i], df$StudyName[i], df$IntCategory[i]))
    }
  }
  
  df <- df[order(df$Trt),] #by arm
  df <- df[order(df$StudyID),] #by study
  
  df
}

centre_covs <- function(data, covs, standardise = FALSE){
  
  #centre all covariates (doesn't matter if study level are repeated over arms)
  centred_vals <- c()
  sd_vals <- c()
  for(i in 1:ncol(data)){
    if(colnames(data)[i] %in% covs){
      #calculate mean and SD across intervention arms only (where ControlType=NA)
      meanval <- mean(data[[i]][is.na(data$ControlType)], na.rm=TRUE)
      sdval <- sd(data[[i]][is.na(data$ControlType)], na.rm=TRUE)
      
      centred_vals <- c(centred_vals, meanval)
      sd_vals <- c(sd_vals, sdval)
      
      if(standardise){
        data[[i]] <- (data[[i]]-meanval)/sdval
      }else{
        data[[i]] <- data[[i]]-meanval
      }
    }
  }
  
  #save centred values
  df_centred <- as.data.frame(cbind(covs,centred_vals, sd_vals))
  list(data, df_centred)
}

get_contrast_covs <- function(data, covs, covs_arm){
  
  df <- data[, c("StudyID", "IntLabel", "ControlActive", "RefArm", "ControlType", 
                 "NoArmsAnalysed", "ROB", "HighROB", covs)]
  df$RefLabel <- NA
  
  
  x<-1
  while(x<=nrow(df)){
    #print(x)
    if(df$ControlActive[x]=="Active"){
      n_a <- df$NoArmsAnalysed[x]
      #find the reference arm
      for(i in 0:(n_a-1)){
        if(df$RefArm[x+i]=="Y"){
          ind <- i
        }
      }
      
      
      cov_inds <- which(colnames(df) %in% covs_arm)
      for(i in 0:(n_a-1)){
        if(i != ind){
          df$RefLabel[x+i] <- df$IntLabel[x+ind]
          for(j in 1:length(covs_arm)){
            #print(paste(covs_J[j], ints[j]))
            #cov_inds[j] is the index of the column, x+i is the index of the row
            df[[x+i, cov_inds[j]]] <- df[[x+i, cov_inds[j]]] - df[[x+ind, cov_inds[j]]]
            #print("okay")
          }
        }
      }
      
      #move on to next study
      x <- x + n_a
      
    }else{
      df$RefLabel[x] <- "Control"
      x <- x+1
    }
  }
  
  #now remove all rows where RefArm=Y
  df<-subset(df, RefArm=="N")
  df
  
}

check_active_comp <- function(data, data_cont, covs_arm){
  
  active_arms <- sum(data$ControlActive=="Active")#66
  active_studies <- length(unique(data$StudyID[data$ControlActive == "Active"]))#29
  print(paste("Active comparisons:", active_studies, "studies,", active_arms, "arms."))
  
  rows_all_zero <- apply(data_cont[covs_arm], 1, function(row) all(row == 0))
  
  # Print StudyID for rows where the condition is TRUE
  study_zero <- data_cont$StudyID[rows_all_zero]
  arm_zero <- data_cont$IntLabel[rows_all_zero]
  df_allzero <- as.data.frame(cbind(study_zero, arm_zero))
  
  df_allzero
}
