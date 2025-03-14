subgroup_int_notall0 <- function(data){
  
  studies <- unique(data$StudyID)
  N <- length(studies)
  
  #to exlcude arms - create an arm label
  data$ArmLabel <- paste(data$StudyID, data$IntLabel)
  
  #find arms where any intervention has all 'interactive elements'=0
  studies_to_exclude <- c()
  arms_to_exclude <- c()
  
  for(i in 1:N){
    dati <- subset(data, StudyID==studies[i])
    
    arms <- c()
    for(j in 1:nrow(dati)){
      if(dati$IntCategory[j]!="Control"){
        if(dati$Access_AppointmentSchedulingHelp[j]==0 &&
           dati$Access_AppointmentSchedulingOnline[j]==0 &&
           dati$Acceptance_VaccineSafety[j]==0 && 
           dati$Acceptance_DiseasePerceivedRisk[j]==0 &&
           dati$Acceptance_SocialFactors[j]==0 &&
           dati$Acceptance_DecisionAids[j]==0 &&
           dati$Acceptance_MotivationalInterviewing[j]==0 &&
           dati$Activation[j]==0){
          arms <- c(arms, dati$ArmLabel[j])
        }
      }
    }
    
    if(length(arms)>0){
      if(nrow(dati)-length(arms)<2){
        #remove study
        studies_to_exclude <- c(studies_to_exclude, dati$StudyID[1])
      }else{
        #remove arms
        arms_to_exclude <- c(arms_to_exclude, arms)
      }
    }
    
    
  }
  
  print("Studies exlcuded (all interactive elements=0):")
  print(studies_to_exclude)
  print("Arms exlcuded (all interactive elements=0):")
  print(arms_to_exclude)
  data <- subset(data, !StudyID%in%studies_to_exclude)
  data <- subset(data, !ArmLabel%in%arms_to_exclude)
  data
  
}

subgroup_int_all0 <- function(data){
  
  studies <- unique(data$StudyID)
  N <- length(studies)
  
  #to exlcude arms - create an arm label
  data$ArmLabel <- paste(data$StudyID, data$IntLabel)
  
  #find arms where any intervention has all 'interactive elements'=0
  studies_to_exclude <- c()
  arms_to_exclude <- c()
  
  for(i in 1:N){
    dati <- subset(data, StudyID==studies[i])
    
    arms <- c()
    for(j in 1:nrow(dati)){
      if(dati$IntCategory[j]!="Control"){
        if(dati$Access_AppointmentSchedulingHelp[j]==0 &&
           dati$Access_AppointmentSchedulingOnline[j]==0 &&
           dati$Acceptance_VaccineSafety[j]==0 && 
           dati$Acceptance_DiseasePerceivedRisk[j]==0 &&
           dati$Acceptance_SocialFactors[j]==0 &&
           dati$Acceptance_DecisionAids[j]==0 &&
           dati$Acceptance_MotivationalInterviewing[j]==0 &&
           dati$Activation[j]==0){
          #do nothing
        }else{
          arms <- c(arms, dati$ArmLabel[j])
        }
      }
    }
    
    if(length(arms)>0){
      if(nrow(dati)-length(arms)<2){
        #remove study
        studies_to_exclude <- c(studies_to_exclude, dati$StudyID[1])
      }else{
        #remove arms
        arms_to_exclude <- c(arms_to_exclude, arms)
      }
    }
    
    
  }
  
  print("Studies exlcuded (not all interactive elements=0):")
  print(studies_to_exclude)
  print("Arms exlcuded (not all interactive elements=0):")
  print(arms_to_exclude)
  data <- subset(data, !StudyID%in%studies_to_exclude)
  data <- subset(data, !ArmLabel%in%arms_to_exclude)
  data
  
}