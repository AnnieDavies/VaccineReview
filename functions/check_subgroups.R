check_sg <- function(data_sg, folder_name, sg_path, df_study01){  
  write_xlsx(data_sg, paste0(sg_path, folder_name,"Data_subgroup.xlsx"))
  
  int_cov_counts <- plot_int_info_corr(data_sg, txtsize = 10)
  ggsave(paste0(sg_path, folder_name, "Counts_Ints.png"), int_cov_counts[[3]], width = 15, height = 11, units = "cm")
  
  int.counts<-int_cov_counts[[1]]
  int.counts$CountsOne <- int.counts$`Proportion of arms coded as 1`*sum(data_sg$IntCategory!="Control")
  int.counts$CountsZero <- sum(data_sg$IntCategory!="Control") - int.counts$CountsOne
  
  write_xlsx(list(Counts = int.counts,
                  Correlations = as.data.frame(int_cov_counts[[2]])),
             paste0(sg_path,folder_name,"Counts_Correlations_Ints.xlsx"))
  
  # Counts - study
  data_study_sg <- subset(df_study01, df_study01$StudyID %in% data_sg$StudyID)
  study_cov_counts <- plot_study_info_corr(data_study_sg, txtsize = 12)
  ggsave(paste0(sg_path,folder_name,"Counts_Study.png"), study_cov_counts[[3]], width = 15, height = 11, units = "cm")
  
  study.counts<-study_cov_counts[[1]]
  study.counts$CountsOne <- study.counts$`Proportion of studies coded as 1`*nrow(data_study_sg)
  study.counts$CountsZero <- nrow(data_study_sg) - study.counts$CountsOne
  
  write_xlsx(list(Info = study.counts,
                  Correlations = as.data.frame(study_cov_counts[[2]])),
             paste0(sg_path, folder_name, "Counts_Correlations_Study.xlsx"))
}