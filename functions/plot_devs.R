plot_devs <- function(df.re.dev, cutoff_plot = 0, cutoff_col = NA){
  df.re.dev$ind <- rownames(df.re.dev)
  colnames(df.re.dev)<- c("LowCI_95","LowCI_50", "Median","HighCI_50","HighCI_95","ind")
  
  df.re.dev <- df.re.dev %>%
    mutate(
      study_ind = as.numeric(str_extract(ind, "(?<=i)\\d+")),  # Extract digits after 'i'
      Arm = str_extract(ind, "(?<=k)\\d+"),       # Extract digits after 'k'
      StudyID = studies[study_ind],
      label = paste0(StudyID, " arm ", Arm)
    )
  # Add a column to define color by whether Median > 2
  if(!is.na(cutoff_col)){
    df.re.dev$color <- ifelse(df.re.dev$Median > cutoff_col, "orange", "black")
  }else{
    df.re.dev$color <- "black"
  }
  
  
  df.dev.plot <- subset(df.re.dev, Median>cutoff_plot)
  
  # Plot
  devplot<-ggplot(df.dev.plot, aes(x = label)) +
    geom_linerange(aes(ymin = LowCI_95, ymax = HighCI_95, color = color), linewidth = 0.5) + # Thin line
    geom_linerange(aes(ymin = LowCI_50, ymax = HighCI_50, color = color), linewidth = 1.5) + # Thick line
    geom_point(aes(y = Median, color = color), size = 3) + # Median point
    scale_color_identity() + # Use colors directly from the dataframe
    labs(
      x = "StudyID and arm",
      y = "Residual Deviance",
      title = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  list(devplot, df.re.dev)
}