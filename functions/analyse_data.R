get_MA_dat <- function(nma_dat, studies, t1, t2){
  r1 <- c()
  r2 <- c()
  
  n1 <- c()
  n2 <- c()
  
  IDS <- c()
  names <- c()
  
  for(i in 1:N){
    dati <- subset(nma_dat, StudyID==studies[i])
    if(t1 %in% dati$Trt && t2 %in% dati$Trt){
      ind1 <- which(dati$Trt==t1)
      ind2 <- which(dati$Trt==t2)
      
      r1 <- c(r1, dati$r_adj[ind1])
      r2 <- c(r2, dati$r_adj[ind2])
      
      n1 <- c(n1, dati$n_adj[ind1])
      n2 <- c(n2, dati$n_adj[ind2])
      
      IDS <- c(IDS, studies[i])
      names <- c(names, dati$StudyName[ind1])
    }
    
  }
  
  res <- list(r1, n1, r2, n2, IDS, names)
  res
}

calc_TE <- function(r1, n1, r2, n2){
  
  r1 <- r1+0.5
  n1 <- n1+0.5
  r2 <- r2+0.5
  n2 <- n2+0.5
  
  p1 <- r1/n1
  p2 <- r2/n2
  
  TE <- log(p2/(1-p2)) - log(p1/(1-p1))
  var <- 1/(n1*p1*(1-p1))+1/(n2*p2*(1-p2))
  
  lci <- TE - 1.96*sqrt(var)
  uci <- TE + 1.96*sqrt(var)
  
  
  list(TE, lci, uci)
}

get_TE <- function(r1, n1, r2, n2){
  TE <- c()
  lci <- c()
  uci <- c()
  for(i in 1:length(r1)){
    res <- calc_TE(r1[i], n1[i], r2[i], n2[i])
    TE <- c(TE, res[[1]])
    lci <- c(lci, res[[2]])
    uci <- c(uci, res[[3]])
  }
  list(TE, lci, uci)
}


# Function to calculate plot dimensions
get_plot_dimensions <- function(plot) {
  temp_file <- tempfile(fileext = ".png")
  png(temp_file)
  replayPlot(plot)
  dev.off()
  
  dims <- dim(png::readPNG(temp_file))
  unlink(temp_file)
  
  width <- dims[2] / 100  # Convert to inches (assuming 100 pixels per inch)
  height <- dims[1] / 100 # Convert to inches
  
  return(list(width = width, height = height))
}

calc_devs_re <- function(samp.sum, m, r, n, cnma = FALSE, q=0){
  num.dev <- length(r)
  
  if(cnma){
    dev.pos <- m+2
    rhat.pos <- m+q+num.dev+2
    tau.pos <- m+q+2*num.dev+2
    Dres.pos <- m+q+2*num.dev+3
  }else{
    dev.pos <- m+1
    rhat.pos <- m+num.dev+1
    tau.pos <- m+2*num.dev+1
    Dres.pos <- m+2*num.dev+2
  }
  
  #separate out results
  #re.effects <- samp.sum[1:num.d, ]
  #re.tau <- samp.sum[tau.pos, ]
  
  re.dev <- samp.sum[dev.pos:(dev.pos+num.dev-1),]
  re.rhat <- samp.sum[rhat.pos:(rhat.pos+num.dev-1),]
  re.Dres <- samp.sum[Dres.pos, ]
  
  #re.dev.vec <- re.dev$X50. #residual deviance per data point
  #names(re.dev.vec) <- rownames(re.dev)
  
  
  ## Calculate leverage and DIC
  lev <- c()
  for(i in 1:num.dev){
    #calculate the posterior mean of the residual deviance for data point i
    post.dev <- re.dev$X50.[i]
    #calculate the posterior mean of rhat for that data point
    post.rhat <- re.rhat$X50.[i]
    #calculate the deviance of posterior mean of rhat
    if(r[i]>0){
      term1 <- r[i]*(log(r[i]) - log(post.rhat))
    }else{
      term1 <- 0
    }
    if(r[i]!=n[i]){
      term2 <- (n[i]-r[i])*(log(n[i]-r[i]) - log(n[i]-post.rhat))
    }else{
      term2 <- 0
    }
    
    dev.post.rhat <- 2*(term1 + term2)
    
    #leverage
    lev <- c(lev, post.dev - dev.post.rhat)
    
    if(is.na(post.dev - dev.post.rhat)){
      print(paste("row = ", i, ", post.dev = ", post.dev, ", post.rhat = ", post.rhat, 
                  ", r = ", r[i], ", n = ", n[i], sep = ""))
      print(paste("log(r) = ", log(r[i]), ", log(n-r) = ", log(n[i]-r[i]), 
                  ", log(rhat) = ", log(post.rhat), ", log(n-rhat)", "log(n[i]-post.rhat)", sep = ""))
    }
    
  }
  pD <- sum(lev)
  DIC <- re.Dres$X50.[1] + pD
  
  #list(re.dev.vec, pD, DIC)
  list(re.dev, pD, DIC)
}


calc_devs_fe <- function(samp.sum, m, r, n, cnma = FALSE, q=0){
  num.dev <- length(r)
  
  if(cnma){
    dev.pos <- m+2
    rhat.pos <- m+q+num.dev+2
    Dres.pos <- m+q+2*num.dev+2
  }else{
    dev.pos <- num.d+1
    rhat.pos <- num.d+num.dev+1
    Dres.pos <- num.d+2*num.dev+1
  }
  
  
  #separate out results
  #re.effects <- samp.sum[1:num.d, ]
  
  re.dev <- samp.sum[dev.pos:(dev.pos+num.dev-1),]
  re.rhat <- samp.sum[rhat.pos:(rhat.pos+num.dev-1),]
  re.Dres <- samp.sum[Dres.pos, ]
  
  re.dev.vec <- re.dev$X50. #residual deviance per data point
  names(re.dev.vec) <- rownames(re.dev)
  ## Calculate leverage and DIC
  lev <- c()
  for(i in 1:num.dev){
    #calculate the posterior mean of the residual deviance for data point i
    post.dev <- re.dev$X50.[i]
    #calculate the posterior mean of rhat for that data point
    post.rhat <- re.rhat$X50.[i]
    #calculate the deviance of posterior mean of rhat
    if(r[i]>0){
      term1 <- r[i]*(log(r[i]) - log(post.rhat))
    }else{
      term1 <- 0
    }
    if(r[i]!=n[i]){
      term2 <- (n[i]-r[i])*(log(n[i]-r[i]) - log(n[i]-post.rhat))
    }else{
      term2 <- 0
    }
    
    dev.post.rhat <- 2*(term1 + term2)
    
    #leverage
    lev <- c(lev, post.dev - dev.post.rhat)
    
    if(is.na(post.dev - dev.post.rhat)){
      print(paste("row = ", i, ", post.dev = ", post.dev, ", post.rhat = ", post.rhat, 
                  ", r = ", r[i], ", n = ", n[i], sep = ""))
      print(paste("log(r) = ", log(r[i]), ", log(n-r) = ", log(n[i]-r[i]), 
                  ", log(rhat) = ", log(post.rhat), ", log(n-rhat)", "log(n[i]-post.rhat)", sep = ""))
    }
    
  }
  pD <- sum(lev)
  DIC <- re.Dres$X50.[1] + pD
  
  list(re.dev.vec, pD, DIC)
}

get_counts_all <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  #Post-covid
  column_cov <- data_unique[[covs_z[1]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  #AGE
  ages <- covs_z[2:6]
  age_cols <- data_unique[ , ages]
  count_adults <- sum(rowSums(age_cols < 0, na.rm = TRUE) == length(ages))
  for(i in 1:length(ages)){
    column_cov <- data_unique[[ages[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_adults))
  }
  
  #Setting
  setting <- covs_z[7:9]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  column_cov <- data_unique[[covs_z[11]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}

get_counts_noage <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  #Post-covid
  column_cov <- data_unique[[covs_z[1]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  
  #Setting
  setting <- covs_z[2:4]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  column_cov <- data_unique[[covs_z[6]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}

get_counts_seasonal <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  #Post-covid
  column_cov <- data_unique[[covs_z[1]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  
  #Setting
  setting <- covs_z[2:3]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  column_cov <- data_unique[[covs_z[5]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}

get_counts_nocovid <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  
  #AGE
  ages <- covs_z[1:5]
  age_cols <- data_unique[ , ages]
  count_adults <- sum(rowSums(age_cols < 0, na.rm = TRUE) == length(ages))
  for(i in 1:length(ages)){
    column_cov <- data_unique[[ages[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_adults))
  }
  
  #Setting
  setting <- covs_z[6:8]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  column_cov <- data_unique[[covs_z[10]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}

get_counts_nocovid_nopreg <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  
  #AGE
  ages <- covs_z[1:4]
  age_cols <- data_unique[ , ages]
  count_adults <- sum(rowSums(age_cols < 0, na.rm = TRUE) == length(ages))
  for(i in 1:length(ages)){
    column_cov <- data_unique[[ages[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_adults))
  }
  
  #Setting
  setting <- covs_z[5:7]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  column_cov <- data_unique[[covs_z[9]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}


get_counts_nounderserved <- function(data, covs_x, covs_z){
  counts <- c()
  for(i in 1:length(covs_x)){
    column_cov <- data[[covs_x[i]]]
    count_neg <- sum(column_cov < 0, na.rm = TRUE)
    count_pos <- sum(column_cov > 0, na.rm = TRUE)
    counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  }
  
  data_unique <- data %>%
    distinct(StudyID, .keep_all = TRUE)
  
  #Post-covid
  column_cov <- data_unique[[covs_z[1]]]
  count_neg <- sum(column_cov < 0)
  count_pos <- sum(column_cov > 0)
  counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  #AGE
  ages <- covs_z[2:6]
  age_cols <- data_unique[ , ages]
  count_adults <- sum(rowSums(age_cols < 0, na.rm = TRUE) == length(ages))
  for(i in 1:length(ages)){
    column_cov <- data_unique[[ages[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_adults))
  }
  
  #Setting
  setting <- covs_z[7:9]
  setting_cols <- data_unique[ , setting]
  count_health <- sum(rowSums(setting_cols < 0, na.rm = TRUE) == length(setting))
  for(i in 1:length(setting)){
    column_cov <- data_unique[[setting[i]]]
    count_pos <- sum(column_cov > 0)
    counts <- c(counts, paste0(count_pos, " vs ", count_health))
  }
  
  #Prop males
  counts <- c(counts,"n/a")
  
  #Underserved
  #column_cov <- data_unique[[covs_z[11]]]
  #count_neg <- sum(column_cov < 0)
  #count_pos <- sum(column_cov > 0)
  #counts <- c(counts, paste0(count_pos, " vs ", count_neg))
  
  counts
}

