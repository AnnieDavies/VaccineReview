######
# Function to plot combined plots
######
format_sg_dat <- function(re.effects, ma_dat, trts, num.d){
  sg1.effects <- re.effects[(num.d+1):(2*num.d),]
  sg2.effects <- re.effects[(2*num.d+1):(3*num.d),]
  
  # Extract row names for each data frame
  sg1_rows <- rownames(sg1.effects)
  sg2_rows <- rownames(sg2.effects)
  
  # Add 'Covariate' and 'Group' columns to each data frame
  sg1.effects <- sg1.effects %>%
    mutate(
      Covariate = trts[2:7],
      #Group = "Pre COVID-19"
      Group = "Pre 2020"
    )
  
  sg2.effects <- sg2.effects %>%
    mutate(
      Covariate = trts[2:7],
      #Group = "Post COVID-19"
      Group = "2020 onwards"
    )
  
  # Combine the two data frames by row binding them together
  sg.effects <- rbind(sg1.effects, sg2.effects)
  
  # Arrange by covariate name if you want them ordered by covariate
  sg.effects <- sg.effects[order(sg.effects$Covariate), ]
  
  #get number of trials/participants
  sg.effects$NumTrials <- NA
  sg.effects$NumParticipants <- NA
  sg.effects$Num<-NA
  for(i in 1:nrow(sg.effects)){
    #dati <- subset(ma_dat, IntCategory==sg.effects$Covariate[i] & PostCovid==ifelse(sg.effects$Group[i]=="Post COVID-19",1,0))
    dati <- subset(ma_dat, IntCategory==sg.effects$Covariate[i] & PostCovid==ifelse(sg.effects$Group[i]=="2020 onwards",1,0))
    sg.effects$NumTrials[i]<-nrow(dati)
    sg.effects$NumParticipants[i]<-round(sum(dati$n))
    sg.effects$Num[i]<-paste0(sg.effects$NumTrials[i], " (", sg.effects$NumParticipants[i], ")")
  }
  
  
  sg.effects
}

format_sg3_dat <- function(re.effects, ma_dat, trts, num.d){
  sg1.effects <- re.effects[(num.d+1):(2*num.d),]
  sg2.effects <- re.effects[(2*num.d+1):(3*num.d),]
  sg3.effects <- re.effects[(3*num.d+1):(4*num.d),]
  
  # Extract row names for each data frame
  sg1_rows <- rownames(sg1.effects)
  sg2_rows <- rownames(sg2.effects)
  sg3_rows <- rownames(sg3.effects)
  
  # Add 'Covariate' and 'Group' columns to each data frame
  sg1.effects <- sg1.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Children"
    )
  
  sg2.effects <- sg2.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Adolescents"
    )
  
  sg3.effects <- sg3.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Adults"
    )
  
  # Combine the three data frames by row binding them together
  sg.effects <- rbind(sg1.effects, sg2.effects, sg3.effects)
  
  # Arrange by covariate name if you want them ordered by covariate
  sg.effects <- sg.effects[order(sg.effects$Covariate), ]
  
  #get number of trials/participants
  sg.effects$NumTrials <- NA
  sg.effects$NumParticipants <- NA
  sg.effects$Num<-NA
  for(i in 1:nrow(sg.effects)){
    if(sg.effects$Group[i]=="Children"){
      ages <- c("Young children", "Children")
    }else if(sg.effects$Group[i]=="Adolescents"){
      ages <- c("Adolescents")
    }else{
      ages <- c("Adults", "Older adults", "Pregnant women")
    }
    
    dati <- subset(ma_dat, IntCategory==sg.effects$Covariate[i] & 
                     AgeGroup %in% ages)
    sg.effects$NumTrials[i]<-nrow(dati)
    sg.effects$NumParticipants[i]<-round(sum(dati$n))
    sg.effects$Num[i]<-paste0(sg.effects$NumTrials[i], " (", sg.effects$NumParticipants[i], ")")
  }
  
  sg.effects
}

format_sg6_dat <- function(re.effects, ma_dat, trts, num.d){
  sg1.effects <- re.effects[(5*num.d+1):(6*num.d),]
  sg2.effects <- re.effects[(6*num.d+1):(7*num.d),]
  sg3.effects <- re.effects[(7*num.d+1):(8*num.d),]
  sg4.effects <- re.effects[(8*num.d+1):(9*num.d),]
  sg5.effects <- re.effects[(9*num.d+1):(10*num.d),]
  sg6.effects <- re.effects[(10*num.d+1):(11*num.d),]
  
  # Extract row names for each data frame
  sg1_rows <- rownames(sg1.effects)
  sg2_rows <- rownames(sg2.effects)
  sg3_rows <- rownames(sg3.effects)
  sg4_rows <- rownames(sg4.effects)
  sg5_rows <- rownames(sg5.effects)
  sg6_rows <- rownames(sg6.effects)
  
  # Add 'Covariate' and 'Group' columns to each data frame
  sg1.effects <- sg1.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Young children"
    )
  
  sg2.effects <- sg2.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Children"
    )
  
  sg3.effects <- sg3.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Adolescents"
    )
  
  sg4.effects <- sg4.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Adults"
    )
  
  sg5.effects <- sg5.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Older adults"
    )
  
  sg6.effects <- sg6.effects %>%
    mutate(
      Covariate = trts[2:7],
      Group = "Pregnant women"
    )
  
  # Combine the three data frames by row binding them together
  sg.effects <- rbind(sg1.effects, sg2.effects, sg3.effects, sg4.effects, sg5.effects, sg6.effects)
  
  # Arrange by covariate name if you want them ordered by covariate
  sg.effects <- sg.effects[order(sg.effects$Covariate), ]
  
  #get number of trials/participants
  sg.effects$NumTrials <- NA
  sg.effects$NumParticipants <- NA
  sg.effects$Num<-NA
  for(i in 1:nrow(sg.effects)){

    dati <- subset(ma_dat, IntCategory==sg.effects$Covariate[i] & 
                     AgeGroup == sg.effects$Group[i])
    sg.effects$NumTrials[i]<-nrow(dati)
    sg.effects$NumParticipants[i]<-round(sum(dati$n))
    sg.effects$Num[i]<-paste0(sg.effects$NumTrials[i], " (", sg.effects$NumParticipants[i], ")")
    
    if(sg.effects$NumTrials[i]==0){
      sg.effects$Median[i]<-NA
      sg.effects$`Lower 95% CI`[i]<-NA
      sg.effects$`Upper 95% CI`[i] <-NA
    }
    
    
  }
  
  sg.effects
}


plot_comb_LOR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  p <- nrow(int.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- 2*p+2 
  top_txt <- 2*p+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:p){
    val <- p + 1 - i
    
    l <-2*val-2
    h <- 2*val-1
    m <- (l+h)/2
    
    #rows <- append(rows, l:h)
    rows <- append(rows, h:l)
    rows2 <- append(rows2, m)
    
    sg.effects$Covariate[2*i]<-""
    
    
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:p){
    if(i %% 2 != 0){
      l <- rows[2*i -1]
      h <- rows[2*i]
      shade <- append(shade, l:h)
    }
  }
  
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.19 #space from study to LHS of SG
  cov_space2 <- xrange*0.21 #space from SG to LHS of N
  est_space <- xrange*0.23 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",2*p),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  text(xlow, 4.5, "Reminder", pos=4)
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:(2*p)){
    
    # est <- paste(round(sg.effects$Median[i],2), " [", round(sg.effects$`Lower 95% CI`[i],2), ", ", 
    #              round(sg.effects$`Upper 95% CI`[i],2), "]", sep = "")
    est <- paste0(sprintf("%.2f", sg.effects$Median[i]), 
                  " [", sprintf("%.2f", sg.effects$`Lower 95% CI`[i]), 
                  ", ", sprintf("%.2f", sg.effects$`Upper 95% CI`[i]), "]")
    
    lab <- c(lab, est)
    
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "LOR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:p){
    # est <- paste(round(int.effects$Median[i],2), " [", round(int.effects$`Lower 95% CI`[i],2), ", ", 
    #              round(int.effects$`Upper 95% CI`[i],2), "]", sep = "")
    est <- paste0(sprintf("%.2f", int.effects$Median[i]), 
                  " [", sprintf("%.2f", int.effects$`Lower 95% CI`[i]), 
                  ", ", sprintf("%.2f", int.effects$`Upper 95% CI`[i]), "]")
    
    
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", p),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
  
  
}


plot_comb_OR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  p <- nrow(int.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- 2*p+2 
  top_txt <- 2*p+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:p){
    val <- p + 1 - i
    
    l <-2*val-2
    h <- 2*val-1
    m <- (l+h)/2
    
    #rows <- append(rows, l:h)
    rows <- append(rows, h:l)
    rows2 <- append(rows2, m)
    
    sg.effects$Covariate[2*i]<-""
    
    
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:p){
    if(i %% 2 != 0){
      l <- rows[2*i -1]
      h <- rows[2*i]
      shade <- append(shade, l:h)
    }
  }
  
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.19 #space from study to LHS of SG
  cov_space2 <- xrange*0.21 #space from SG to LHS of N
  est_space <- xrange*0.23 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",2*p),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  text(xlow, 4.5, "Reminder", pos=4)
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:(2*p)){
    
    # est <- paste(round(exp(sg.effects$Median[i]),2), " [", round(exp(sg.effects$`Lower 95% CI`[i]),2), ", ", 
    #              round(exp(sg.effects$`Upper 95% CI`[i]),2), "]", sep = "")
    est <- paste0(sprintf("%.2f", exp(sg.effects$Median[i])), 
                  " [", sprintf("%.2f", exp(sg.effects$`Lower 95% CI`[i])), 
                  ", ", sprintf("%.2f", exp(sg.effects$`Upper 95% CI`[i])), "]")
    
    lab <- c(lab, est)
    
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "OR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:p){
    # est <- paste(round(exp(int.effects$Median[i]),2), " [", round(exp(int.effects$`Lower 95% CI`[i]),2), ", ", 
    #              round(exp(int.effects$`Upper 95% CI`[i]),2), "]", sep = "")
    est <- paste0(sprintf("%.2f", exp(int.effects$Median[i])), 
                  " [", sprintf("%.2f", exp(int.effects$`Lower 95% CI`[i])), 
                  ", ", sprintf("%.2f", exp(int.effects$`Upper 95% CI`[i])), "]")
    
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", p),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
}


plot_comb_age_LOR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  #p <- nrow(int.effects)
  l <- nrow(sg.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- l+2 
  top_txt <- l+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:(l/3)){
    val <- (l/3) + 1 - i
    
    g1 <-3*val-1
    g2 <- 3*val-2
    g3 <- 3*val-3
    
    #rows <- append(rows, l:h)
    rows <- append(rows, g1:g3)
    rows2 <- append(rows2, g2:g3)
    
    sg.effects$Covariate[3*i]<-""
    sg.effects$Covariate[3*i-1]<-""
    
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:(l/3)){
    if(i %% 2 != 0){
      g1 <- rows[3*i -2]
      g3 <- rows[3*i]
      shade <- append(shade, g1:g3)
    }
  }
  
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.2 #space from study to LHS of SG
  cov_space2 <- xrange*0.17 #space from SG to LHS of N
  est_space <- xrange*0.23 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",l),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  text(xlow, 7.5, "Reminder", pos=4)
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:l){
    
    # est <- paste(round(sg.effects$Median[i],2), " [", round(sg.effects$`Lower 95% CI`[i],2), ", ", 
    #              round(sg.effects$`Upper 95% CI`[i],2), "]", sep = "")
    est <- paste0(sprintf("%.2f", sg.effects$Median[i]), 
                  " [", sprintf("%.2f", sg.effects$`Lower 95% CI`[i]), 
                  ", ", sprintf("%.2f", sg.effects$`Upper 95% CI`[i]), "]")
    
    lab <- c(lab, est)
    
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "LOR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:(l*2/3)){
    # est <- paste(round(int.effects$Median[i],2), " [", round(int.effects$`Lower 95% CI`[i],2), ", ", 
    #              round(int.effects$`Upper 95% CI`[i],2), "]", sep = "")
    est <- paste0(sprintf("%.2f", int.effects$Median[i]), 
                  " [", sprintf("%.2f", int.effects$`Lower 95% CI`[i]), 
                  ", ", sprintf("%.2f", int.effects$`Upper 95% CI`[i]), "]")
    
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", l*2/3),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
  
  
}


plot_comb_age_OR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  #p <- nrow(int.effects)
  l <- nrow(sg.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- l+2 
  top_txt <- l+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:(l/3)){
    val <- (l/3) + 1 - i
    
    g1 <-3*val-1
    g2 <- 3*val-2
    g3 <- 3*val-3
    
    #rows <- append(rows, l:h)
    rows <- append(rows, g1:g3)
    rows2 <- append(rows2, g2:g3)
    
    sg.effects$Covariate[3*i]<-""
    sg.effects$Covariate[3*i-1]<-""
    
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:(l/3)){
    if(i %% 2 != 0){
      g1 <- rows[3*i -2]
      g3 <- rows[3*i]
      shade <- append(shade, g1:g3)
    }
  }
  
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.2 #space from study to LHS of SG
  cov_space2 <- xrange*0.17 #space from SG to LHS of N
  est_space <- xrange*0.23 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",l),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  text(xlow, 7.5, "Reminder", pos=4)
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:l){
    
    # est <- paste(round(exp(sg.effects$Median[i]),2), " [", round(exp(sg.effects$`Lower 95% CI`[i]),2), ", ", 
    #              round(exp(sg.effects$`Upper 95% CI`[i]),2), "]", sep = "")
    est <- paste0(sprintf("%.2f", exp(sg.effects$Median[i])), 
                  " [", sprintf("%.2f", exp(sg.effects$`Lower 95% CI`[i])), 
                  ", ", sprintf("%.2f", exp(sg.effects$`Upper 95% CI`[i])), "]")
    
    lab <- c(lab, est)
    
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "OR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:(l*2/3)){
    # est <- paste(round(exp(int.effects$Median[i]),2), " [", round(exp(int.effects$`Lower 95% CI`[i]),2), ", ", 
    #              round(exp(int.effects$`Upper 95% CI`[i]),2), "]", sep = "")
    est <- paste0(sprintf("%.2f", exp(int.effects$Median[i])), 
                  " [", sprintf("%.2f", exp(int.effects$`Lower 95% CI`[i])), 
                  ", ", sprintf("%.2f", exp(int.effects$`Upper 95% CI`[i])), "]")
    
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", l*2/3),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
  
  
}





plot_comb_age_all_LOR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  #p <- nrow(int.effects)
  l <- nrow(sg.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- l+2 
  top_txt <- l+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:(l/6)){
    val <- (l/6) + 1 - i
    
    g1 <-6*val-1
    g2 <- 6*val-2
    g3 <- 6*val-3
    g4 <- 6*val-4
    g5 <- 6*val-5
    g6 <- 6*val-6
    
    #rows <- append(rows, l:h)
    rows <- append(rows, g1:g6)
    rows2 <- append(rows2, g2:g6)
    
    sg.effects$Covariate[6*i]<-""
    sg.effects$Covariate[6*i-1]<-""
    sg.effects$Covariate[6*i-2]<-""
    sg.effects$Covariate[6*i-3]<-""
    sg.effects$Covariate[6*i-4]<-""
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:(l/6)){
    if(i %% 2 != 0){
      g1 <- rows[6*i -5]
      g3 <- rows[6*i]
      shade <- append(shade, g1:g3)
    }
  }
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.2 #space from study to LHS of SG
  cov_space2 <- xrange*0.2 #space from SG to LHS of N
  est_space <- xrange*0.2 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  options(na.action = "na.pass")
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",l),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  text(xlow, 16.25, "Reminder", pos=4)
  
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:l){
    if(!is.na(sg.effects$Median[i])){
      # est <- paste(round(sg.effects$Median[i],2), " [", round(sg.effects$`Lower 95% CI`[i],2), ", ", 
      #              round(sg.effects$`Upper 95% CI`[i],2), "]", sep = "")
      est <- paste0(sprintf("%.2f", sg.effects$Median[i]), 
                    " [", sprintf("%.2f", sg.effects$`Lower 95% CI`[i]), 
                    ", ", sprintf("%.2f", sg.effects$`Upper 95% CI`[i]), "]")
      
      
    }else{
      est <- " "
    }
    lab <- c(lab, est)
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "LOR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:nrow(int.effects)){
    if(!is.na(int.effects$Median[i])){
      # est <- paste(round(int.effects$Median[i],2), " [", round(int.effects$`Lower 95% CI`[i],2), ", ", 
      #              round(int.effects$`Upper 95% CI`[i],2), "]", sep = "")
      est <- paste0(sprintf("%.2f", int.effects$Median[i]), 
                    " [", sprintf("%.2f", int.effects$`Lower 95% CI`[i]), 
                    ", ", sprintf("%.2f", int.effects$`Upper 95% CI`[i]), "]")
      
    }else{
      est <- " "
    }
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", nrow(int.effects)),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1)
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
}



plot_comb_age_all_OR <- function(sg.effects, int.effects, alim, left_int_lab, right_int_lab){
  
  #p <- nrow(int.effects)
  l <- nrow(sg.effects)
  
  ## vertical coordinates-----------------------------
  ytop <- l+2 
  top_txt <- l+1 
  
  rows <- c()
  rows2 <- c()
  for(i in 1:(l/6)){
    val <- (l/6) + 1 - i
    
    g1 <-6*val-1
    g2 <- 6*val-2
    g3 <- 6*val-3
    g4 <- 6*val-4
    g5 <- 6*val-5
    g6 <- 6*val-6
    
    #rows <- append(rows, l:h)
    rows <- append(rows, g1:g6)
    rows2 <- append(rows2, g2:g6)
    
    sg.effects$Covariate[6*i]<-""
    sg.effects$Covariate[6*i-1]<-""
    sg.effects$Covariate[6*i-2]<-""
    sg.effects$Covariate[6*i-3]<-""
    sg.effects$Covariate[6*i-4]<-""
    
  }
  
  ind <- which(sg.effects$Covariate=="Education and Reminder")
  sg.effects$Covariate[ind] <- "Education and"
  
  
  shade <- c()
  for(i in 1:(l/6)){
    if(i %% 2 != 0){
      g1 <- rows[6*i -5]
      g3 <- rows[6*i]
      shade <- append(shade, g1:g3)
    }
  }
  
  #alim is a 2-vector - same for both plots
  arange <- alim[2]-alim[1]
  
  ## PAIRS coordinates--------------------------------
  #x-limits
  xrange <- 3.75*arange
  
  left_space <- 0.55*xrange #space to the left of axis
  right_space <- 0.25*xrange #space to the right of axis
  
  xlow <- alim[1] - left_space #left coordinate of plot
  xhigh <- alim[2] + right_space #right coordinate of plot
  
  #covariate spacing
  cov_space1 <- xrange*0.2 #space from study to LHS of SG
  cov_space2 <- xrange*0.2 #space from SG to LHS of N
  est_space <- xrange*0.2 #space from axis to RHS of estimate
  
  sg_left <- xlow + cov_space1 #coordinate of SG (left)
  n_left <- sg_left + cov_space2 #coordinate of N (left)
  est_right <- alim[2] + est_space #coordinate of Estimate (right)
  
  ## INT coordinates--------------------------------
  xrange2 <- 1.875*arange
  
  left_space2 <- xrange2*1/15 #space to the left of axis
  right_space2 <- xrange2*0.4 #space to the right of axis
  
  xlow2 <- alim[1] - left_space2 #left coordinate of int plot
  xhigh2 <- alim[2] + right_space2 #right coordinate of int plot
  
  #axis labels
  lab_space <- 0.125*arange
  left_lab_x <- alim[1]-lab_space
  right_lab_x <- alim[2]+lab_space
  
  ## PLOT-------------------------------------------
  par(mfrow=c(1,2))
  par(mar=c(5,0,1,0)) #c(bottom, left, top, right)
  options(na.action = "na.pass")
  ## PAIRS PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(0, 2/3, 0, 1))
  forest(sg.effects$Median,
         ci.lb = sg.effects$`Lower 95% CI`,
         ci.ub = sg.effects$`Upper 95% CI`,
         slab = rep("",l),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow, xhigh),
         ylim = c(0,ytop),
         col = "red3",
         annotate = FALSE,
         rows = rows,
         xlab = "",
         ilab = cbind(sg.effects$Covariate, sg.effects$Group, sg.effects$Num),
         ilab.xpos = c(xlow, sg_left, n_left),
         ilab.pos = c(4,4,4),
         textpos = c(alim[2], est_right),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  text(xlow, 16.25, "Reminder", pos=4)
  
  #add estimates
  #create vector of annotation
  lab <- c()
  for(i in 1:l){
    if(!is.na(sg.effects$Median[i])){

      # est <- paste(round(exp(sg.effects$Median[i]),2), " [", round(exp(sg.effects$`Lower 95% CI`[i]),2), ", ", 
      #              round(exp(sg.effects$`Upper 95% CI`[i]),2), "]", sep = "")
      est <- paste0(sprintf("%.2f", exp(sg.effects$Median[i])), 
                    " [", sprintf("%.2f", exp(sg.effects$`Lower 95% CI`[i])), 
                    ", ", sprintf("%.2f", exp(sg.effects$`Upper 95% CI`[i])), "]")
      
      
    }else{
      est <- " "
    }
    lab <- c(lab, est)
  }
  text(est_right, rows, pos = 2, lab)
  text(est_right, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  
  #add text
  text(xlow, top_txt, "Intervention", pos=4, font=2)
  text(sg_left, top_txt, "Subgroup", pos=4, font=2)
  text(n_left, top_txt, "No. trials", pos=4, font=2)
  text(n_left, top_txt-0.5, "(participants)", pos=4, font=2)
  text(0, top_txt, "OR", font=2)
  
  
  mtext("Favours \ncontrol", side=1, line = 3, at = alim[1])
  mtext("Favours \nintervention", side=1, line = 3, at = alim[2])
  
  #segments(0, -5, 0, ytop-2, lwd=1)
  
  ## INT PLOT ~~~~~~~~~~~~~~~~~
  par(fig=c(2/3, 1, 0, 1))
  par(new=T)
  # forest(ints.res,
  #        slab = rep("", nrow(ints)),
  #        alim = alim,
  #        #at = ax_labs,
  #        xlim = c(xlow2, xhigh2),
  #        ylim = c(0,ytop),
  #        colout = "red3",
  #        border = "red3",
  #        col = "red3",
  #        pch = 19, #circles
  #        annotate = FALSE,
  #        rows = rows2,
  #        addfit = FALSE,
  #        xlab = "",
  #        textpos = c(alim[2], xhigh2),
  #        header = "",
  #        shade = shade,
  #        efac = c(0,vpoly,1)
  # )
  #create vector of annotation
  
  lab <- c()
  for(i in 1:nrow(int.effects)){
    if(!is.na(int.effects$Median[i])){
      # est <- paste(round(exp(int.effects$Median[i]),2), " [", round(exp(int.effects$`Lower 95% CI`[i]),2), ", ", 
      #              round(exp(int.effects$`Upper 95% CI`[i]),2), "]", sep = "")
      est <- paste0(sprintf("%.2f", exp(int.effects$Median[i])), 
                    " [", sprintf("%.2f", exp(int.effects$`Lower 95% CI`[i])), 
                    ", ", sprintf("%.2f", exp(int.effects$`Upper 95% CI`[i])), "]")
    }else{
      est <- " "
    }
    lab <- c(lab, est)
  }
  forest(int.effects$Median,
         ci.lb = int.effects$`Lower 95% CI`,
         ci.ub = int.effects$`Upper 95% CI`,
         slab = rep("", nrow(int.effects)),
         alim = alim,
         #at = ax_labs,
         xlim = c(xlow2, xhigh2),
         ylim = c(0,ytop),
         col = "red3",
         pch = 19, #circles
         annotate = FALSE,
         rows = rows2,
         #addfit = FALSE,
         xlab = "",
         textpos = c(alim[2], xhigh2),
         header = "",
         shade = shade,
         efac = c(0,1),
         atransf = exp
  )
  #add estimates
  text(xhigh2, rows2, pos = 2, lab)
  text(xhigh2, top_txt, pos = 2, "Estimate [95% CrI]", font=2)
  
  #add text
  text(0, top_txt, "Interaction", font=2)
  
  #abline(h = 2, lty = 2)
  
  mtext(left_int_lab, side=1, line = 3, at = left_lab_x) #(SG2)
  mtext(right_int_lab, side=1, line = 3, at = right_lab_x) #(SG1)
  
  
}
