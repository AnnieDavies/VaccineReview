library(tidyverse)
library(rjags)
library(mcmcplots)
library(readxl)
library(writexl)
library(MCMCvis)
library(metafor)

## 0.0 Read in functions & data & set wd
list.files("functions", full.names = TRUE) %>% map(source)

load("MA_NMA_DATA.RData")

path_RE <- "RandomEffects\\"
path_FE <- "FixedEffects\\"


################################# NMA ##########################################

###########
# RE model
###########

## DATA
dat <- list("r" = r.adj, "n" = n.adj, "a" = a, "S" = S, "t" = t, "N"=N, "M"=M,
            "aind" = arm.ind, "cind" = cont.ind)

## RUN 
t0 <- Sys.time()
jags.m <- jags.model(file = "Models/model_nma_re.txt", data=dat, n.chains = 4, n.adapt = 10000) #3.45 mins
t1 <- Sys.time()
print(t1-t0)
params <- c("d[2:7]", "tau", "rhat", "dev", "totdev") 
## run JAGS and save posterior samples
samps <- coda.samples( jags.m, params, n.iter=20000)
t2 <- Sys.time()
print(t2-t1)
save.image(file = paste0(path_RE,"MAIN_RE_Results.RData"))


## RESULTS 
samp_sum.re <- summary(window(samps, start=20000))
re.samp.sum <- data.frame(samp_sum.re$quantiles)#nb: params stored in alphabetical order

# label rows
rhat_labs <- c()
dev_labs <- c()
for(i in 1:N){
  for(k in 1:a[i]){
    rlab <- paste("rhat_i",i,"_k",k,sep = "")
    dlab <- paste("dev_i",i,"_k",k,sep = "")
    rhat_labs <- c(rhat_labs, rlab)
    dev_labs <- c(dev_labs, dlab)
  }
}
rownames(re.samp.sum) <- c(trts[2:7], dev_labs, rhat_labs, "tau", "Dres")

num.d <- length(trts[2:7])
num.dev <- length(r.adj)

# Deviance parameters
dev_params <- calc_devs_re(re.samp.sum, m = length(trts[2:7]), r.adj, n.adj)
re.dev.vec <- dev_params[[1]]
re.pD <- dev_params[[2]]
re.DIC <- dev_params[[3]]
re.pD
re.DIC
write.csv(re.dev.vec, paste(path_RE,"MAIN_RE_deviances.csv",sep=""))

# Parameter estimates
re.effects <- re.samp.sum[c(1:num.d, num.d+2*num.dev+1), ]
re.effects <- re.effects[, c("X50.", "X2.5.", "X97.5.")]
colnames(re.effects) <- c("Median", "Lower 95% CI", "Upper 95% CI")

#get number of trials/participants
re.effects$NumTrials <- NA
re.effects$NumParticipants <- NA
re.effects$Num<-NA
for(i in 1:nrow(re.effects)){
  dati <- subset(ma_dat, IntCategory==rownames(re.effects)[i])
  re.effects$NumTrials[i]<-nrow(dati)
  re.effects$NumParticipants[i]<-round(sum(dati$n))
  re.effects$Num[i]<-paste0(re.effects$NumTrials[i], " (", re.effects$NumParticipants[i], ")")
}


write.csv(re.effects, paste(path_RE,"MAIN_RE_Parameter_Estimates.csv",sep=""))

# Trace Plots
comps <- c(paste(trts[2:7], "vs", trts[1]), "Heterogeneity")
params <- varnames(samps)[c(1:num.d, num.d+2*num.dev+1)]
for (i in 1:length(comps)) {
  png(paste0(path_RE, "TracePlots//", comps[i], ".png"), width = 25, height = 12, units = "cm", res = 300)
  MCMCtrace(samps, 
            pdf = FALSE, 
            params = params[i], 
            main_den = c(comps[i]), 
            main_tr = c(comps[i]), 
            iter = 20000, 
            ISB = FALSE)
  dev.off()
}

# Convergence plots
for (i in 1:length(comps)) {
  png(paste0(path_RE, "ConvergencePlots//", comps[i], ".png"), width = 13, height = 13, units = "cm", res = 300)
  gelman.plot(samps[,params[i]], main = comps[i])
  dev.off()
}

# Forest plots
df <- re.effects[1:num.d, ]

# LOR scale
x_left <- -1.7
x_right <- 2.0
xnum <- -0.9
top_txt <- nrow(df)+2
alim<-c(-0.35, 1.4)
png(paste0(path_RE, "MAIN_RE_ForestPlot_LOR", ".png"), width = 25, height = 12, units = "cm", res = 300)
forest(df$Median, 
       ci.lb = df$`Lower 95% CI`, 
       ci.ub = df$`Upper 95% CI`, 
       xlim=c(x_left, x_right), 
       alim=alim,
       at=c(-0.35, 0, 0.35, 0.7, 1.05, 1.4),
       slab = rownames(df),
       main = "",
       ilab = cbind(df$Num),
       ilab.xpos = c(xnum),
       ilab.pos = c(4),
       header=FALSE,
       xlab = "LOR",
       efac = c(0,1),
       digits=3
)
text(xnum, top_txt, "No. trials", pos=4, font=2)
text(xnum, top_txt-0.5, "(participants)", pos=4, font=2)
text(x_left, top_txt, "Intervention", pos=4, font=2)
text(x_left, top_txt-0.5, "(vs Control)", pos=4, font=2)
text(x_right, top_txt, "LOR [95% CrI]", pos=2, font=2)

mtext("Favours control", side=1, line = 3.5, at = alim[1])
mtext("Favours intervention", side=1, line = 3.5, at = alim[2])

dev.off()

# OR scale
x_left <- -1.7
x_right <- 2.0
xnum <- -0.9
top_txt <- nrow(df)+2
alim<-c(-0.35, 1.4)
png(paste0(path_RE, "MAIN_RE_ForestPlot_OR", ".png"), width = 25, height = 12, units = "cm", res = 300)
forest(df$Median, 
       ci.lb = df$`Lower 95% CI`, 
       ci.ub = df$`Upper 95% CI`, 
       xlim=c(x_left, x_right), 
       alim=alim,
       at=c(-0.35, 0, 0.35, 0.7, 1.05, 1.4),
       slab = rownames(df),
       main = "",
       ilab = cbind(df$Num),
       ilab.xpos = c(xnum),
       ilab.pos = c(4),
       header=FALSE,
       xlab = "OR",
       efac = c(0,1),
       digits=3,
       atransf = exp
)
text(xnum, top_txt, "No. trials", pos=4, font=2)
text(xnum, top_txt-0.5, "(participants)", pos=4, font=2)
text(x_left, top_txt, "Intervention", pos=4, font=2)
text(x_left, top_txt-0.5, "(vs Control)", pos=4, font=2)
text(x_right, top_txt, "OR [95% CrI]", pos=2, font=2)

mtext("Favours control", side=1, line = 3.5, at = alim[1])
mtext("Favours intervention", side=1, line = 3.5, at = alim[2])

dev.off()


###########
# FE model
###########

## DATA
dat.fe <- list("r" = r.adj, "n" = n.adj, "a" = a, "t" = t, "N"=N, "M"=M,
            "aind" = arm.ind, "cind" = cont.ind)

## RUN 
t0 <- Sys.time()
jags.fe <- jags.model(file = "Models/model_nma_fe.txt", data=dat.fe, n.chains = 4, n.adapt = 10000)
t1 <- Sys.time()
print(t1-t0)
params <- c("d[2:7]", "rhat", "dev", "totdev") 
## run JAGS and save posterior samples
samps.fe <- coda.samples(jags.fe, params, n.iter=20000)#1.1 mins
t2 <- Sys.time()
print(t2-t1)
save.image(file = paste0(path_FE, "MAIN_FE_RE_Results.RData"))

## RESULTS
samp_sum.fe <- summary(window(samps.fe, start=20000))
fe.samp.sum <- data.frame(samp_sum.fe$quantiles)#nb: params stored in alphabetical order

# label rows
rownames(fe.samp.sum) <- c(trts[2:7], dev_labs, rhat_labs, "Dres")

# Deviance parameters
fe.dev_params <- calc_devs_fe(fe.samp.sum, m = length(trts[2:7]), r.adj, n.adj)
fe.dev.vec <- fe.dev_params[[1]]
fe.pD <- fe.dev_params[[2]]
fe.DIC <- fe.dev_params[[3]]
fe.pD
fe.DIC
write.csv(fe.dev.vec, paste0(path_FE,"MAIN_FE_deviances.csv"))

# Parameter estimates
fe.effects <- fe.samp.sum[c(1:num.d, num.d+2*num.dev+1), ]
fe.effects <- fe.effects[, c("X50.", "X2.5.", "X97.5.")]
colnames(fe.effects) <- c("Median", "Lower 95% CI", "Upper 95% CI")

#get number of trials/participants
fe.effects$NumTrials <- NA
fe.effects$NumParticipants <- NA
fe.effects$Num<-NA
for(i in 1:nrow(re.effects)){
  dati <- subset(ma_dat, IntCategory==rownames(fe.effects)[i])
  fe.effects$NumTrials[i]<-nrow(dati)
  fe.effects$NumParticipants[i]<-round(sum(dati$n))
  fe.effects$Num[i]<-paste0(fe.effects$NumTrials[i], " (", fe.effects$NumParticipants[i], ")")
}

write.csv(fe.effects, paste(path_FE,"MAIN_FE_Parameter_Estimates.csv",sep=""))

# Trace Plots
comps <- c(paste(trts[2:7], "vs", trts[1]))
params <- varnames(samps.fe)[c(1:num.d)]
for (i in 1:length(comps)) {
  png(paste0(path_FE, "TracePlots//", comps[i], ".png"), width = 25, height = 12, units = "cm", res = 300)
  MCMCtrace(samps.fe, 
            pdf = FALSE, 
            params = params[i], 
            main_den = c(comps[i]), 
            main_tr = c(comps[i]), 
            iter = 20000, 
            ISB = FALSE)
  dev.off()
}

#Convergence plots
for (i in 1:length(comps)) {
  png(paste0(path_FE, "ConvergencePlots//", comps[i], ".png"), width = 13, height = 13, units = "cm", res = 300)
  gelman.plot(samps.fe[,params[i]], main = comps[i])
  dev.off()
}

# Forest Plots
df.fe <- fe.effects[1:num.d, ]

# LOR scale
x_left <- -0.65
x_right <- 0.75
xnum <- -0.325
top_txt <- nrow(df.fe)+2
alim<-c(-0.125, 0.5)
png(paste0(path_FE, "MAIN_FE_ForestPlot_LOR", ".png"), width = 25, height = 12, units = "cm", res = 300)
forest(df.fe$Median, 
       ci.lb = df.fe$`Lower 95% CI`, 
       ci.ub = df.fe$`Upper 95% CI`, 
       xlim=c(x_left, x_right), 
       alim=alim,
       at=c(-0.125, 0, 0.125, 0.25, 0.375, 0.5),
       slab = rownames(df.fe),
       main = "",
       ilab = cbind(df.fe$Num),
       ilab.xpos = c(xnum),
       ilab.pos = c(4),
       header=FALSE,
       xlab = "LOR",
       efac = c(0,1),
       digits=3
)
text(xnum, top_txt, "No. trials", pos=4, font=2)
text(xnum, top_txt-0.5, "(participants)", pos=4, font=2)
text(x_left, top_txt, "Intervention", pos=4, font=2)
text(x_left, top_txt-0.5, "(vs Control)", pos=4, font=2)
text(x_right, top_txt, "LOR [95% CrI]", pos=2, font=2)

mtext("Favours control", side=1, line = 3.5, at = alim[1])
mtext("Favours intervention", side=1, line = 3.5, at = alim[2])

dev.off()

# OR scale
x_left <- -0.65
x_right <- 0.75
xnum <- -0.325
top_txt <- nrow(df.fe)+2
alim<-c(-0.125, 0.5)
png(paste0(path_FE, "MAIN_FE_ForestPlot_OR", ".png"), width = 25, height = 12, units = "cm", res = 300)
forest(df.fe$Median, 
       ci.lb = df.fe$`Lower 95% CI`, 
       ci.ub = df.fe$`Upper 95% CI`, 
       xlim=c(x_left, x_right), 
       alim=alim,
       at=c(-0.125, 0, 0.125, 0.25, 0.375, 0.5),
       slab = rownames(df.fe),
       main = "",
       ilab = cbind(df.fe$Num),
       ilab.xpos = c(xnum),
       ilab.pos = c(4),
       header=FALSE,
       xlab = "OR",
       efac = c(0,1),
       #cex = 1.1,
       digits=3,
       atransf = exp
)
text(xnum, top_txt, "No. trials", pos=4, font=2)
text(xnum, top_txt-0.5, "(participants)", pos=4, font=2)
text(x_left, top_txt, "Intervention", pos=4, font=2)
text(x_left, top_txt-0.5, "(vs Control)", pos=4, font=2)
text(x_right, top_txt, "OR [95% CrI]", pos=2, font=2)

mtext("Favours control", side=1, line = 3.5, at = alim[1])
mtext("Favours intervention", side=1, line = 3.5, at = alim[2])

dev.off()

## Network plot-------------------------------------------------------------
library(multinma)
options(mc.cores = parallel::detectCores())

ma_dat$r_adj_int <- as.integer(ma_dat$r_adj)
ma_dat$n_adj_int <- as.integer(ma_dat$n_adj)
vacc_net <- set_agd_arm(ma_dat, 
                           study = StudyID,
                           trt = IntCategory,
                           r = r_adj_int, 
                           n = n_adj_int)

#Network plot
png("NetworkPlot.png", width = 25, height = 15, units = "cm", res = 300)
plot(vacc_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.05)+
  ggplot2::theme(legend.position = "bottom",
                 legend.box = "vertical",
                 legend.margin = ggplot2::margin(0, 0, 0, 0),
                 legend.spacing = ggplot2::unit(0.5, "lines"))
dev.off()


############# MA ###############################################################

ma_path <- "\\Pairwise\\"

t0<-Sys.time()
sheet_names <- list()
MA_results <- list()
ind<-1
for(i in 1:(M-1)){
  for(j in (i+1):M){
    
    t1 <- i
    t2 <- j
    print(paste(trts[t2], "vs", trts[t1]))
    
    ## DATA
    dat.ma <- get_MA_dat(ma_dat, studies, t1, t2)
    r1 <- dat.ma[[1]]
    n1 <- dat.ma[[2]]
    r2 <- dat.ma[[3]]
    n2 <- dat.ma[[4]]
    IDS <- dat.ma[[5]]
    names <- dat.ma[[6]]
    
    N12 <- length(IDS)
    
    if(N12>0){
      name1 <- trts[t1]
      name2 <- trts[t2]
      if(trts[t1]=="Education and Reminder"){
        name1 <- "Educ & Remind"
      }else if(trts[t2]=="Education and Reminder"){
        name2 <- "Educ & Remind"
      }
      sheet_names <- append(sheet_names, paste(name2, "vs", name1))
      
      #make rs and ns integer
      r1 <- round(r1)
      n1 <- round(n1)
      r2 <- round(r2)
      n2 <- round(n2)
      
      height <- (6+N12^(1/1.15))
      width <- 30
      
      dat <- list("r1" = r1, "n1" = n1, "r2" = r2, "n2" = n2, "N" = N12)
      
      #Random effects
      jags.re <- jags.model(file = "Models/model_ma_re.txt", data=dat, n.chains = 4, n.adapt = 10000, quiet = TRUE) 
      params <- c("d", "tau", "delta", "rhat1", "rhat2", "dev1", "dev2", "trialdev", "totdev")
      samps.re <- coda.samples(jags.re, params, n.iter=20000, quiet = TRUE)
      
      samp_sum_re <- summary(window(samps.re, start=15000))
      df.re <- data.frame(samp_sum_re$quantiles)
      
      tau.med <- df.re[c("tau"),]$X50.
      tau.lci <- df.re[c("tau"),]$X2.5.
      tau.uci <- df.re[c("tau"),]$X97.5.
      
      #Fixed effects
      jags.fe <- jags.model(file = "Models/model_ma_fe.txt", data=dat, n.chains = 4, n.adapt = 10000, quiet = TRUE) 
      params <- c("d", "rhat1", "rhat2", "dev1", "dev2", "trialdev", "totdev")
      samps.fe <- coda.samples(jags.fe, params, n.iter=20000, quiet = TRUE)
      
      samp_sum_fe <- summary(window(samps.fe, start=15000))
      df.fe <- data.frame(samp_sum_fe$quantiles)
      
      #Save results
      df.res <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
      colnames(df.res)<-c("Model","Parameter", "Median", "LowerCI", "UpperCI")
      df.res$Model<-c("FE", "RE", " ", "Studies")
      df.res$Parameter <- c("d", "d", "tau", "k")
       
      df.res$Median[1] <- df.fe[c("d"),]$X50.
      df.res$LowerCI[1] <- df.fe[c("d"),]$X2.5.
      df.res$UpperCI[1] <- df.fe[c("d"),]$X97.5.
       
      df.res$Median[2] <- df.re[c("d"),]$X50.
      df.res$LowerCI[2] <- df.re[c("d"),]$X2.5.
      df.res$UpperCI[2] <- df.re[c("d"),]$X97.5.
      
      df.res$Median[3] <- tau.med
      df.res$LowerCI[3] <- tau.lci
      df.res$UpperCI[3] <- tau.uci
      
      df.res$Median[4] <- N12
      
      MA_results[[ind]] <- df.res
      ind<-ind+1
      
      #Plot Results
      study_TEs <- get_TE(r1, n1, r2, n2) 
      
      name.lor <- paste0(ma_path, "LOR\\Forest_MA_LOR_", trts[[t2]], "_vs_", trts[[t1]], ".tiff")
      name.or <- paste0(ma_path, "OR\\Forest_MA_OR_", trts[[t2]], "_vs_", trts[[t1]], ".tiff")
      if(N12>1){
        #get alim from study estimates and RE estimate
        xmax <- max(study_TEs[[3]], df.re[c("d"),]$X97.5.)
        xmin <- min(study_TEs[[2]], df.re[c("d"),]$X2.5.)
        xrange <- xmax - xmin
        
        # Define a suitable step for the x-axis ticks
        step <- if (xrange <= 1) {
          0.2
        } else if (xrange <= 3) {
          0.5
        } else if (xrange <= 6) {
          1
        } else {
          2
        }
        
        # Define the axis limits
        alim <- c(floor(xmin/step) * step, ceiling(xmax/step) * step)
        
        # Define the axis ticks
        xticks <- seq(from = alim[1], to = alim[2], by = step)
        
        # plot width
        prange <- 2*xrange
        left.plot <- alim[1]-0.3*prange
        right.plot <- alim[2]+0.2*prange
        
        ##LOR
        tiff(name.lor, height = height, width = width, 
             units = 'cm', compression = "lzw", res = 150)
        forest(study_TEs[[1]], 
               ci.lb = study_TEs[[2]], 
               ci.ub = study_TEs[[3]],
               slab = names,
               order = rev(IDS),
               rows = c(4:(3+N12)),
               #header = TRUE,
               header = FALSE,
               xlab = "Log odds ratio",
               xlim = c(left.plot, right.plot),
               alim = alim,  # Set x-axis limits
               at = xticks,  # Set x-axis tick positions
               efac = c(0,1),
               main = paste(trts[t2], "vs", trts[t1])
        )
        #RE estimate
        addpoly(x = df.re[c("d"),]$X50.,
                ci.lb = df.re[c("d"),]$X2.5.,
                ci.ub = df.re[c("d"),]$X97.5.,
                row = 2.5,
                mlab = expression(bold("RE estimate"))
        )
        #FE estimate
        addpoly(x = df.fe[c("d"),]$X50.,
                ci.lb = df.fe[c("d"),]$X2.5.,
                ci.ub = df.fe[c("d"),]$X97.5.,
                row = 1.5,
                mlab = expression(bold("FE estimate"))
        )
        
        top_txt <- N12+5
        text(left.plot, top_txt, "Study", pos=4, font=2, adj=1)
        text(right.plot, top_txt, "LOR [95% CrI]", pos=2, font=2, adj=1)
        
        ## add tau estimate to plot------------------
        # Add text to the far left of row 1 and 2
        tau.text <- bquote(tau == .(round(tau.med, 2)) ~ "[" ~ .(round(tau.lci, 2)) ~ "," ~ .(round(tau.uci, 2)) ~ "]")
        tau2.text <- bquote(tau^2 == .(round(tau.med^2, 2)))
        mtext(tau.text, side = 1, line = 1, at = left.plot, adj=0)
        mtext(tau2.text, side = 1, line = 2, at = left.plot, adj=0)
        
        mtext("Favours control", side=1, line = 3.5, at = alim[1])
        mtext("Favours intervention", side=1, line = 3.5, at = alim[2])
        dev.off()
        
        ##OR
        tiff(name.or, height = height, width = width, 
             units = 'cm', compression = "lzw", res = 150)
        forest(study_TEs[[1]], 
               ci.lb = study_TEs[[2]], 
               ci.ub = study_TEs[[3]],
               slab = names,
               order = rev(IDS),
               rows = c(4:(3+N12)),
               header = FALSE,
               xlab = "Odds ratio",
               xlim = c(left.plot, right.plot),
               alim = alim,  # Set x-axis limits
               at = xticks,  # Set x-axis tick positions
               efac = c(0,1),
               main = paste(trts[t2], "vs", trts[t1]),
               atransf = exp
        )
        #RE estimate
        addpoly(x = df.re[c("d"),]$X50.,
                ci.lb = df.re[c("d"),]$X2.5.,
                ci.ub = df.re[c("d"),]$X97.5.,
                row = 2.5,
                atransf = exp,
                mlab = expression(bold("RE estimate"))
        )
        #FE estimate
        addpoly(x = df.fe[c("d"),]$X50.,
                ci.lb = df.fe[c("d"),]$X2.5.,
                ci.ub = df.fe[c("d"),]$X97.5.,
                row = 1.5,
                atransf = exp,
                mlab = expression(bold("FE estimate"))
        )
        ## add tau estimate to plot------------------
        # Get the plotting region coordinates
        plot_coords <- par("usr")
        # Extract the left x-coordinate
        left_x <- plot_coords[1]
        right_x <- plot_coords[2]
        
        top_txt <- N12+5
        text(left_x, top_txt, "Study", pos=4, font=2, adj=1)
        text(right_x, top_txt, "OR [95% CrI]", pos=2, font=2, adj=1)
        
        # # Add text to the far left of row 0
        tau.text <- bquote(tau == .(round(tau.med, 2)) ~ "[" ~ .(round(tau.lci, 2)) ~ "," ~ .(round(tau.uci, 2)) ~ "]")
        tau2.text <- bquote(tau^2 == .(round(tau.med^2, 2)))
        mtext(tau.text, side = 1, line = 1, at = left_x, adj=0)
        mtext(tau2.text, side = 1, line = 2, at = left_x, adj=0)
        
        mtext("Favours control", side=1, line = 3.5, at = alim[1])
        mtext("Favours intervention", side=1, line = 3.5, at = alim[2])
        dev.off()
        
      }else{
        
        #get alim from study estimates and RE estimate
        xmax <- max(study_TEs[[3]])
        xmin <- min(study_TEs[[2]])
        xrange <- xmax - xmin
        
        # Define a suitable step for the x-axis ticks
        step <- if (xrange <= 1) {
          0.2
        } else if (xrange <= 3) {
          0.5
        } else if (xrange <= 6) {
          1
        } else {
          2
        }
        
        # Define the axis limits
        alim <- c(floor(xmin/step) * step, ceiling(xmax/step) * step)
        
        # Define the axis ticks
        xticks <- seq(from = alim[1], to = alim[2], by = step)
        
        # plot width
        prange <- 2*xrange
        left.plot <- alim[1]-0.3*prange
        right.plot <- alim[2]+0.2*prange
        
        #LOR
        tiff(name.lor, height = height, width = width, 
             units = 'cm', compression = "lzw", res = 150)
        forest(study_TEs[[1]], 
               ci.lb = study_TEs[[2]], 
               ci.ub = study_TEs[[3]],
               slab = names,
               #order = rev(IDS),
               rows = c(2:(1+N12)),
               header = FALSE,
               xlab = "Log odds ratio",
               xlim = c(left.plot, right.plot),
               alim = alim,  # Set x-axis limits
               at = xticks,  # Set x-axis tick positions
               efac = c(0,1),
               main = paste(trts[t2], "vs", trts[t1])
        )
        
        top_txt <- N12+3
        text(left.plot, top_txt, "Study", pos=4, font=2, adj=1)
        text(right.plot, top_txt, "LOR [95% CrI]", pos=2, font=2, adj=1)
        
        
        mtext("Favours control", side=1, line = 3.5, at = alim[1])
        mtext("Favours intervention", side=1, line = 3.5, at = alim[2])
        dev.off()
        
        #OR
        tiff(name.or, height = height, width = width, 
             units = 'cm', compression = "lzw", res = 150)
        forest(study_TEs[[1]], 
               ci.lb = study_TEs[[2]], 
               ci.ub = study_TEs[[3]],
               slab = names,
               #order = rev(IDS),
               rows = c(2:(1+N12)),
               header = FALSE,
               xlab = "Odds ratio",
               xlim = c(left.plot, right.plot),
               alim = alim,  # Set x-axis limits
               at = xticks,  # Set x-axis tick positions
               efac = c(0,1),
               main = paste(trts[t2], "vs", trts[t1]),
               atransf = exp
        )
        plot_coords <- par("usr")
        left_x <- plot_coords[1]
        right_x <- plot_coords[2]
        
        top_txt <- N12+3
        text(left_x, top_txt, "Study", pos=4, font=2, adj=1)
        text(right_x, top_txt, "OR [95% CrI]", pos=2, font=2, adj=1)
        
        mtext("Favours control", side=1, line = 3.5, at = alim[1])
        mtext("Favours intervention", side=1, line = 3.5, at = alim[2])
        dev.off()
      }
    }
    
  }
}
names(MA_results)<-sheet_names
write_xlsx(MA_results, path = paste0(ma_path, "Pairwise_results.xlsx"))
t1<-Sys.time()
print(difftime(t1, t0, units = "mins"))

save.image(paste0(ma_path, "Pairwise_Results.RData"))
