create_a_tot <- function(dat){
  df <- dat[!duplicated(dat$StudyID), ]
  a <- df$NoArmsAnalysed
  a
}

create_armlevel_vec <- function(data_cont, cont.ind, covs, a, N, A){
  n <- length(covs)

  x <- array(dim = c(N, A-1, n))
  
  if(n>0){
    ind<-0
    for(i in 1:N){ #loop over study
      for(k in 1:(a[i]-1)){#loop over contrasts
        for(j in 1:n){#loop over covariates
          cind <- which(colnames(data_cont)==covs[j]) #index of column
          #df[[row, column]]
          x[i, k, j] <- data_cont[[cont.ind[i] + (k-1), cind]]
        }
      }
    }
  }

  x
}

create_studylevel_vec <- function(data_cont, cont.ind, N, covs_z){
  #N is number of studies
  #covs_z is a vector of covariate names (must match colnames)
  
  p <- length(covs_z)
  z <- array(dim = c(N, p)) 
  
  for(i in 1:N){ #loop over study
    for(j in 1:p){#loop over covariates
      cind <- which(colnames(data_cont)==covs_z[j]) #index of column
      #df[[row, column]]
      z[i, j] <- data_cont[[cont.ind[i], cind]]
    }
  }
  
  z
}

