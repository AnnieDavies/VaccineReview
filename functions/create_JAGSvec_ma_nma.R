create_a_ma <- function(nma_dat){
  df <- nma_dat[!duplicated(nma_dat$StudyID), ]
  a <- df$NoArmsNMA
  a
}

#index where new study begins in arm-level vectors
create_armind <- function(a, N){
  arm.ind <- c()
  sum_ind <- 1
  for(i in 1:N){
    arm.ind <- append(arm.ind, sum_ind)
    sum_ind <- sum_ind + a[i]
  }
  arm.ind
}

#index where new study begins in contrast-level vectors
create_contind <- function(a, N){
  cont.ind <- c()
  sum_ind <- 1
  for(i in 1:N){
    cont.ind <- append(cont.ind, sum_ind)
    sum_ind <- sum_ind + (a[i]-1)
  }
  cont.ind
}


create_S <- function(a, N){
  dim <- a-1
  max_dim <- max(dim)
  S <- array(dim = c(N, max_dim, max_dim)) #Sigma = tau^2*S (S=(1,0.5...\\0.5,1,...\\...))
  
  for(i in 1:N){
    for(j in 1:dim[i]){
      for(k in 1:dim[i]){
        if(j==k){
          S[i,j,k] <- 1
        }else{
          S[i,j,k] <- 0.5
        }
      }
    }
  }
  
  S
}