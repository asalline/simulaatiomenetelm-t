n = 100
p = 16
X = sapply(1:p, function(x) rnorm(n, mean=0, sd=1))
error = rnorm(n, mean=0, sd=sqrt(0.5))
Y = 2 * X[,1] + X[,5] + 1.5 * X[,10] + error

b = matrix(0, p, n)
sigma = matrix(2, p, n)

N=100
for(k in 2:N){
  # Lasketaan b_0 samplerille keskiarvo
  b_0_mean = 0
  for(i in 1:n){
    for(j in 2:p){
      b_0_mean = b_0_mean + Y[i] - X[i,j] * b[,j]
    }
  }
  b_0_mean = b_0_mean / n
  
  # Lasketaan b_0 samplerille keskihajonta
  s_0 = sqrt((sigma[,1])/n)
  
  # Samplataan uusi b_0
  for(ii in 1:p){
    b[ii,1] = rnorm(1, mean=b_0_mean[ii], sd=s_0[ii])
  }
  
  # Lasketaan b_j samplerille keskiarvot
  b_j_mean = NULL
  left_side = 0
  right_side = 0
  for(j in 1:p){
    for(i in 1:n){
      right_side = right_side + X[i,j]^2 + (sigma[1]^2 / sigma[j]^2)
    }
    right_side = 1 / right_side
    for(i in 1:n){
      for(h in 1:p){
        if(h != j){
          left_side = left_side + X[i,j] * (Y[i] - b[,1] - X[j,h] * b[,h])
        }
      }
    }
    b_j_mean[j] = (right_side * left_side)[j]
  }
  
  # Lasketaan b_j samplerille keskihajonnat
  b_j_sd = NULL
  left_side = 0
  for(j in 1:p){
    for(i in 1:n){
      left_side = left_side + X[i,j]^2 + (sigma[1]^2 / sigma[j]^2)
    }
    b_j_sd[j] = sqrt((1/left_side) * sigma[1]^2)
  }
  
  # Samplataan uusi b_j
  for(ii in 1:n){
    for(jj in 2:p){
      b[jj,ii] = rnorm(1, mean=b_j_mean[jj], sd=b_j_sd[jj])
    } 
  }
  
  # Päivitetään sigmat
  update = 0
  for(i in 1:1){
    for(j in 1:p){
      update = update + (Y[i] - b[,1] - X[i,j] * b[,j])^2
    }
  }
  sigma[,1] = (1/rchisq(p,n)^2) * update
  
  for(j in 2:n){
    sigma[,j] = b[,j]^2 / (rchisq(p, 1)^2)
  }
  
  
}

