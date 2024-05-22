# creating = function(base_name, num){
#   return(paste(base_name, num, sep='_'))
# }
# 
# for (num in 1:15){
#   name = creating('vector', num)
#   assign(name, rnorm(100,0,1))
# }

x = sapply(1:15, function(t) rnorm(100, 0, 1))

error = rnorm(100, 0, sqrt(0.5))

n = 100

y = 2*x[,1] + x[,5] + 1.5*x[,10] + error
# y = 2*vector_1 + vector_5 + 1.5*vector_10 + error

Q = NULL

# for (k in 1:15){
#   Q[k] = 0
#   Q[k+15] = runif(1)
# }

for (k in 1:16){
  Q[k] = 0
  Q[k+16] = runif(1)
}

update_b_0 = function(n, y, x, b){
  old_b_0 = b[1]
  mean_b_0 = 0
  additional_term = 0
  for(i in 1:n){
    for(j in 1:15){
      mean_b_0 = mean_b_0 + (y[i] + x[i,j] * b[j+1])
    }
  }
  mean_b_0 = mean_b_0 / n
  return(list(old_b_0, mean_b_0))
}

update_b_j = function(j, n, y, x, sigma, b_0, b){
  old_b_j = b
  new_mean_b = NULL
  for(jj in 1:j){
    left_term = 0
    for(i in 1:n){
      left_term = left_term + x[i,jj]^2 + (sigma[1]^2 / sigma[j]^2)
    }
    left_term = 1/left_term
    right_term = 0
    for(i in 1:n){
      for(k in 1:15){
        if(k != j){
          right_term = right_term + x[i,jj] * (y[i] - b_0[[1]] - x[i,k] * b[k])
        }
      }
    }
    new_mean_b[jj] = left_term * right_term
    # print(new_mean_b[jj])
  }
  return(list(old_b_j, new_mean_b))
}

update_variance = function(n, sigma){
  old_variance = sigma
  variance_sq = (1/n) * sigma^2
  return(list(old_variance,variance_sq))
}

update_variance_j = function(j, n, x, sigma){
  old_variance_j = sigma
  variance_j_new = NULL
  variance_j = 0
  for(jj in 1:j){
    for(i in 1:n){
      variance_j = variance_j + x[i,jj]^2 + (sigma[1]^2 / sigma[jj]^2)
    }
    variance_j_new[jj] = (1/variance_j) * sigma[1]^2
  }
  return(list(old_variance_j, variance_j_new))
}

update_sigma_0 = function(y, old_b, x, Q){
  old_sigma_0 = Q[17:32]
  for(j in 1:15){
    right_side = 0
    for(i in 1:100){
      for(j in 1:15){
        right_side = right_side + (y[i] - Q[[1]] - x[i,j] * Q[j+1])^2
      }
    }
    left_side = 1/(rchisq(1,100)^2)
    sigma_0 = left_side*right_side
  }
  return(list(old_sigma_0, sigma_0))
}

update_sigma_j = function(Q, sigma, old_b){
  sigma_j_old = Q[17:32]
  new_sigma = NULL
  for(j in 1:16){
    new_sigma[j] = Q[[j+1]]^2 / (rchisq(1, 1)^2)
  }
  return(list(sigma_j_old, new_sigma))
}

# 
# list_b_0 = update_b_0(100, y, x, Q[1:15])
# old_b_0 = list_b_0[1]
# b_0 = list_b_0[2]
# variance_0 = update_variance(n, Q[16])
# 
# 
# 
# b_j = update_b_j(1, 100, y, x, Q[16:30], b_0, Q[1:15])
# variance_j = update_variance_j(1, 100, x, Q[16:30])
# 
# iteration_b = function(j, y, x, Q, b_0){
#   old_b = Q[1:15]
#   for (j in 1:15){
#     new_mean = update_b_j(j, 100, y, x, Q[16:39], b_0, Q[1:15])
#     new_variance = update_variance_j(j, 100, x, Q[16:30])
#     Q[j] = rnorm(1, new_mean, new_variance)
#   }
#   return(old_b, Q)
# }


for(k in 1:10){
  # print(Q)
  list_update_b_0 = update_b_0(100, y, x, Q[1:16])
  old_b_0 = list_update_b_0[1]
  mean_b_0 = list_update_b_0[2]
  list_update_variance_0 = update_variance(100, Q[17])
  old_variance = list_update_variance_0[1]
  variance_sq = list_update_variance_0[2]
  b_0 = rnorm(1,mean_b_0[[1]], variance_sq[[1]])
  # print('here')
  list_update_b_j = update_b_j(15, 100, y, x, Q[17:32], b_0, Q[1:16])
  # print('here also')
  old_b_j = list_update_b_j[1]
  mean_b_j = list_update_b_j[2]
  list_update_variance_j = update_variance_j(15, 100, x, Q[17:32])
  old_variance_j = list_update_variance_j[1]
  variance_j = list_update_variance_j[2]
  for(i in 1:16){
    if(i==1){
      Q[i] = b_0
    }
    else{
      Q[i] = rnorm(1, mean_b_j[[1]][i-1], variance_j[[1]][i-1])
    }
  }
  # Q[1:16] = rnorm(16, mean_b_j[[1]], variance_sq[[1]])
  list_update_sigma_0 = update_sigma_0(y, b_0, x, Q)
  old_sigma_0 = list_update_sigma_0[1]
  sigma_0 = list_update_sigma_0[2]
  list_update_sigma_j = update_sigma_j(Q, Q[17:32], Q[1:16])
  sigma_j_old = list_update_sigma_j[1]
  sigma_j = list_update_sigma_j[2]
  for(i in 1:16){
    if(i==1){
      Q[i+16] = sigma_0[[1]]
    }
    else{
      Q[i+16] = sigma_j[[1]][i]
    }
  }
  # Q = sigma_j
}



