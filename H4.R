gibbs_sampler_normal = function(sample_size, X_dist, Y_dist, X_initial, rho){
  for (k in 1:sample_size){
    if (k == 1){
      Y_dist[k] = rnorm(1, rho*X_initial, 1-rho**2)
      X_dist[k] = rnorm(1, rho*Y_dist[k], 1-rho**2)
    } else{
      Y_dist[k] = rnorm(1, rho*X_dist[k-1], 1-rho**2)
      X_dist[k] = rnorm(1, rho*Y_dist[k], 1-rho**2)
    }
  }
  return(list(X_dist, Y_dist))
}

random_MH = function(sample_size, X_dist, Y_dist, X_initial, Y_initial, sd){
  for (k in 1:sample_size){
    if (k == 1){
      X = rnorm(1, X_initial, sd)
      rho_x = min(dnorm(X, mean=0, 0.6) / dnorm(X_initial,mean=0,sd=0.6), 1)
      Y = rnorm(1, Y_initial, sd)
      rho_y = min(dnorm(Y, mean=0, 0.6) / dnorm(Y_initial,mean=0,sd=0.6), 1)
      if (runif(1)<rho_x){
        # acceptance = acceptance + 1
        X_dist[k] = X
      } else {
        X_dist[k] = X_initial
      }
      if (runif(1)<rho_y){
        # acceptance = acceptance + 1
        Y_dist[k] = Y
      } else {
        Y_dist[k] = Y_initial
      }
    } else {
      X = rnorm(1, X_initial, sd)
      rho_x = min(dnorm(X, mean=0, 0.6) / dnorm(X_dist[k-1],mean=0,sd=0.6), 1)
      Y = rnorm(1, Y_initial, sd)
      rho_y = min(dnorm(Y, mean=0, 0.6) / dnorm(Y_dist[k-1],mean=0,sd=0.6), 1)
      if (runif(1)<rho_x){
        # acceptance = acceptance + 1
        X_dist[k] = X
      } else {
        X_dist[k] = X_initial
      }
      if (runif(1)<rho_y){
        # acceptance = acceptance + 1
        Y_dist[k] = Y
      } else {
        Y_dist[k] = Y_initial
      }
    }
  }
  return(list(X_dist, Y_dist))
}


sample_size = 10000
X_values = NULL
X_initial = 0
Y_values = NULL


return_list = gibbs_sampler_normal(sample_size, X_values, Y_values, X_initial, 0.6)
X_dist = return_list[[1]]
Y_dist = return_list[[2]]

X_values = NULL
X_initial = 0
Y_values = NULL
Y_initial = 0
return_list2 = random_MH(sample_size, X_values, Y_values, X_initial, Y_initial, 0.6)
X_dist2 = return_list2[[1]]
Y_dist2 = return_list2[[2]]

plot(density(X_dist), main=NA, col='red')
title(main=('X and Y distributions'))
lines(density(Y_dist), col='blue')
legend(-3,0.3, legend=c('X-jakauma', 'Y-jakauma'), 
       fill=c('red', 'blue'))

plot(density(X_dist2), main=NA, col='red')
title(main=('X and Y distributions'))
lines(density(Y_dist2), col='blue')
legend(-3,0.3, legend=c('X-jakauma', 'Y-jakauma'), 
       fill=c('red', 'blue'))

