gibbs_sampler_normal = function(sample_size, X_dist, Y_dist, a, b, n){
  for (k in 1:sample_size){
    if (k == 1){
      Y_dist[k] = rbeta(1, a, b)
      X_dist[k] = rbinom(1, n, Y_dist[k])
    } else{
      Y_dist[k] = rbeta(1, a + X_dist[k-1], n - X_dist[k-1] + b)
      X_dist[k] = rbinom(1, n, Y_dist[k-1])
    }
  }
  return(list(X_dist, Y_dist))
}

sample_size = 10000
X_values = NULL
Y_values = NULL
n = 100
a = 61
b = 41

return_list = gibbs_sampler_normal(sample_size, X_values, Y_values, a, b, n)
X_dist = return_list[[1]]
Y_dist = return_list[[2]]

par(mfrow = c(1,2))
hist(X_dist)
lines(density(X_dist), col='red')
hist(Y_dist)


plot(density(Y_dist), main=NA, col='red')
title(main=('X and Y distributions'))
# lines(density(Y_dist), col='blue')
legend(-3,0.3, legend=c('X-jakauma', 'Y-jakauma'), 
       fill=c('red', 'blue'))

