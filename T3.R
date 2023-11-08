MCMC = function(X_k, X, sample_size, lower_limit, upper_limit, acceptance=0){
  for (k in 2:sample_size){
    #k
    Y = runif(1, lower_limit,upper_limit)
    rho =  min(dnorm(Y,mean=10,sd=20) / dnorm(X[k-1],mean=10,sd=20), 1)
    #rho
    #runif(1)<rho
    if (runif(1)<rho){
      acceptance = acceptance + 1
      X[k] = Y
    } else {
      X[k] = X[k-1]
    }
  }
  # for (k in 2:sample_size){
  #   x = runif(1)
  #   rho = dnorm(x,mean=10,sd=20) / dnorm(Y[k-1],mean=10,sd=20)
  #   X[k] = Y[k-1] + (x - Y[k-1])*(runif(1)<rho)
  # }
  acceptance_ratio = acceptance / sample_size
  print(acceptance_ratio)
  return(X)
}

sample_size = 10000
lower_limit = -100
upper_limit = 100
g_1 = runif(sample_size, lower_limit, upper_limit)

X = 1000

trgt = MCMC(g_1, X, sample_size, lower_limit, upper_limit)

plot(density(trgt), xlim = c(-50,50))

# lines(density(rnorm(2500, mean=10, sd=20)))
# 
# plot(trgt, type='l')

