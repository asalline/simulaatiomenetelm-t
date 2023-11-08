# Basic Metropolis-Hastings algorithm for a generic Markov Chain Monte Carlo
MCMC = function(X, sample_size, lower_limit, upper_limit, acceptance=0){
  for (k in 2:sample_size){
    Y = runif(1, lower_limit,upper_limit)
    rho =  min(dnorm(Y,mean=10,sd=20) / dnorm(X[k-1],mean=10,sd=20), 1)
    if (runif(1)<rho){
      acceptance = acceptance + 1
      X[k] = Y
    } else {
      X[k] = X[k-1]
    }
  }
  return(list(X, acceptance))
}

# Function for Monte Carlo ratio
MC_ratio = function(x, xt){
  (f(x)*g(xt, xt, sd))/(f(xt)*g(x, xt, sd))
}

# Function for the goal distribution
f = function(x, mean=10,sd=20){
  dnorm(x, mean, sd)
}

# Function for the proposition distribution
g = function(x, xt, sd=sd){
  dnorm(x, xt, sd)
}

burn_in = function(sample, amount_to_get_rid_of){
  sample = sample[amount_to_get_rid_of:length(sample)]
}

sample_size = 100

# T1a)
lower_limit = -100
upper_limit = 100
X = 10

return_list = MCMC(X, sample_size, lower_limit, upper_limit)
dist_values1 = return_list[[1]]
acceptance1 = return_list[[2]]

#T1b)
X_values2 = NULL
X_values2[1] = 10
sd = 75
l = 50
#acceptance = 0
independent_MH = function(X_list, prop_distribution, sd, l){
  acceptance = 0
  for (k in 2:sample_size){
    if (prop_distribution == 'b'){
      x = rnorm(1, X_list[k-1], sd)
    } else if (prop_distribution == 'c') {
      x = runif(1, xt-l, xt+l)
    }
    xt = X_list[k-1]
    mc = MC_ratio(x,xt)
    p = min(mc, 1)
    d = rbinom(1,1,p)
    if (d == 1){
      acceptance = acceptance + 1
    }
    X_list[k] = x*d + xt*(1-d)
  }
  return(list(X_list, acceptance))
}

b_prop_dist = function(X, sd){
  rnorm(1, X[k-1], sd)
}
return_list = independent_MH(X_values2, prop_distribution='b', sd, l)
dist_values2 = return_list[[1]]
acceptance2 = return_list[[2]]

X_values3 = NULL
X_values3[1] = 10

return_list2 = independent_MH(X_values3, prop_distribution='c', sd, l)
dist_values3 = return_list2[[1]]
acceptance3 = return_list2[[2]]

# X_values
a_ratio2 = acceptance / sample_size
plotting = function(dist_values1, dist_values2, dist_values3, initial){
  plot(density(dist_values3), main=NA, col='red', xlim = c(-50,50), ylim = c(0, 0.025))
  title(main=paste0('Initial guess: ', initial))
  lines(density(dist_values2), col='blue')
  lines(density(dist_values1), col='green')
  lines(density(rnorm(10000,10,20)), col='black')
  legend(-50,0.015, legend=c('Kohdejakauma', 'a-kohta', 'b-kohta', 'c-kohta'), 
         fill=c('black', 'green', 'blue', 'red'))
}

acc_ratio = function(acceptance, sample_size){
  acc_ratio = acceptance/sample_size
}

plotting(dist_values1, dist_values2, dist_values3, X)
print(paste0("a) acceptance ratio: ", acc_ratio(acceptance1, sample_size)))
print(paste0("b) acceptance ratio: ", acc_ratio(acceptance2, sample_size)))
print(paste0("c) acceptance ratio: ", acc_ratio(acceptance3, sample_size)))

vec = c(1,5,100)
X = vec
X_values2 = vec
X_values3 = vec

for (k in 1:3){
  
  return_list = MCMC(X[k], sample_size, lower_limit, upper_limit)
  dist_values1 = return_list[[1]]
  a_ratio1 = return_list[[2]]
  return_list = independent_MH(X_values2[k], prop_distribution='b', sd, l)
  dist_values2 = return_list[[1]]
  a_ratio2 = return_list[[2]]
  return_list2 = independent_MH(X_values3[k], prop_distribution='c', sd, l)
  dist_values3 = return_list2[[1]]
  a_ratio3 = return_list2[[2]]
  plotting(dist_values1, dist_values2, dist_values3, X[k])
  print(paste0("Initial guess: ", X[k]))
  print(paste0("a) acceptance ratio: ", acc_ratio(acceptance1, sample_size)))
  print(paste0("b) acceptance ratio: ", acc_ratio(acceptance2, sample_size)))
  print(paste0("c) acceptance ratio: ", acc_ratio(acceptance3, sample_size)))
}

for (k in 1:3){
  
  return_list = MCMC(X[k], sample_size, lower_limit, upper_limit)
  dist_values1 = burn_in(return_list[[1]], 10)
  a_ratio1 = return_list[[2]]
  return_list = independent_MH(X_values2[k], prop_distribution='b', sd, l)
  dist_values2 = burn_in(return_list[[1]], 10)
  a_ratio2 = return_list[[2]]
  return_list2 = independent_MH(X_values3[k], prop_distribution='c', sd, l)
  dist_values3 = burn_in(return_list2[[1]], 10)
  a_ratio3 = return_list2[[2]]
  plotting(dist_values1, dist_values2, dist_values3, X[k])
  print(paste0("Initial guess: ", X[k]))
  print(paste0("a) acceptance ratio: ", acc_ratio(acceptance1, sample_size)))
  print(paste0("b) acceptance ratio: ", acc_ratio(acceptance2, sample_size)))
  print(paste0("c) acceptance ratio: ", acc_ratio(acceptance3, sample_size)))
}



