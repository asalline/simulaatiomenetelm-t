### A-kohta

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
  return(list(X, acceptance_ratio))
}

sample_size = 10000
lower_limit = -100
upper_limit = 100
g_1 = runif(sample_size, lower_limit, upper_limit)

X = 10

return_list = MCMC(g_1, X, sample_size, lower_limit, upper_limit)
X_values1 = return_list[[1]]
a_ratio1 = return_list[[2]]
plot(density(X_values1), xlim = c(-50,50))


### B-kohta

MC_ratio = function(x, xt){
  (f(x)*g(xt, xt, sd))/(f(xt)*g(x, xt, sd))
}

f = function(x, mean=10,sd=20){
  dnorm(x, mean, sd)
}

g = function(x, xt, sd=variance){
  dnorm(x, xt, sd)
}

sample_size = 10000
# given_x = 10
X_values2 = NULL
X_values2[1] = 10
sd = 75
acceptance = 0
for (k in 2:sample_size){
  x = rnorm(1, X_values2[k-1], variance) #x
  # print(y)
  xt = X_values[k-1] #xt
  # print(x)
  mc = MC_ratio(x,xt)
  # print(mc)
  p = min(mc, 1)
  # print(p)
  d = rbinom(1,1,p)
  if (d == 1){
    acceptance = acceptance + 1
  }
  X_values2[k] = x*d + xt*(1-d)
  # print(X_values[k])
}

# X_values
a_ratio2 = acceptance / sample_size
# X_values
print(acceptance_ratio)

plot(density(X_values2))


### C-kohta

MC_ratio = function(x, xt){
  (f(x)*g(xt, xt, sd))/(f(xt)*g(x, xt, sd))
}

f = function(x, mean=10,sd=20){
  dnorm(x, mean, sd)
}

g = function(x, xt, sd=variance){
  dnorm(x, xt, sd)
}

sample_size = 10000
# given_x = 10
X_values3 = NULL
X_values3[1] = 10
sd = 75
l = 20
acceptance = 0

for (k in 2:sample_size){
  xt = X_values3[k-1]
  x = runif(1, xt-l, xt+l)
  # print(x)
  mc = MC_ratio(x,xt)
  # print(mc)
  p = min(mc, 1)
  # print(p)
  d = rbinom(1,1,p)
  if (d == 1){
    acceptance = acceptance + 1
  }
  X_values3[k] = x*d + xt*(1-d)
  # print(X_values[k])
}

a_ratio3 = acceptance / sample_size
# X_values
print(acceptance_ratio)
plot(density(X_values3), col='red')

lines(density(X_values2), col='blue')
lines(density(X_values1), col='green')
lines(density(rnorm(10000,10,20)), col='black')
legend(-75,0.020, legend=c('Kohdejakauma', 'a-kohta', 'b-kohta', 'c-kohta'), 
       fill=c('black', 'green', 'blue', 'red'))





