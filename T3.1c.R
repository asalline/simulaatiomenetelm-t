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
X_values = NULL
X_values[1] = 10
sd = 75
l = 20
acceptance = 0

for (k in 2:sample_size){
  xt = X_values[k-1]
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
  X_values[k] = x*d + xt*(1-d)
  # print(X_values[k])
}

acceptance_ratio = acceptance / sample_size
# X_values
print(acceptance_ratio)
plot(density(X_values))