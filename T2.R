sample_size = 10000
h = function(x){
  exp(-2*x) + 1
}

f = rnorm(sample_size, 0, 1)

g_1 = runif(sample_size)

expectation_1 = 1/sample_size * (sum((dnorm(g_1)/dunif(g_1))*h(g_1)))
expectation_1

g_2 = rexp(sample_size)

expectation_2 = 1/sample_size * (sum((dnorm(g_2)/dexp(g_2))*h(g_2)))
expectation_2

g_3 = rt(sample_size, 2)

expectation_3 = 1/sample_size * (sum((dnorm(g_3)/dt(g_3, 2))*h(g_3)))
expectation_3

g_4 = rnorm(sample_size)
         
expectation_4 = 1/sample_size * (sum((dnorm(g_4)/dnorm(g_4)*h(g_4))))
expectation_4

#plot(cumsum((dnorm(g_1)/dunif(g_1))*h(g_1)), type='l', ylim=c(0,100000))
#lines(cumsum(dnorm(g_2)/dexp(g_2))*h(g_2))
#lines(cumsum(dnorm(g_3)/dt(g_3, 2))*h(g_3))
#lines(cumsum(dnorm(g_4)/dnorm(g_4)*h(g_4)))

ess_1 = ESS(expectation_1)
ess_1
