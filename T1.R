f = dnorm(0)
#M = 2.5
sample_size = 10000
limit = 4
M = max(f) * 2*limit
u = runif(sample_size)
y = runif(sample_size, -limit, limit)
dist = 0
j=1
x=0
for (k in 1:sample_size){
	if (u[k] < dnorm(y[k])/(M*dunif(y[k], -limit, limit))){
	  x[j] = y[k]
	  j = j+1
	}
}
length(x)/sample_size
plot(density(x))




