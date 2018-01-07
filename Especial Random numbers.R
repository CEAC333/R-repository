target=function(x){
  (sin(x)^2)*(sin(2*x)^2)*dnorm(x)*4
  }

metropolis=function(x,alpha=1){
  y=runif(1,x-alpha,x+alpha)
  if (runif(1)>target(y)/target(x)) y=x
  return(y)}

T=10^5
x=rep(pi,T)
for (t in 2:T) x[t]=metropolis(x[t-1])

xs=seq(-4,4,.001)
xr=target(xs)

plot(density(x))
lines(xs,xr)