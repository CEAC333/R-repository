
#Valuación opción call
T <- 252
r <-.03
sigma<- .0060
K <- 30
So <- 25
Z<- rnorm(n)
S<- So* exp((r-sigma^2/2)*T + sigma*sqrt(T)*Z)
Ct <- max((S-K),0)







