#Estimador montecarlo
r <-.03/252
Te <- 252
t <-0
sigma <- .0075/sqrt(252)
k <- 45
St <- 46
n <- 1000000
Zt <- rnorm(n)

a <- log(k/St)
b <- (r-sigma^2/2)*(Te-t)+6*sigma*sqrt(Te-t)
h <- (b-a)/n

funcion1 <- function(x){
  
  y <-(St*exp(x)-k)*1/(sigma*sqrt((Te-t)*2*pi))*exp(-(x-(r-sigma^2/2)*(Te-t))^2/(2*(sigma^2)*(Te-t)))
  return(y)
  
}
x <- runif(n)*(b-a)+a

Ct <- h*exp(-r*(Te-t))* sum(funcion1(x))
CTeo <- St*pnorm((log(St/k)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t)),0,1)-k*exp(-r*(T-t))*pnorm((log(St/k)+(r-sigma^2/2)*(T-t))/(sigma*sqrt(T-t)),0,1)
