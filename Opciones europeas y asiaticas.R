r <-.03/252
Te <- 252
t <-0
sigma <- .0075/sqrt(252)
k <- 45
St <- 43
n <- 1000000
Zt <- rnorm(n)#variable aleatoria con distribución normal estándar

#Call europeo

ST <- St*exp((r-sigma^2/2)*(Te-t)+sigma*sqrt(Te-t)*Zt)-k #solo se simula el última valor en tiempo T( en lugar de toda la trayectoria)
ST[ST<0] <- 0 #cuando el precio simulado menos el precio strike es menor a 0, el valor es 0
ct <- mean(ST)
CTeo <- St*pnorm((log(St/k)+(r+sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)-k*exp(-r*(Te-t))*pnorm((log(St/k)+(r-sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)

#Put europe

ST <- k-St*exp((r-sigma^2/2)*(Te-t)+sigma*sqrt(Te-t)*Zt)
ST[ST<0] <- 0
Pt <- mean(ST)
PTeo <- -St*pnorm(-(log(St/k)+(r+sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)+k*exp(-r*(Te-t))*pnorm(-(log(St/k)+(r-sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)

#opcion call asiática

ST <- St*exp((r-sigma^2/2)*(Te-t)+sigma*sqrt(Te-t)*Zt)-k #solo se simula el última valor en tiempo T( en lugar de toda la trayectoria)
c<-c(1,2,3,4,5,6,7,8,9,10)
STa<- St*exp((r-sigma^2/2)*(Te(-c)-t)+sigma*sqrt(Te(-c)-t)*Zt)-k
STa[STa<0]<-0
ct<-mean(STa)