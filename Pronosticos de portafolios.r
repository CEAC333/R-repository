

metrics<-function(a1,a2,a3,a4,a5,n,Te,t)

library("quantmod", lib.loc="~/R/win-library/3.2")
library("fPortfolio", lib.loc="~/R/win-library/3.2")
getSymbols("GRUMAB.MX;LALAB.MX;PINFRA.MX;VOLARA.MX;GFREGIOO.MX", from="2015-01-01", to="2016-01-01")

Te<-252
t<-0
n<-100000
act1 <- as.vector(GRUMAB.MX[,6])
act2 <- as.vector(LALAB.MX[,6])
act3 <- as.vector(PINFRA.MX[,6])
act4 <- as.vector(VOLARA.MX[,6])
act5 <- as.vector(GFREGIOO.MX[,6])

filas<-length(act1)

activos <- matrix(data=c(act1,act2,act3,act4,act5), nrow= filas, ncol= 5)
#colnames(activos) <- c(a1,a2,a3,a4,a5)
activosTS <- as.timeSeries(activos)

Portafolio<-tangencyPortfolio(activosTS)
Pesos<-getWeights(Portafolio)


Ract1 <- diff(log(act1))
Ract2 <- diff(log(act2))
Ract3 <- diff(log(act3))
Ract4 <- diff(log(act4))
Ract5 <- diff(log(act5))

matrizrend <- matrix(data=c(Ract1,Ract2,Ract3,Ract4,Ract5), nrow=filas-1, ncol=5)

matrizcov <- 100*cov(matrizrend)
C <- chol(matrizcov)
numact<-ncol(matrizrend)
Z <- matrix (data = c(rnorm(n*numact)), nrow = numact, ncol = n)
X <- t(C) %*% Z
x<-t(X)

sta<-act1[filas]
stb<-act2[filas]
stc<-act3[filas]
std<-act4[filas]
ste<-act5[filas]


Vmean <- colMeans(matrizrend)
Vdesv<-apply(matrizrend,2,sd)
STA<- sta*exp((Vmean[1]-(Vdesv[1]^2)/2)*(Te-t)+Vdesv[1]*sqrt(Te-t)*x[,1])
STB<- stb*exp((Vmean[2]-(Vdesv[2]^2)/2)*(Te-t)+Vdesv[2]*sqrt(Te-t)*x[,2])
STC<- stc*exp((Vmean[3]-(Vdesv[3]^2)/2)*(Te-t)+Vdesv[3]*sqrt(Te-t)*x[,3])
STD<- std*exp((Vmean[4]-(Vdesv[4]^2)/2)*(Te-t)+Vdesv[4]*sqrt(Te-t)*x[,4])
STE<- ste*exp((Vmean[5]-(Vdesv[5]^2)/2)*(Te-t)+Vdesv[5]*sqrt(Te-t)*x[,5])

po<-Pesos[1]*sta+ Pesos[2]*stb+ Pesos[3]*stc+Pesos[4]*std+Pesos[5]*ste
pT<-Pesos[1]*STA + Pesos[2]*STB + Pesos[3]*STC+Pesos[4]*STD+Pesos[5]*STE

rend<-(pT/po)
contador<-length(rend[rend<=1])
prob<-contador/n
return(prob)
