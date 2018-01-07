##################### funcion prob caida portafolio (académico) 

portafolioac <- function(miuA,miuB,sA,sB,w1,w2,Rho,stA,stB,n,Te,t){#Rendimientos y Desv anuales en decimal
  
  sigmaA <- sA
  sigmaB<- sB
  
  Matriz <- matrix(data = c(sigmaA^2, sigmaA*sigmaB*Rho, sigmaA*sigmaB*Rho, sigmaB^2), nrow = 2, ncol = 2) 
  C <- chol(Matriz)
  Z <- matrix (data = c(rnorm(n),rnorm(n)), nrow = 2, ncol = n)
  X <- t(C) %*% Z
  x<-t(X)
  
  
  STA<- stA*exp((miuA-(sigmaA^2)/2)*(Te-t)+(sigmaA*sqrt(Te-t)*x[,1]))
  STB<- stB*exp((miuB-(sigmaB^2)/2)*(Te-t)+(sigmaB*sqrt(Te-t)*x[,2]))
  po<-w1*stA + w2*stB
  pT<-w1*STA + w2*STB
  
  rend<-(pT/po)
  contador<-length(rend[rend<=1])
  prob<-contador/n
  return(prob)
}


##################### funcion prob caida portafolio (datos reales) 
library("quantmod", lib.loc="~/R/win-library/3.2")
getSymbols("HCITY.MX;LALAB.MX;ALPEKA.MX", from="2015-02-26", to="2016-02-26")

Gruma <- as.vector(HCITY.MX[,6])
Pinfra <- as.vector(LALAB.MX[,6])
Gfinburo <- as.vector(ALPEKA.MX[,6])


Rgruma <- diff(log(Gruma))
Rpinfra <- diff(log(Pinfra))
Rgfinburo <- diff(log(Gfinburo))

rendimientos <- matrix(data=c(Rgruma,Rpinfra,Rgfinburo), nrow=261, ncol=3)

portafoliore <- function(rendimientos,w1,w2,w3,Te,t,n){
  

  matrizcov <- 100*cov(rendimientos)
  C <- chol(matrizcov)
  numact<-ncol(rendimientos)
  numdias<-nrow(rendimientos)
  Z <- matrix (data = c(rnorm(n*numact)), nrow = numact, ncol = n)
  X <- t(C) %*% Z
  x<-t(X)
  
  sta<-Gruma[numdias]
  stb<-Pinfra[numdias]
  stc<-Gfinburo[numdias]

  
  Vmean <- colMeans(rendimientos)
  Vdesv<-apply(rendimientos,2,sd)
  STA<- sta*exp((Vmean[1]-(Vdesv[1]^2)/2)*(Te-t)+Vdesv[1]*sqrt(Te-t)*x[,1])
  STB<- stb*exp((Vmean[2]-(Vdesv[2]^2)/2)*(Te-t)+Vdesv[2]*sqrt(Te-t)*x[,2])
  STC<- stc*exp((Vmean[3]-(Vdesv[3]^2)/2)*(Te-t)+Vdesv[3]*sqrt(Te-t)*x[,3])

  po<-w1*sta+ w2*stb+ w3*stc
  pT<-w1*STA + w2*STB + w3*STC

  rend<-(pT/po)
  contador<-length(rend[rend<=1])
  prob<-contador/n
  
  return(prob)
}


