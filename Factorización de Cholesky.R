matriz<-matrix(data=c(1,.5,.6,.8,1,.5,.3,.6,1),nrow=3,ncol=3)
#Choleskinskys
chole<-function(matriz,n){
  C <- chol(matriz)
  MatrizVer <- t(C) %*% C
  tama�o<-ncol(matriz)
  Z <- matrix (data = c(rnorm(n*tama�o)), nrow = tama�o, ncol = n)
  X <-t(C)  %*% Z
  x<-t(X)
  MatrizVer2 <- cov(x)
  return(x)
}