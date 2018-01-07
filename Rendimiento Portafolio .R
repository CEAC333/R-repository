
#portafolio
nA<-.5
nB<-.5
miuA <-.15/252
miuB<-.12
Te <- 126
t <-0
sigmaA <- .2/sqrt(252)
sigmaB<- .18/sqrt(252)
StA <- 100
StB<- 75
n <- 1000000
STA<- StA*exp((miuA-sigmaA^2/2)*(Te-t)+sigmaA*sqrt(Te-t)*Zt)
STB<-StA*exp((miuB-sigmaB^2/2)*(Te-t)+sigmaB*sqrt(Te-t)*Zt)
po<-nA*StA + nB*StB
pT<-nA*STA + Nb*STB

rend<-(pT/po)
contador<-length(rend[rend<=.9])
prob<-contador/n






