library("quantmod", lib.loc="~/R/win-library/3.2")
getSymbols("KOFL.MX;ELEKTRA.MX;PINFRA.MX;SANMEXB.MX;GFINBURO.MX", from="2015-01-01", to="2016-01-01")
library("ggplot2", lib.loc="~/R/win-library/3.2")

Elektra <- as.vector(ELEKTRA.MX[,4])
Kofl <- as.vector(KOFL.MX[,4])
Pinfra <- as.vector(PINFRA.MX[,4])
Sanmexb <- as.vector(SANMEXB.MX[,4])
Gfinburo <- as.vector(GFINBURO.MX[,4])

Relektra <- diff(log(Elektra))
Rkofl <- diff(log(Kofl))
Rpinfra <- diff(log(Pinfra))
Rsanmexb <- diff(log(Sanmexb))
Rgfinburo <- diff(log(Gfinburo))

rendimientos <- matrix(data=c(Relektra,Rkofl,Rpinfra,Rsanmexb,Rgfinburo), nrow=261, ncol=5)
numeros<-10000

Nube <- function(rendimientos,numeros){
  matrizcov <- cov(rendimientos)
  activos<-ncol(rendimientos)
  randnum <-  matrix(data = runif(numeros*activos), nrow = numeros, ncol = activos)
  randfin <- rowSums(randnum)
  randnum <- randnum/randfin
  Vmean <- colMeans(rendimientos)
  vEsperanza <- crossprod(t(randnum), Vmean)
  mcovpart <-randnum %*% matrizcov %*% t(randnum)
  vDesvSinfor <- sqrt(diag(mcovpart))
  matrizVyE <-  matrix(data = c(vDesvSinfor, vEsperanza), nrow = numeros, ncol = 2)
  #plot
  data <- data.frame(x= vDesvSinfor, y = vEsperanza)
  plotobj <- qplot (x=vDesvSinfor, y = vEsperanza, xlab = "Desviación", ylab= "Esperanza", color= 'pink')
  # + abline(lm(vDesvSinfor~vEsperanza), col="red")
  #+ scale_fill_gradient(max(vEsperanza), limits= c(min(vEsperanza), max(vEsperanza)))
  # plotobj <- ggplot(data) + geom_point(aes(x= vDesvSinfor, y= vEsperanza), color= 'blue')
  
  
return(plotobj)
}
Nube(rendimientos,numeros)

