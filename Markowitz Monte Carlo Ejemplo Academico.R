library("ggplot2", lib.loc="~/R/win-library/3.2")

Nube <- function(miuA,miuB,sA,sB,Rho,n){#Rendimientos y Desv anuales en decimal
  
  rendimientos <- matrix(data=c(miuA,miuB))
  sigmaA<- sA
  sigmaB<- sB
  matrizcov <- matrix(data = c(sigmaA^2, sigmaA*sigmaB*Rho, sigmaA*sigmaB*Rho, sigmaB^2), nrow = 2, ncol = 2)
  activos<-length(rendimientos)
  randnum <-  matrix(data = runif(n*activos), nrow = n, ncol = activos)
  randfin <- rowSums(randnum)
  randnum <- randnum/randfin
  Vmean <- c(miuA,miuB)
  vEsperanza <- crossprod(t(randnum), Vmean)
  mcovpart <-randnum %*% matrizcov %*% t(randnum)
  vDesvSinfor <- sqrt(diag(mcovpart))
  matrizVyE <-  matrix(data = c(vDesvSinfor, vEsperanza), nrow = n, ncol = 2)
  #plot
  data <- data.frame(x= vDesvSinfor, y = vEsperanza)
  plotobj <- qplot (x=vDesvSinfor, y = vEsperanza, xlab = "Desviación", ylab= "Esperanza", color= "Portafolios")
  # + abline(lm(vDesvSinfor~vEsperanza), col="red")
  #+ scale_fill_gradient(max(vEsperanza), limits= c(min(vEsperanza), max(vEsperanza)))
  # plotobj <- ggplot(data) + geom_point(aes(x= vDesvSinfor, y= vEsperanza), color= 'blue')
  
  
return(plotobj)
}
Nube(.0013,.00041,.037,.06,-.2,10000)

