library("quantmod", lib.loc="~/R/win-library/3.2")
getSymbols("ALFAA.MX;PINFRA.MX;^MXX;MXN=X;TLEVISACPO.MX", from="2015-02-26", to="2016-02-26")

Alfa<- as.vector(ALFAA.MX[,6])
Pinfra <- as.vector(PINFRA.MX[,6])
IPC <- as.vector(MXX[,6])
Dolar <- as.vector(`MXN=X`[,6])
Televisa <- as.vector(TLEVISACPO.MX[,6])

Ralfa <- mean(diff(log(Alfa)))
Rpinfra <- mean(diff(log(Pinfra)))
Ripc <- mean(diff(log(IPC)))
Rdolar <- mean(diff(log(Dolar)))
Rtelevisa <- mean(diff(log(Televisa)))

Valfa <- sd(diff(log(Alfa)))
Vpinfra <- sd(diff(log(Pinfra)))
Vipc <- sd(diff(log(IPC)))
Vdolar <- sd(diff(log(Dolar)))
Vtelevisa <- sd(diff(log(Televisa)))



                                        #Parámetros de Entrada            

r<-Rdolar                    #Rendimiento esperado el activo
sigma<-Vdolar                #Volatilidad del rendimiento del activo
k<-18.5                      #Precio StrikeIPC[length(IPC)]
St<- Dolar[length(Dolar)]    #Precio Spot
Te<-76                       #Tiempo Final
t<-0                         #Tiempo inicial
n<-1000000             #Numero de simulaciones para Montecarlo y Analítico
tipo<-2                #1 para Call y 2 para Put
alfa<-1                #Parámetro de elasticidad de varianza para Euler
dt<-.01                #Tañmaño de pasos para Euler
sim<-1000              #numero de simulaciones para Euler

SolBS<-BS(r,sigma,k,St,Te,t,tipo)
SolAnalitica<-Analitico(r,sigma,k,St,Te,t,n,tipo)
SolMontecarlo<-Montecarlo(r,sigma,k,St,Te,t,n,tipo)
SolEuler<-Euler(r,sigma,k,St,Te,alfa,dt,sim,tipo)


                                           #Solucion montecarlo


Montecarlo<- function(r,sigma,k,St,Te,t,n,tipo){
  
  if(tipo==1){
    a <- log(k/St)
    b <- (r-sigma^2/2)*(Te-t)+6*sigma*sqrt(Te-t)
    h <- (b-a)/n
    x <- runif(n)*(b-a)+a
  }
  if(tipo==2){
    b <- log(k/St)
    a <- ((r-sigma^2/2)*(Te-t)+6*sigma*sqrt(Te-t))*-1
    h <- (b-a)/n
    x <- runif(n)*(b-a)+a
  }
  
  funcion1 <- function(x){
    if(tipo==1){
      
      
      Ct <-(St*exp(x)-k)*1/(sigma*sqrt((Te-t)*2*pi))*exp(-(x-(r-sigma^2/2)*(Te-t))^2/(2*(sigma^2)*(Te-t)))
      Ct[Ct<0] <- 0
      
      }
    if(tipo==2){
      Ct <-(k-St*exp(x))*1/(sigma*sqrt((Te-t)*2*pi))*exp(-(x-(r-sigma^2/2)*(Te-t))^2/(2*(sigma^2)*(Te-t)))
      Ct[Ct<0] <- 0
      
      }
    return(Ct)    
  }
  
  Ct <- h*exp(-r*(Te-t))* sum(funcion1(x))
  return(Ct)
}



                                      #Solucion Black-Sholes


BS<- function(r,sigma,k,St,Te,t,tipo){
  if (tipo==1){   #Call europeo
    
    CTeo <- St*pnorm((log(St/k)+(r+sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)-k*exp(-r*(Te-t))*pnorm((log(St/k)+(r-sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)
  }
  if (tipo==2){   #Put europeo
    CTeo <- -St*pnorm(-(log(St/k)+(r+sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)+k*exp(-r*(Te-t))*pnorm(-(log(St/k)+(r-sigma^2/2)*(Te-t))/(sigma*sqrt(Te-t)),0,1)
  }
  
  return(CTeo)
  
}


                                          #montecarlo sobre Solución Analítica 


Analitico<-function(r,sigma,k,St,Te,t,n,tipo){
  Zt <- rnorm(n)
  if (tipo==1){  #Call europeo
    
    ST <- St*exp((r-sigma^2/2)*(Te-t)+sigma*sqrt(Te-t)*Zt)-k #solo se simula el última valor en tiempo T( en lugar de toda la trayectoria)
    ST[ST<0] <- 0 #cuando el precio simulado menos el precio strike es menor a 0, el valor es 0
    Ct <- exp(-r*(Te-t))*mean(ST)
   
  }
  
  if (tipo==2){  #Put europeo
    ST <- k-St*exp((r-sigma^2/2)*(Te-t)+sigma*sqrt(Te-t)*Zt)
    ST[ST<0] <- 0
    Ct <- exp(-r*(Te-t))*mean(ST)
    
  }
  return(Ct)
}



                            ## Elasticidad constante de la varianza (Método de Euler)


Euler<-function(r,sigma,k,St,Te,alfa,dt,sim,tipo){
  
  t <- seq(0, Te, dt)
  n <- length(t)
  Xt <- matrix(, nrow = sim, ncol = n)
  Zt <-  matrix(data = rnorm(n*sim), nrow = sim, ncol = n)
  
  Xt[,1] <- St
  for (i in 2:n) {
    Xt[,i] <- Xt[,i-1] + dt*r*Xt[,i-1]+sigma*(Xt[,i-1]^alfa)*sqrt(dt)*Zt[,i-1]
  }
  if (tipo==1){  #Call europeo
  intm<-Xt[,n]-k
  intm[intm<0]<-0
  }
  if (tipo==2){  #Put europeo
  intm<-k-Xt[,n]
  intm[intm<0]<-0
  }
  Ct<-exp(-r*(Te))*mean(intm)
  
  return(Ct)
}
