sigma <- 0.77/100
Xo <- 1.33 #precio inicial
k<-1.33 #precio strike 
r<-.072/100 # tasa libre de riesgo 
alfa<- 1 # coeficiente de la elasticidad de la varianza 
Te <- 1
dt <- .0001
sim<- 10000
t <- seq(0, Te, dt)
n <- length(t)
Xt <- matrix(, nrow = sim, ncol = n)
Zt <-  matrix(data = rnorm(n*sim), nrow = sim, ncol = n)

Xt[,1] <- Xo
for (i in 2:n) {
  Xt[,i] <- Xt[,i-1] + dt*r*Xt[,i-1]+sigma*(Xt[,i-1]^alfa)*sqrt(dt)*Zt[,i-1]
}

intm<-Xt[,n]-k
intm[intm<0]<-0
ct<-exp(-r)*mean(intm)

CTeo <- Xo*pnorm((log(Xo/k)+(r+sigma^2/2)*(Te))/(sigma*sqrt(Te)),0,1)-k*exp(-r*(Te))*pnorm((log(Xo/k)+(r-sigma^2/2)*(Te))/(sigma*sqrt(Te)),0,1)

