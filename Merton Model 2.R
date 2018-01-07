setwd("C:/Users/usuario/jmo/iteso/examples")

mydata <- read.table("equity.csv",header=T,sep=",")

r <- 0.03/252

Equity <- mydata$price*mydata$shares

Debt_1 <- mydata$debt[1]*1.5

Debt_2<- mydata$debt[1]

Asset <- Equity + Debt_1+Debt_2

Time <- length(Equity)

t <- mydata$day

Phi <- function(x) {
  
  phi <- pnorm(x)
  
  return(phi)
  
}

log_rendimiento_E <- diff(log(Equity))

sigma_E <- sd(log_rendimiento_E)

#Primer iteraciÃ³n

sigma_A <- sigma_E*Equity[1]/Asset[1]

difference <- 1

while (difference > 0.00001){
  
  d1 <- (log(Asset/(Debt_1+Debt_2))+(r+sigma_A^2/2)*(Time-t))/(sigma_A*sqrt(Time-t))
  
  d2 <- d1 - sigma_A*sqrt(Time-t)
  
  phi_d1 <- Phi(d1)
  
  phi_d2 <- Phi(d2)
  
  Asset0 <- Asset
  
  Asset <- (Equity + phi_d2 * (Debt_1+ Debt_2) * exp(-r*(Time-t)))/phi_d1
  
  difference <- sum((Asset-Asset0)^2)
  
  log_rendimiento_A <- diff(log(Asset))
  
  sigma_A <- sd(log_rendimiento_A)
  
}

log_rendimiento_A <- diff(log(Asset))

mu_A <- mean(log_rendimiento_A)

sigma_A <- sd(log_rendimiento_A)

Dividendo_0 <- Equity[length(Equity)]*0.005

g <- 0.03

c_1 <- 0.045

c_2<- 0.04

Dividendos <- c()

Intereses_1 <- c()

Intereses_2 <- c()

Tiempo_1 <- 3

Tiempo_2 <- 6

V_t <- 0

I_t_1 <- 0

I_t_2 <- 0

for (tau in 1:Tiempo_2){
  
  V_t <- V_t + Dividendo_0 * (1+g) ^tau * exp(r*252*(Tiempo_2-(tau-1)))
  
  Dividendos <- c(Dividendos, V_t)
  
  I_t_2 <- I_t_2 + Debt_2 * c_2 * exp(r*252*(Tiempo_2-(tau-1)))
  
  Intereses_2 <- c(Intereses_2, I_t_2)
  
  #log_initial_value <- log(Asset[length(Asset)])
  
  #period <- tau * 252
  
  #asset_value <- rnorm(1000000,log_initial_value + (mu_A+sigma_A^2/2)*period,sigma_A*sqrt(period))
  
  #default_5 <- asset_value[asset_value<log(V_t + I_t)]
  
  #print(sprintf("Probabilidad de Default (Tipo 1) al año %i, de %i: %1.3f",tau,Tiempo
  
  # ,length(default_5)/1000000))
  
}

for (tau in 1:Tiempo_1){
  
  I_t_1 <- I_t_1 + Debt_1 * c_1 * exp(r*252*(Tiempo_1-(tau-1)))
  
  Intereses_1 <- c(Intereses_1, I_t_1)
  
}

difference <- 1

while (difference > 0.00001){
  
  d1 <- (log(Asset/(Debt_1 + Debt_2 + V_t + I_t_1 + I_t_2 ))+(r+sigma_A^2/2)*(Time-t))/(sigma_A*sqrt(Time-t))
  
  d2 <- d1 - sigma_A*sqrt(Time-t)
  
  phi_d1 <- Phi(d1)
  
  phi_d2 <- Phi(d2)
  
  k1 <- (log(Asset/( V_t + I_t_1 + I_t_2))+(r+sigma_A^2/2)*(Time-t))/(sigma_A*sqrt(Time-t))
  
  k2 <- k1 - sigma_A*sqrt(Time-t)
  
  Asset0 <- Asset
  
  primer <- Equity
  
  segundo <- phi_d2 * (Debt_1 + Debt_2 + I_t_1 + I_t_2 + V_t ) * exp(-r*(Time-t))
  
  tercero <- V_t * exp(-r*(Time-t)) * Phi(k2)
  
  divisor <- phi_d1 + V_t * (1- Phi(k1))/(V_t + I_t_1 + I_t_2)
  
  Asset <- (primer + segundo - tercero)/ divisor
  
  difference <- sum((Asset-Asset0)^2)
  
  log_rendimiento_A <- diff(log(Asset))
  
  sigma_A <- sd(log_rendimiento_A)
  
}

log_rendimiento_A <- diff(log(Asset))

mu_A <- mean(log_rendimiento_A)

sigma_A <- sd(log_rendimiento_A)

for (tau in 1:Tiempo_1){
  
  log_initial_value <- log(Asset[length(Asset)])
  
  V_t <- Dividendos[tau]
  
  I_t_1 <- Intereses_1[tau]
  
  I_t_2 <- Intereses_2[tau]
  
  period <- tau * 252
  
  asset_value <- rnorm(1000000,log_initial_value + (mu_A+sigma_A^2/2)*period,sigma_A*sqrt(period))
  
  default_3 <- asset_value[asset_value<log(V_t + I_t_1 + I_t_2)]
  
}

for (tau in 4:Tiempo_2){
  
  V_t <- Dividendos[tau]
  
  I_t_2 <- Intereses_2[tau]
  
  period <- tau * 252
  
  default_6<- asset_value[asset_value<log(V_t + I_t_2)]
  
  print(sprintf("Probabilidad de Default (Tipo 1) al año %i, de %i: %1.3f",tau,Tiempo_2,(length(default_3)+length(default_6)/1000000)))
        
}

period <- Tiempo_2*252

log_initial_value <- log(Asset[length(Asset)])

asset_value <- rnorm(1000000,log_initial_value + (mu_A+sigma_A^2/2)*period,sigma_A*sqrt(period))

default <- asset_value[asset_value<log(Debt_1 +Debt_2 + V_t + I_t_1 + I_t_2)]

print(sprintf("Probabilidad de Default (Tipo 2) al año %i, de %i: %1.3f",Tiempo_2,Tiempo_2
              
              ,(length(default)-length(default_3)-length(default_6)/1000000))))