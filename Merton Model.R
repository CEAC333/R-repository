mydata<-equity

equity<-mydata$price*mydata$shares

debt<-mydata$debt[1]

asset<-equity+debt

r<-0.03/252 #tasa lbre de riesgo diaria

time<-length(equity)

rendimiento_E<-diff(equity)/equity[-length(equity)]

sigma_E<-sd(rendimiento_E)

t<-mydata$day

phi<-function(x){
  
  phi<-2* pnorm(x*sqrt(2))-1
  
  return(phi)
  
}

#primera interacion

sigma_A<-sigma_E*equity[1]/asset[1]

d1<-(log(asset/debt)+(r+sigma_A^2/2)*(time-t))/(sigma_A*sqrt(time-t))

d2<-d1-(sigma_A*sqrt(time-t))

phi_d1<-phi(d1)

phi_d2<-phi(d2)

asset<-(equity+phi_d2*debt*exp(-r(time-t)))/phi_d1

sigma_prev<-sigma_A

sigma_n<-sigma_E*equity[1]/(asset[1]*phi_d1)

#las siguientes interaciones

while((sigma_n-sigma_prev)>0.001){
  
  d1<-(log(asset/debt)+(r+sigma_n^2/2)*(time-t))/(sigma_n*sqrt(time-t))
  
  d2<-d1-(sigma_n*sqrt(time-t))
  
  phi_d1<-phi(d1)
  
  phi_d2<-phi(d2)
  
  asset<-(equity+phi_d2*debt*exp(-r(time-t)))/phi_d1
  
  sigma_prev<-sigma_n
  
  sigma_n<-sigma_E*equity[1]/(asset[1]*phi_d1)
  
}#fin ciclo while

sigma_A<-sigma_n

miu_A<-mean(asset