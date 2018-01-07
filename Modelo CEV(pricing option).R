st<-1.33 #precio actual del activo
k<-1.33 # precio strike
Te<-252 #vencimiento
r<-.072/100 #tasa de interes libre de riesgo diaria
sigma<-0.77/100 #volalidad diaria
alfa<- 1 #coeficiente elasticidad de la varianza
N<- 24 #número de pasos para el método de euler
h<-T/N #tamaño de los pasos método euler
n<-10000 #número simulaciones 

STsum<-0
for(i in 1:n){
x1<-st
x2<-sigma

 for (j in 1:N){
  
  w<-sqrt(h)*randn
  x1<- x1 + h*r*x1 + x2*x1^alfa*w
 }#fin segundo ciclo for

STsum<-STsum + max(0,x1-k)
}#fin ciclo for

callprice<-exp(-r)*STsum/n