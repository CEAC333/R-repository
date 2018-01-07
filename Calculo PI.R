#programa calculo de PI 

n<-100000
suma<-0
n
x<-numeric()
y<-numeric() #paradigma del valor de las medias
r<-numeric()

for(i in 1:n){
  x[i]<-runif(1)
  y[i]<-runif(1)
  r[i]<-(x[i]^2+y[i]^2)^(1/2)
  
  if(r[i]<=1){
    suma<-suma+1
  }

}
pi<-4*(suma/n)
pi