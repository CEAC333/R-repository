optimista<- 2
pesimista<-8
media<-4
x<-numeric()
y<-numeric()
xn<-numeric()
yn<-numeric()
punto1x<-0
punto1y<-0
punto2x<-0
punto2y<-0
funcion<-numeric()
aceptadosx<-numeric()
aceptadosy<-numeric()
contador<-0

h<-0
n<-1000
i<-0

  
while(contador!=n){  
  i<-i+1
  x[i]<-runif(1)
  y[i]<-runif(1)
  xn[i]<-x[i]*(pesimista-optimista)+optimista 
  h<-(2/(pesimista-optimista))
  yn[i]<-y[i]*(h)
  
  if(xn[i]<=media){
     punto1x<-optimista
     punto1y<-0
     punto2x<-media
     punto2y<-h
     m<-(punto2y-punto1y)/(punto2x-punto1x)
     b<-punto1y-(m*punto1x)
     funcion[i]<-m*xn[i]+b
    }
  if(xn[i]>media){
    punto1x<-pesimista
    punto1y<-0
    punto2x<-media
    punto2y<-h
    m<-(punto2y-punto1y)/(punto2x-punto1x)
    b<-punto1y-(m*punto1x)
    funcion[i]<-m*xn[i]+b  
   }
  
  if(yn[i]<=funcion[i]){
    contador<-contador+1
    aceptadosx[contador]<-xn[i]
    aceptadosy[contador]<-yn[i]
  }
 
}#cierre while 

hist(aceptadosy)
hist(aceptadosx)
