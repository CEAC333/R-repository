subrutina_triangulo<-function(n,optimista,media,pesimista){

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
aceptadosx 
} #cIERRE SUBRUTINA

ruta<-numeric()
tiempo<-numeric()
conteo_exito<-0
w<-1000
for (k in 1:w){
  A<-subrutina_triangulo(1,1,2,3)
  B<-subrutina_triangulo(1,2,3.5,8)
  C<-subrutina_triangulo(1,6,9,18)
  D<-subrutina_triangulo(1,4,5.5,10)
  E<-subrutina_triangulo(1,1,4.5,5)
  F<-subrutina_triangulo(1,4,4,10)
  G<-subrutina_triangulo(1,5,6.5,11)
  H<-subrutina_triangulo(1,5,8,17)
  I<-subrutina_triangulo(1,3,7.5,9)
  J<-subrutina_triangulo(1,3,9,9)
  K<-4
  L<-subrutina_triangulo(1,1,5.5,7)
  M<-subrutina_triangulo(1,1,2,3)
  N<-subrutina_triangulo(1,5,5.5,9)
  
  ruta[1]<-A+B+C+D+G+H+M
  ruta[2]<-A+B+C+E+H+M
  ruta[3]<-A+B+C+E+F+J+K+N
  ruta[4]<-A+B+C+E+F+J+L+N
  ruta[5]<-A+B+C+I+J+K+N
  ruta[6]<-A+B+C+I+J+L+N
  tiempo[k]<-max(ruta)
  if(max(ruta)<=47){
    conteo_exito<-conteo_exito+1
  }
}#cierre for 
histo<-hist(tiempo)
prom<-mean(tiempo)
prob<-conteo_exito/w


