a<-numeric() #numero aleatorio entre 0-1
b<-numeric() #numero aleatorio entre 0-1
c<-numeric() #numero aleatorio entre 0-1
d<-numeric() #numero aleatorio entre 0-1
x<- numeric() # tiempo entre descomposturas de las maquinas en horas
y<-numeric() #tiempo que le toma a 2 hombres reparar la maquina en horas 
z<-numeric() #tiempo que le toma a 3 hombres reparar la maquina en horas
w<-numeric() #tiempo que le toma a 4 hombres reparar la maquina en horas
tiempoesp2<-0 #tiempo medio de espera para reparar la máquina con 2 hombres
tiempoesp3<-0 #tiempo medio de espera para reparar la máquina con 3 hombres 
tiempoesp4<-0 #tiempo medio de espera para reparar la máquina con 4 hombres 
j<-0          #un contador
k<-0          #un contador
prob3horas<-0 #probabilidad de que una máquina tenga que esperar más de 3 horas para ser reparada por 3 hombres
prob3horas4h<-0 #probabilidad de que una máquina tenga que esperar más de 3 horas para ser reparada por 4 hombres
n<-100

for(i in 1:n){
  a[i]<-runif(1)
  x[i]<-(a[i]*5.5+3.5)
  b[i]<-runif(1)
  y[i]<-(b[i]*8+0)
  c[i]<-runif(1)
  z[i]<-(c[i]*6+0)
  d[i]<-runif(1)
  w[i]<-(d[i]*4+0)
  
  if(z[i]>3){
    j<-j+1
  } #cierre if 
  
  if(w[i]>3){
    k<-k+1
  } #cierre if
  
} #cierre ciclo for

#respuestas inciso a)
tiempoesp2<-mean(y)
tiempoesp2hist<-hist(y)
# probabilidad de que una máquina tenga que esperar más de 8 horas para ser reparada es 
# de 0% porque el intervalo de la distribucion va de 0 a 8.

#respuestas inciso b)
tiempoesp3<-mean(z)
tiempoesp3hist<-hist(z)
prob3horas<-j/n

#respuestas inciso c)
tiempoesp4<-mean(w)
tiempoesp4hist<-hist(w)
prob3horas4h<-k/n


