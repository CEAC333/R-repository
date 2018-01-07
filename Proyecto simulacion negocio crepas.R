# Del total de crepas vendidad 60% son saladas y 40% son dulces.
# De los Datos recabados se observó que los rangos de tiempo de prepación de las crepas 
# varía dependiendo si son crepas saladas o dulces, Rango dulces [2,4], Rango Saladas [3,5] min.
# se utilizará una distribución uniforme para simular los tiempos de prepación de las crepas.

n<-100
sabor_crepa<-runif(n*3)
saladas<-0
dulces<-0


for (i in 1:(n*3)){

  if(sabor_crepa[i]<.60){ 
  sabor_crepa[i]<-1  # las crepas saladas serán representadas con el número 1
  saladas<-saladas+1
  
  }else {
  sabor_crepa[i]<-2  # las crepas dulces serán representadas con el número 2
  dulces<-dulces+1
         }
               }  #fin ciclo for 


tiempo_s<-round((runif(n*3)*(300-180)+180)) #tiempo preparación crepas saladas en segundos
tiempo_d<-round((runif(n*3)*(240-120)+120)) #tiempo preparación crepas dulces en segundos
tiempo_crepa<-numeric()

for(i in 1:(n*3)){   # ciclo for para tiempo preparación de crepas 
  
  if(sabor_crepa[i]==1){
    tiempo_crepa[i]<-tiempo_s[i]
  }else{
    tiempo_crepa[i]<-tiempo_d[i]
  }
   
} #fin ciclo for


#Ahora se simulará cuantas crepas pide cada cliente, de los datos se obtivieron las siguientes
#probabilidades:
#(probabilidad de que un cliente pide una crepa) 1= 37.84% , 2= 41.22% , 3= 12.16%
# 4= 7.43% y 5= 1.35 %

numeros<-c(1,2,3,4,5)
proba<-c(.3784,.4122,.1216,.0743,.0135)
numero_crepas<-sample(numeros, n, replace = TRUE, prob = proba)

#se simulará los tiempo de llegada de los clientes, con los supuestos de que los días miercoles,
#viernes y sábado se tienen mayor número de clientes que los otros 3 días(M,J,D).
# Además todos los días despúes de las 8 pm se observa una mayor concentracíon de clientes.
# Por lo tanto se tienen 4 diferentes escenarios:
# 1. Los días con menor número de clientes en las horas de menos concentración de llegada 
#de los mismos.
# 2. Los días con menor número de clientes en las horas de mayor concentración de llegada 
#de los mismos.
# 3. Los días con mayor número de clientes en las horas de menos concentración de llegada 
#de los mismos.
# 4. Los días con mayor número de clientes en las horas de mayor concentración de llegada 
#de los mismos.

tiempo_clientes_1<-round(rnorm(n,19.19,3.93))
tiempo_clientes_2<-round(rnorm(n,12.72,3.58))
tiempo_clientes_3<-round(rnorm(n,18,3.30))
tiempo_clientes_4<-round(rnorm(n,10.15,2.78))


#Aquí se simularán los tiempos que le toma al cliente elegir,ordenar y pagar.
# Se supuso que el 70% de los clientes que llegan no saben aún que ordenaran
# y el 30% restante ya sabe que ordenará desde que llegan.

numero_aleatorio<-runif(n)
tipo_cliente<-numeric()

for (j in 1:n){
  
  if(numero_aleatorio[j]<.70){ 
    tipo_cliente[j]<-1  # los clientes que no saben que ordenaran serán representados con el número 1
    
  }else {
    tipo_cliente[j]<-2  # los clientes que si saben que ordenaran serán representados con el número 2
    
  }
}  #fin ciclo for 

#A los clientes que ya saben que ordenarán(tipo 2), el tiempo que les toma realizar el proceso 
#de atención hasta que pagan(tiempo orden) es de 1 a 2 min y a los clientes que aún no 
#saben cuándo llegan(tipo 1) les toma entre 1 y 3 min.

tiempo_tipo_1<-round((runif(n)*(180-60)+60)) #tiempo orden en clientes que no saben que ordenarán
tiempo_tipo_2<-round((runif(n)*(120-60)+60)) #tiempo orden en clientes que si saben que ordenarán

tiempo_orden<-numeric()

for(j in 1:n){   # ciclo for para tiempo orden (desde que llega hasta que paga las crepas)
  
  if(tipo_cliente[j]==1){
    tiempo_orden[j]<-tiempo_tipo_1[j]
  }else{
    tiempo_orden[j]<-tiempo_tipo_2[j]
  }
  
} #fin ciclo for

#se necesitará obtener una variable de tiempo de atención en minutos que incluya:
# 1. tiempo que se tarda el cliente en ordenar(tiempo_orden)
# 2. tiempo que se tardan las crepas que pidio el cliente en estar listas.

crepas_pedidas<-0
tiempo_atencion<-numeric()

for(i in 1:n){
  
  if(numero_crepas[i]==1){
    
    tiempo_atencion[i]<-(tiempo_orden[i]+tiempo_crepa[i])/60
    crepas_pedidas<-crepas_pedidas+1
  }
  
  if(numero_crepas[i]==2){
    
    tiempo_atencion[i]<-(tiempo_orden[i]+tiempo_crepa[i]+tiempo_crepa[i+60])/60
    crepas_pedidas<-crepas_pedidas+2 
  }
  
  if(numero_crepas[i]==3){
    
    tiempo_atencion[i]<-(tiempo_orden[i]+tiempo_crepa[i]+tiempo_crepa[i+60]+tiempo_crepa[i+120])/60
    crepas_pedidas<-crepas_pedidas+3 
  }
  
  if(numero_crepas[i]==4){
    
    tiempo_atencion[i]<-(tiempo_orden[i]+tiempo_crepa[i]+tiempo_crepa[i+60]+tiempo_crepa[i+120]
                      + tiempo_crepa[i+160])/60
    crepas_pedidas<-crepas_pedidas+4 
  }
  
  if(numero_crepas[i]==5){
    
    tiempo_atencion[i]<-(tiempo_orden[i]+tiempo_crepa[i]+tiempo_crepa[i+60]+tiempo_crepa[i+120]
                      + tiempo_crepa[i+160]+ tiempo_crepa[i+180])/60
    crepas_pedidas<-crepas_pedidas+5 
  }
  
}#fin ciclo for


#ahora que ya se tienen todas las variables necesarias, se simularán los días de la semana por 
#separado.

#Para simular los diferentes días de la semana solo se necesita en el caso 
#de los días con menos clientela(Martes,Jueves,DOmingo) cambiar las variables del
#Renglón (1) y (3) por "tiempo_clientes_1" y el renglón (2) por "tiempo_clientes_2"
# y en el caso de los días con más clientela(Miercoles,Viernes y Sábado)
# en el renglón (1) y (3) "tiempo_clientes_3" y renglón (2) "tiempo clientes_4"

#nota: También es muuy recomendable correr cada vez todas las variables anteriores 
# en este código.

#MARTES(este día se considera uno de los dias con menor número de clientes):

c<-100;      #Número de clientes para simulación
HAM<-5;      #Hora de apertura (e.g. apertura a las 1700hrs)
HCM<-360;    #Hora de cierre en minútos (1700hrs-2300hrs)


#Tabla General
# Formato: # Cliente / Tiempo que tarda en llegar / Minuto en el cual llega / Tiempo de atención /
#Tiempo de espera / Tiempo de finalización 

m=matrix(nrow=c,ncol=6) 
for(i in 1:c){
  
  m[i,1]<-i  #Numero de cliente
  
  if(i==1){
     m[i,2]<-tiempo_clientes_1[i]      #%%%%%%%%%%%%(1) Renglón 1
  }else{
    if(m[i-1,3]>=180){
      m[i,2]<-tiempo_clientes_2[i]     #%%%%%%%%%%%%(2)  #Tiempo que tarda en llegar el cliente i después que el cliente i-1
    }else{
      m[i,2]<-tiempo_clientes_1[i]     #%%%%%%%%%%%%(3) Renglón 3
    } 
  }
  
  if(i==1){
   m[i,3]<-m[i,2]                       #Minuto en el cual llega el cliente 1
   }else{
    m[i,3]<-m[i-1,3]+m[i,2]             #Minuto en el cual llega el cliente i, donde i>1
   }


  m[i,4]<-tiempo_atencion[i]            #Tiempo de atención por cliente

  if(i==1){
    m[i,5]<-0                           #Tiempo de espera para el primer cliente (espera 0 min.)
  }else{
    
    if(m[i-1,6]>m[i,3]){
      m[i,5]<-m[i-1,6]-m[i,3]           #Posible tiempo de espera (en cola) para el i-ésimo cliente, en donde i>1
    }else{
    m[i,5]<-0                           #Posible tiempo de espera (en cola) para el i-ésimo cliente, en donde i>1
         }
  }   
 
  m[i,6]<-m[i,3]+m[i,4]+m[i,5]            #Minuto en el cual el cliente se retira de la sucursal (tiempo de finalización)

} #fin ciclo for


#Tabla de "Número de cliente" / "Ingreso del cliente" / "Salida del cliente" en minutos
ñ=matrix(nrow=n,ncol=3)
for(i in 1:n){
  ñ[i,1]<-m[i,1]
  ñ[i,2]<-m[i,3]
  ñ[i,3]<-m[i,6]
}

#Tabla para hora "Ingreso de cliente" -/#cliente /Desgloce en Horas/ minutos /segundos
o=matrix(c(1),nrow=n,ncol=4)
for(i in 1:n){
  o[i,1]<-m[i,1] #Columna para identificar número de cliente
  o[i,2]<-floor(ñ[i,2]/60)+HAM #COlumna para identificar hora de ingreso
  o[i,3]<-floor(ñ[i,2]-60*floor(ñ[i,2]/60)) #Columna para identificar minutos complementarios a la hora de ingreso
  o[i,4]<-round((ñ[i,2]-60*floor(ñ[i,2]/60)-floor(ñ[i,2]-60*floor(ñ[i,2]/60)))*60,digits=0) #Columna para ident. seg. complemetarios a la hora de ingreso
}

#Determinar el último cliente recibido.
#Supuesto: Se atienden a todos los clientes que lleguen antes de 23hrs (360 min.)

m_recibido=matrix(nrow=1,ncol=4)
for(i in 1:n){
  if(m[i,3]<=HCM){
    num_ultimo_cliente_recibido<-m[i,1]
    m_recibido[1,1]<-o[i,1]
    m_recibido[1,2]<-o[i,2]
    m_recibido[1,3]<-o[i,3]
    m_recibido[1,4]<-o[i,4]
  }  
}

#Tabla para "Finalización del cliente" - /#cliente/ Desfloce en horas / minutos / segundos.
p=matrix(nrow=n,ncol=4)
for(i in 1:n){
  p[i,1]<-m[i,1] #Columna para identificar número de cliente
  p[i,2]<-floor(ñ[i,3]/60)+HAM #COlumna para identificar hora de finalización
  p[i,3]<-floor(ñ[i,3]-60*floor(ñ[i,3]/60)) #Columna para identificar minutos complementarios a la hora de finalización
  p[i,4]<-round((ñ[i,3]-60*floor(ñ[i,3]/60)-floor(ñ[i,3]-60*floor(ñ[i,3]/60)))*60,digits=0) #Columna para ident. seg. complemetarios a la hora de finalización
}

#Determinar hora finalización del último cliente atendido.
#Supuesto: Se atienden a todos los clientes que lleguen antes de 23hrs (360 min.), por lo tanto se cierra después de las 23hrs.
m_finalizo=matrix(nrow=1,ncol=4)
m_finalizo[1,1]<-p[num_ultimo_cliente_recibido,1]
m_finalizo[1,2]<-p[num_ultimo_cliente_recibido,2]
m_finalizo[1,3]<-p[num_ultimo_cliente_recibido,3]
m_finalizo[1,4]<-p[num_ultimo_cliente_recibido,4]

#aquí se determina cuantas crepas fueron vendidas en el día

crepas_vendidas<-0

for(j in 1:num_ultimo_cliente_recibido){
  crepas_vendidas<-crepas_vendidas+numero_crepas[j]
}
#Resultados relevantes de la simulación:

crepas_vendidas #crepas vendidas en el día
num_ultimo_cliente_recibido #Número de clientes que fueron atendidos y llegaron antes de las 23hrs.
m_recibido #Formato: (cl/hr/min/seg) #Hora en la cual llegó el último cliente. 
m_finalizo #Formato: (cl/hr/min/seg) #Hora en la cual se terminó de atender al último cliente.