#programa juego puertas
gano<-0
n<-250

for(i in 1:n){
    i
    participante<-runif(1)
    
    if(participante<(1/3)){
        elegida<-1
       }
    if(participante>(1/3)&participante<(2/3)){
        elegida<-2
       }
    if(participante>(2/3)){
        elegida<-3
       }

    catafixia<-runif(1)

    if(catafixia<(1/3)){
       ganadora<-1
      }
    if(catafixia>(1/3)&catafixia<(2/3)){
        ganadora<-2
      }
    if(catafixia>(2/3)){
       ganadora<-3
      }
 
   if(ganadora==elegida){
       gano<-gano+1
      }

  }#fin del for

prob_exito<-gano/n


ganoS<-0
n<-250

for(i in 1:n){
  i
  participanteS<-runif(1)
  
  if(participanteS<(1/3)){
    elegidaS<-1
  }
  if(participanteS>(1/3)&participanteS<(2/3)){
    elegidaS<-2
  }
  if(participanteS>(2/3)){
    elegidaS<-3
  }
  
  catafixiaS<-runif(1)
  
  if(catafixiaS<(1/3)){
    ganadoraS<-1
  }
  if(catafixia>(1/3)&catafixiaS<(2/3)){
    ganadoraS<-2
  }
  if(catafixiaS>(2/3)){
    ganadoraS<-3
  }
  
  if(ganadoraS!=elegidaS){
    ganoS<-ganoS+1
  }
  
}#fin del for

prob_exitoS<-ganoS/n

