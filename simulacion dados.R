x<-numeric() #numero aleatorio entre 0-1
y<-numeric() #numero aleatorio entre 0-1
num1<-numeric() #numero del dado 1 sin redondear
num2<-numeric() #numero del dado 2 sin redondear
dado1<-numeric() #dado 1
dado2<-numeric() #dado 2
gana<-0
pierde<-0
pending<-0
n<-100




for(i in 1:n){
  
  while(gana!=1 & pierde!=1){

  if(pending==1 and suma[i]==4){
    gana<-1
  }#cierre if
  if(pending==2 and suma[i]==5){
    gana<-1
  }#cierre if
  if(pending==3 and suma[i]==6){
    gana<-1
  }#cierre if
  if(pending==4 and suma[i]==8){
    gana<-1
  }#cierre if
  if(pending==5 and suma[i]==9){
    gana<-1
  }#cierre if
  if(pending==6 and suma[i]==10){
    gana<-1
  }#cierre if  
  
  if(pending==1 || pending==2 || pending==3 || pending==4 || pending==5 || pending==6 & suma[i]==7 ){
    pierde<-1
  }#cierre if
  
x[i]<-runif(1)
num1[i]<-x[i]*5+1
dado1[i]<-round(num1[i]+0.5) 

y[i]<-runif(1)
num2[i]<-y[i]*5+1
dado2[i]<-round(num2[i]+0.5) 

suma<-dado1+dado2



if(suma[1]==7 || suma[1]==11){
  gana<-1
}#cierre if

if(suma[1]==2 || suma[1]==3 || suma[1]==12){

    pierde<-1
}#cierre if

if(suma[1]==4){
  pending<-1
}#cierre if 
if(suma[1]==5){
  pending<-2
}#cierre if 
if(suma[1]==6){
  pending<-3
}#cierre if 
if(suma[1]==8){
  pending<-4
}#cierre if 
if(suma[1]==9){
  pending<-5
}#cierre if 
if(suma[1]==10){
  pending<-6
}#cierre if 


  
  }#cierre ciclo for 
  
} #cierre while
