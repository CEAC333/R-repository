
n<-1000
numeros<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        32,33,34,35,36)
proba<-c(0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,
        0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,
        0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,
        0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,0.027027027,
        0.027027027,0.027027027,0.027027027,0.027027027,0.027027027)
numero_ganador<-sample(numeros, n, replace = TRUE, prob = proba)

negro<-0
rojo<-0
verde<-0
color_ganador<-numeric()

for(i in 1:n){
  
if(numero_ganador[i]==2 || numero_ganador[i]==4 || numero_ganador[i]==6 || numero_ganador[i]==8 || 
     numero_ganador[i]==10 || numero_ganador[i]==11 || numero_ganador[i]==13 || numero_ganador[i]==15 || 
     numero_ganador[i]==17 || numero_ganador[i]==20 || numero_ganador[i]==22 || numero_ganador[i]==24 || 
     numero_ganador[i]==26 || numero_ganador[i]==28 || numero_ganador[i]==29 || numero_ganador[i]==31 || 
     numero_ganador[i]==33 || numero_ganador[i]==35){
  negro<-negro+1
  color_ganador[i]<-40
 
}

if(numero_ganador[i]==1 || numero_ganador[i]==3 || numero_ganador[i]==5 || numero_ganador[i]==7 || 
     numero_ganador[i]==9|| numero_ganador[i]==12 || numero_ganador[i]==14 || numero_ganador[i]==16 || 
     numero_ganador[i]==18|| numero_ganador[i]==19 || numero_ganador[i]==21 || numero_ganador[i]==23 || 
     numero_ganador[i]==25 || numero_ganador[i]==27 || numero_ganador[i]==30 || numero_ganador[i]==32 || 
     numero_ganador[i]==34 || numero_ganador[i]==36){
  rojo<-rojo+1
  color_ganador[i]<-50
}

if(numero_ganador[i]==0){
  verde<-verde+1
  color_ganador[i]<-60
}
}



