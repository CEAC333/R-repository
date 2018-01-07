dia<-numeric()
x<-numeric()
diaa<-numeric()
venta<-numeric()
u<-numeric()
precio<-numeric()
v<-numeric()

for(i in 1:25){
 x[i]<-runif(1)
 dia[i]<-x[i]*24+1 
 diaa[i]<-round(dia[i]+0.5) 
 
  if(diaa[i]<=4){
    venta[i]<-2
  }
  if(diaa[i]<=11 & diaa[i]>4){
    venta[i]<-3
  }
  if(diaa[i]<=19 & diaa[i]>11){
   venta[i]<-4
  }
  if(diaa[i]<=24 & diaa[i]>19){
   venta[i]<-5
  }
  if(diaa[i]==25){
   venta[i]<-6
  }
   
  ut_tot<-0
  for(k in 1:venta[i]){
   precio[k]<-runif(1)
    if(precio[k]<.5){
     utilidad<-2000 
    }
    if(precio[k]>=.5 & precio[k]<.7){
     utilidad<-5000
    }
    if(precio[k]>=.7){
     utilidad<-1500
    }
   ut_tot<-ut_tot+utilidad 
  }#cierre segundo for
 v[i]<-venta[i]
 u[i]<-ut_tot
}#cierre primer for 
venta
diaa
u
v
mean(u)
mean(v)



