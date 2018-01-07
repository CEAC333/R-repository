head<-0
tail<-0
x<-0
n<-100
dinero<-numeric()
num_tiros<-numeric()
sumadinero<-10
menor_5<-0
mayor_10<-0
tiro_num<-0
valoresp_100tiros<-0
dinero_100tiros<-numeric()
nume_tiros<-numeric()

for(i in 1:n){
  dinero[i]<-0
  num_tiros[i]<-0
  head<-0
  tail<-0
while(abs(tail-head)!=3){
  
  dinero[i]<-dinero[i]-1
  num_tiros[i]<-num_tiros[i]+1
  
    x<-runif(1)
    
    if(x<(1/2)){
      head<-head+1
    } else{
      tail<-tail+1
    }
  
  } #end while
   
   if(num_tiros[i]<5){
     menor_5<-menor_5+1
   }
   if(num_tiros[i]>10){
     mayor_10<-mayor_10+1
   }
tiro_num<-tiro_num+num_tiros[i]
dinero[i]<-dinero[i]+8
sumadinero<-sumadinero+dinero[i]

   if(tiro_num<101){
    dinero_100tiros[i]<-dinero[i]
    nume_tiros[i]<-num_tiros[i]
   }
   
} #end ciclo for 

prom_tiros<-mean(num_tiros)
desv_tiros<-sd(num_tiros)
histograma<-hist(num_tiros)
prob_menor_5<-menor_5/n
prob_mayor_10<-mayor_10/n
valoresp_100tiros<-mean(dinero_100tiros)
valor_masprobable<-mean(nume_tiros)
