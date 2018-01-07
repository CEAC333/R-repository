head<-0
tail<-0
x<-0
dinero<-numeric()
sumadinero<-10

for(i in 1:100){
  dinero[i]<-0
  head<-0
  tail<-0
while(abs(tail-head)!=3){
  
  dinero[i]<-dinero[i]-1
 
    x<-runif(1)
    
    if(x<(1/2)){
      head<-head+1
    } else{
      tail<-tail+1
    }
  } #end while
dinero[i]<-dinero[i]+8
sumadinero<-sumadinero+dinero[i]
} #end for
