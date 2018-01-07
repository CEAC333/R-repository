#Metdodo Box-Muller

n<-190
ale1_n<-numeric()

de<-14931.86
mi<-48807

for(i in 1:n){
  
  ale1<-runif(1)
  ale2<-runif(1)
  
  ale1_n[2*i-1]<-(sqrt(-2*log(ale1))*cos(2*pi*ale2))*de+mi
  ale1_n[2*i]<-(sqrt(-2*log(ale1))*sin(2*pi*ale2))*de+mi
  
}
num_cont_menos50mil<-0
sum_cont_menos50mil<-0
num_cont_mas50mil<-0
sum_cont_mas50mil<-0
hist(ale1_n)
for(i in 1:(2*n)){
  if(ale1_n[i]<=50000){
    num_cont_menos50mil<-num_cont_menos50mil+1
    sum_cont_menos50mil<-sum_cont_menos50mil+ale1_n[i]
      }
  else {
    num_cont_mas50mil<-num_cont_mas50mil+1
    sum_cont_mas50mil<-sum_cont_mas50mil+ale1_n[i]
  }
}
ingreso<-(sum_cont_menos50mil*.05)+(sum_cont_mas50mil*.03)
comision<-(num_cont_menos50mil+num_cont_mas50mil)*300
investigacion<-round((sum_cont_menos50mil+sum_cont_mas50mil)/500000)*1000
utilidad<-ingreso-comision-investigacion
ingreso
comision
investigacion
utilidad
