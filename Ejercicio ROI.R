inver<-numeric()
inver[1]<-runif(1, min=-195000, max=-150000)
inver[2]<-runif(1, min=-538000, max=-335000)
inver[3]<-0
inver[4]<-0
inver[5]<-0
inver[6]<-0
costos<-numeric()
costos[1]<-runif(1, min=-2700, max=-1500)
costos[2]<-runif(1, min=-2800, max=-1400)
costos[3]<-runif(1, min=-13260, max=-6000)
costos[4]<-runif(1, min=-12760, max=-5025)
costos[5]<-runif(1, min=-29820, max=-14280)
costos[6]<-runif(1, min=-24025, max=-15500)
ROI1<-numeric()
ROI1[1]<-0
ROI1[2]<-rnorm(1,-10000,2500)
ROI1[3]<-runif(1, min=-2000, max=6000)
ROI1[4]<-rnorm(1,25000,3400)
ROI1[5]<-rnorm(1,50000,1500)
ROI1[6]<-0
ROI2<-numeric()
ROI2[1]<-0
ROI2[2]<-runif(1, min=-60000, max=-16000)
ROI2[3]<-rnorm(1,10000,2500)
ROI2[4]<-runif(1, min=5000, max=27800)
ROI2[5]<-rnorm(1,100000,37000)
ROI2[6]<-0
tasa1<-c(.09,.19,.25,.23,.14,.1)
tasa2<-c(.07,.13,.32,.31,.12,.05)
tasa3<-c(.06,.17,.28,.29,.12,.08)
tasa4<-c(.11,.18,.27,.03,.09,.05)
numeros<-numeric()
for (i in 1:6){
  numeros[i]<-i
}
tasacobrada<-numeric()
tasa<-numeric()
tasa[1]<-sample(numeros,1,replace = FALSE, prob = tasa1)
tasa[2]<-sample(numeros,1,replace = FALSE, prob = tasa2)
tasa[3]<-sample(numeros,1,replace = FALSE, prob = tasa3)
tasa[4]<-sample(numeros,1,replace = FALSE, prob = tasa4)

for (i in 1:4){
if (tasa[i]==1){
  tasacobrada[i]<-runif(1,min=.02,max=.0254)
}
if (tasa[i]==2){
  tasacobrada[i]<-runif(1,min=.0254, max=.028)
}
if (tasa[i]==3){
  tasacobrada[i]<-runif(1,min=.028, max=.032)
}
if (tasa[i]==4){
  tasacobrada[i]<-runif(1,min=.032, max=.0355)
}
if (tasa[i]==5){
  tasacobrada[i]<-runif(1,min=.0355, max=.04)
}
if (tasa[i]==6){
  tasacobrada[i]<-runif(1,min=.04, max=.0455)
}
}
tasacobrada[5]<-1
tasacobrada[6]<-1
caja<-0
i<-0
for(i in 1:1){
  caja<-ROI1[i]+ROI2[i]+inver[i]+costos[i]+caja
  
  if(caja<0){
  caja<-caja*(1+tasacobrada[i])
}
}






