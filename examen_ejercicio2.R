n<-100
w<-numeric(n)
x<-runif(n)

r1<-0
r2<-0.3
r3<-0.75
r4<-1
ob1<-30
ob2<-30
ob3<-40
peso1<-(ob1/n)/(r2-r1)
peso2<-(ob2/n)/(r3-r2)
peso3<-(ob3/n)/(r4-r3)

for (i in 1:30){
  w[i]<-x[i]*(r2-r1)+r1
  w[i]<-w[i]*cos((w[i])^2)
  w[i]<-w[i]/peso1

}

for (i in 31:60) {
  w[i]<-x[i]*(r3-r2)+r2
  w[i]<-w[i]*cos((w[i])^2)
  w[i]<-w[i]/peso2
}
for (i in 61:n) {
  w[i]<-x[i]*(r4-r3)+r3
  w[i]<-w[i]*cos((w[i])^2)
  w[i]<-w[i]/peso3
}

media_estratificado<-mean(w)


comp<-numeric()
y<-numeric()
for (i in 1:100){
  y[i]<-x[i]
 
}
for(i in 101:200){
  comp[i]<-1-x[i-100]
  y[i]<-comp[i]
}
 
media_complementario<-mean(y)

media<-(media_estratificado+media_complementario)/2

#inciso 1

tasa_interes_anual<-media
tasa_interes_mensual<-(tasa_interes_anual/12)

precio_maquina<-5000000

pago_mensual_maquina<-(precio_maquina/60)*(1+tasa_interes_mensual)

costo_maquina<-pago_mensual_maquina*60

#inciso 2 
ene<-numeric()
feb<-numeric()
mar<-numeric()
abr<-numeric()
may<-numeric()
jun<-numeric()
jul<-numeric()
ago<-numeric()
sep<-numeric()
oct<-numeric()
nov<-numeric()
dic<-numeric()
subrutina_ventas<-function(mediav,desve){
  
  mes<-numeric()
  mes<-rnorm(1,mediav,desve)*1.30
  mes
  
}

for(j in 1:1000){
  ene[j]<-subrutina_ventas(16696,3339)
  feb[j]<-subrutina_ventas(12415,2483)
  mar[j]<-subrutina_ventas(27529,5505)
  abr[j]<-subrutina_ventas(23067,4613)
  may[j]<-subrutina_ventas(33799,6759)
  jun[j]<-subrutina_ventas(36211,7242)
  jul[j]<-subrutina_ventas(44401,8880)
  ago[j]<-subrutina_ventas(37828,7565)
  sep[j]<-subrutina_ventas(27135,5427)
  oct[j]<-subrutina_ventas(40377,8075)
  nov[j]<-subrutina_ventas(28706,5741)
  dic[j]<-subrutina_ventas(16145,3229)
}

meanene<-round(mean(ene))
meanfeb<-round(mean(feb))
meanmar<-round(mean(mar))
meanabr<-round(mean(abr))
meanmay<-round(mean(may))
meanjun<-round(mean(jun))
meanjul<-round(mean(jul))
meanago<-round(mean(ago))
meansep<-round(mean(sep))
meanoct<-round(mean(oct))
meannov<-round(mean(nov))
meandic<-round(mean(dic))

ventas_anuales<-meanene+meanfeb+meanmar+meanabr+meanmay+meanjun+meanjul+meanago+meansep+meanoct+meannov+meandic

utilidad_anual<-(ventas_anuales*(2.5*.60))
