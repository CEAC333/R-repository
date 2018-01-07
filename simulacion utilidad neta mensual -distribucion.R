utilidad_neta_sem<-numeric()
for(j in 1:1000){
ventas<-numeric()
surtir<-0
ventas<-rbinom(361,6,0.5)
prom_ventas<-mean(ventas)
prom_ventas_sem<-prom_ventas*7
prom_ventas_men<-prom_ventas*30
des_est<-sd(ventas)
des_est_sem<-des_est*7
des_est_men<-des_est*30
inv_max_sem<-floor(1.1*prom_ventas_sem+3*des_est_sem)
inv_inicial<-100
inv<-numeric()
orden<-numeric()
tardanza<-3 #Dias que tarda en llegar el pedido
bandera<-0
fecha_entrega<-0
ventas_logradas<-0
ventas_perdidas<-0
costo_inv<-0
costo_ventas<-0

for(i in 2:361) {
  inv[1]<-inv_inicial
  inv[i]<-inv[i-1]
  if(inv[i]>=ventas[i]){
   ventas_logradas<-ventas_logradas+ventas[i]
   inv[i]<-inv[i]-ventas[i]
  } else {
    ventas_logradas<-ventas_logradas+inv[i]
    ventas_perdidas<-(ventas[i]-inv[i])+ventas_perdidas
    inv[i]<-0
  }
  
    
  if(inv[i]<=(prom_ventas*3)&bandera!=1){
    surtir<-surtir+1
    bandera<-1
    fecha_entrega<-i+tardanza
    orden[surtir]<-inv_max_sem-inv[i]
  }
    if(i==fecha_entrega){
    bandera<-0
    inv[i]<-orden[surtir]+inv[i]
    }
costo_inv<-inv[i]+costo_inv
  
   }
costo_ventas<-(ventas_perdidas*10)
costo_orden<-sum(orden)*500+(50*surtir)

precio_venta<-numeric()
ganancia_total<-0
ganancia<-0


for(x in 1:ventas_logradas){
precio_venta[x]<-1+(runif(1, min=.10, max=.20))
ganancia<-500*precio_venta[x]
ganancia_total<-ganancia_total+ganancia
}

utilidad_neta_sem[j]<-ganancia_total-costo_inv-costo_orden-costo_ventas
}
promedio_utilidad_anual_prom_max_sem<-mean(utilidad_neta_sem)
histogrma<-hist(utilidad_neta_sem)
mean(orden)



utilidad_neta_men<-numeric()
for(j in 1:1000){
  ventas<-numeric()
  surtir<-0
  ventas<-rbinom(361,6,0.5)
  prom_ventas<-mean(ventas)
  prom_ventas_sem<-prom_ventas*7
  prom_ventas_men<-prom_ventas*30
  des_est<-sd(ventas)
  des_est_sem<-des_est*7
  des_est_men<-des_est*30
  inv_max_sem<-floor(1.1*prom_ventas_sem+3*des_est_sem)
  inv_max_men<-floor(1.1*prom_ventas_men+3*des_est_men)
  inv_inicial<-100
  inv<-numeric()
  orden<-numeric()
  tardanza<-3 #Dias que tarda en llegar el pedido
  bandera<-0
  fecha_entrega<-0
  ventas_logradas<-0
  ventas_perdidas<-0
  costo_inv<-0
  costo_ventas<-0
  
  for(i in 2:361) {
    inv[1]<-inv_inicial
    inv[i]<-inv[i-1]
    if(inv[i]>=ventas[i]){
      ventas_logradas<-ventas_logradas+ventas[i]
      inv[i]<-inv[i]-ventas[i]
    } else {
      ventas_logradas<-ventas_logradas+inv[i]
      ventas_perdidas<-(ventas[i]-inv[i])+ventas_perdidas
      inv[i]<-0
    }
    
    
    if(inv[i]<=(prom_ventas*3)&bandera!=1){
      surtir<-surtir+1
      bandera<-1
      fecha_entrega<-i+tardanza
      orden[surtir]<-inv_max_men-inv[i]
    }
    if(i==fecha_entrega){
      bandera<-0
      inv[i]<-orden[surtir]+inv[i]
    }
    costo_inv<-inv[i]+costo_inv
    
  }
  costo_ventas<-(ventas_perdidas*10)
  costo_orden<-sum(orden)*500+(50*surtir)
  
  precio_venta<-numeric()
  ganancia_total<-0
  ganancia<-0
  
  
  for(x in 1:ventas_logradas){
    precio_venta[x]<-1+(runif(1, min=.10, max=.20))
    ganancia<-500*precio_venta[x]
    ganancia_total<-ganancia_total+ganancia
  }
  
  utilidad_neta_men[j]<-ganancia_total-costo_inv-costo_orden-costo_ventas
}
promedio_utilidad_anual_prom_max_men<-mean(utilidad_neta_men)
histogrma<-hist(utilidad_neta_men)

