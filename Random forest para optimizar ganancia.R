library("randomForest")

mydata0 <- read.table("german.data-numeric.csv",header = F, sep=',')

set.seed(1324)

train <- sample(1:nrow(mydata0), 700) 
mydata.train <- mydata0[train,]
mydata.test <- mydata0[-train,]

mydata.train$V25 <- as.factor(mydata.train$V25)

maxganancia<- -1000000 #valor de prueba inicial 
max_nvar<-0
max_ntrees<-0

for(nvar in 8:12){
  for(ntrees in seq(from=300,to=600,50)){
    
   rf <- randomForest(V25 ~ ., data=mydata.train,ntree = ntrees,mtry=nvar)#genera ntrees arboles con


  for (i in 1:nrow(mydata.test)){
   mydata.test[i,'rf'] <- predict(rf, mydata.test[i,], type="prob")[2]
  }


  bad_test <- mydata.test[mydata.test$V25==2,]
  good_test <- mydata.test[mydata.test$V25==1,]

  breaks <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  gh <- hist(good_test$rf,breaks,col=rgb(0,0,1,0.5))
  bh <- hist(bad_test$rf,breaks,col=rgb(1,0,0,0.5),add=T)

  x <- gh$mids
  pureza <- cumsum(gh$counts)/(cumsum(gh$counts) + cumsum(bh$counts))

  valor_modelo <- gh$mids
  eficiencia <- cumsum(gh$counts + bh$counts)/sum(gh$counts + bh$counts)


  ingreso <- (1000*eficiencia)*6500*pureza
  impago <- (1000*eficiencia)*(7000)*(1-pureza)

  ganancia = (-2000000 + ingreso - impago)/1000 #el entre mil es solo para reducir la escala de los datos
  
  if(max(ganancia)>maxganancia){
    maxganancia<-max(ganancia)
    max_nvar<-nvar
    max_ntrees<-ntrees 
  }

}#fin ciclos for tarea
}#fin de ciclos for tarea

print( maxganancia)
print(max_nvar)
print(max_ntrees)