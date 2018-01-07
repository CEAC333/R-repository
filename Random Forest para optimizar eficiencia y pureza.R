library("randomForest")

mydata0 <- read.table("german.data-numeric.csv",header = F, sep=',')

set.seed(1324)

train <- sample(1:nrow(mydata0), 700) 
mydata.train <- mydata0[train,]
mydata.test <- mydata0[-train,]

mydata.train$V25 <- as.factor(mydata.train$V25)

efic_04<-c()
pure_08<-c()
inter_04_08<-c()
max_pureza<-0
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
    bh <- hist(bad_test$rf,breaks,col=rgb(1,0,0,0.5))
    
    x <- gh$mids
    pureza <- cumsum(gh$counts)/(cumsum(gh$counts) + cumsum(bh$counts))
    
    valor_modelo <- gh$mids
    eficiencia <- cumsum(gh$counts + bh$counts)/sum(gh$counts + bh$counts)
    
    efic_04<-which(eficiencia>.4)
    pure_08<-wich(pureza>.8)
    inter_04_08<-intersect(efic_04,pure_08)
    pass_condition<-pureza[inter_04_08]
    
    if(max(pass_condition)>max_pureza){
      max_pureza<-max(pass_condition)
      max_nvar<-nvar
      max_ntrees<-ntrees
    }
    
  }#fin ciclos for tarea
}#fin de ciclos for tarea

print( efic_04)
print(pure_08)
print(inter_04_08)
