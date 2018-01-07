#regresion logistica
set.seed(1232)
mydata<-data[sample(nrow(data),700),]
mytest<-data[!(rownames(data) %in% rownames(mydata)),]

x1<-mydata$V2
x2<-mydata$V4
y<-2-mydata$V25

train<-data.frame(y,x1,x2) # data frame de entrenamiento

logistic<-glm(y ~ x1 + x2,data=train,family=binomial )

x1<- mytest$V2
x2<- mytest$V4
y<-2-mytest$V25

test<-data.frame(y,x1,x2)

mytest["logistic"]<-predict(logistic,newdata=test,type="response")

# estamos comparando las diferencias entre el GLM o hacer la regresion lineal a mano y ajustarlo a "p"
beta0<-0.88724
beta1<--0.00765
beta2<--0.008

for(i in 1:nrow(mytest)){
  row<-mytest[i,]
  x1<-mytest[i,"V2"]
  x2<-mytest[i,"V4"]
  y_modelo<-1/(1+exp(-(beta0+beta1*x1+beta2*x2)))
  mytest[rownames(row),"manual_log"]<-y_modelo
}

good_test<-mytest[mytest["V25"]==1,]
bad_test<-mytest[mytest["V25"]==2,]

breaks<-c(0.28,0.39,0.50,0.61,0.72,0.83)
gh<-hist(good_test$logistic,breaks=breaks)
bh<-hist(bad_test$logistic,breaks=breaks)

x<-gh$mids
y<-cumsum(gh$counts)/(cumsum(gh$counts)+cumsum(bh$counts))

plot(x=x,y=y,main="pureza")
lines(x,y)

x<-gh$mids
y<-cumsum(gh$counts+bh$counts)/sum(gh$counts+bh$counts)

plot(x=x,y=y,main="eficiencia")
lines(x,y)



