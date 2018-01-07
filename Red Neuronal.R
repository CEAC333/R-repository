library("neuralnet")

mydata0 <- german.data.numeric

set.seed(1324)

train_s <- sample(1:nrow(mydata0), 700) 
train <- mydata0[train_s,c("V1","V2","V3","V4","V10","V25")]
test <- mydata0[-train_s,c("V1","V2","V3","V4","V10","V25")]

nn<-neuralnet(train$V25 ~ train$V1+train$V2+train$V3+train$V4+train$V10,train,hidden=2,threshold=0.05) #train$V25 es la variable yi, y las demás son las 
#entradas de los nodos, thereshold es la precisión buscada de los errores.

result<-compute(nn,test[,c("V1","V2","V3","V4","V10")])

test$nn<-result$net.result

bad_test <- test[test$V25==2,]
good_test <- test[test$V25==1,]

gh <- hist(good_test$nn,breaks=5,col=rgb(0,0,1,0.5))
breaks <- gh$breaks
bh <- hist(bad_test$nn,breaks=breaks,col=rgb(1,0,0,0.5),add=T)

x <- gh$mids + (0.05)
pureza <- cumsum(gh$counts)/(cumsum(gh$counts) + cumsum(bh$counts))

plot(x=x,y=pureza,main ="Pureza")
lines(x,pureza)

valor_modelo <- gh$mids + (0.05)
eficiencia <- cumsum(gh$counts + bh$counts)/sum(gh$counts + bh$counts)

plot(x=x,y=eficiencia,main ="Eficiencia")
lines(x,eficiencia)