mydata3 <- read.csv("~/ITESO/7mo Semestre/Modelos de Credito/german.data-numeric.csv", header=FALSE)
library(MASS)
x1<-mydata3$V2
x2<-mydata3$V4
ones<-rep(1,length(x1))

y <- 2 - mydata3$V25
x <- matrix(c(ones,x1,x2),nrow=length(mydata3$V2),ncol=3)

m<-t(x)
beta<- (ginv(t(x)%*%x))%*%t(x)%*%y

y.model <- x%*%beta
residuales<- y-y.model

plot(residuales,y.model)

