library(MASS)
data<-read.table("./data.csv")
set.seed(1232)
data<-data
x1data<-data$V2
x2data<-data$V4
ydata<-(2-data$V25) #dice que los 2 los hagamos 0 (2-1)=1 (2-2)=0

xtx00<-length(x1data)
xtx01<-sum(x1data)
xtx10<-xtx01
xtx02<-sum(x2data)
xtx20<-xtx02
xtx11<-sum(x1data*x1data)
xtx12<-sum(x1data*x2data)
xtx21<-xtx12
xtx22<-sum(x2data*x2data)

yx0<-sum(ydata)
yx1<-sum(x1data*ydata)
yx2<-sum(x2data*ydata)

xtx<-rbind(c(xtx00,xtx01,xtx02),
           c(xtx10,xtx11,xtx12),
           c(xtx20,xtx21,xtx22))  

yx<-rbind(c(yx0),c(yx1),c(yx2))

print(xtx)

invxtx<-ginv(xtx) 

betas<-invxtx %*% yx

testx1<-mytest$V2
testx2<-mytest$V4

y_modelo<-betas[1]+betas[2]*testx1+betas[3]*testx2       

breaks<-c(0.3,0.41,0.52,0.63,0.74,0.85)
hist<-(y_modelo,breaks=breaks)



