mydata<-read.table("./german.data-numeric.csv",sep=",")

xdata<-mydata$V4

ydata<- 2-mydata$V25

meanx<-mean(xdata)

meany<-mean(ydata)

sumx2<-sum(xdata^2)

sumxy<-sum(xdata*ydata)

nxy<-length(xdata)*meanx*meany

denom<-sumx2-length(xdata)*meanx^2

beta0<-(meany*sumx2-meanx*sumxy)/denom

beta1<-(sumxy-length(xdata)*meanx*meany)/denom

y_model<-beta0+beta1*xdata

plot(xdata,ydata,add=T)

lines(xdata,y_model)