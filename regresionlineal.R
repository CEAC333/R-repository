data<-read.table("./data.csv")
xdata<-data$V2
ydata<-(2-data$V25) #dice que los 2 los hagamos 0 (2-1)=1 (2-2)=0

meanx<-mean(xdata)
meany<-mean(ydata)

sumx2<-sum(xdata^2)
sumxy<-sum(xdata*ydata)

nxy<-length(xdata)*meanx*meany

denom<-sumx2-length(xdata)*meanx^2
beta0<-(meany*sumx2-meanx*sumxy)/denom
beta1<-(sumxy-nxy)/denom

y_model<-beta0+beta1*xdata

plot(xdata,ydata,add=T)
lines(xdata,y_model)
