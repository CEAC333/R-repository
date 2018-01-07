library(MASS)

setwd("~/clases/iteso/modelos_de_credito/examples")

print(getwd())

my_file<-paste(getwd(),"/../german_data/german.data-numeric.csv",sep='')

mydata0 <- read.table(my_file, header=FALSE, sep=",")

set.seed(1237819)

mydata <- mydata0[sample(nrow(mydata0), 700), ]

mytest <- mydata0[ !(rownames(mydata0) %in% rownames(mydata)), ]

x1data <-mydata$V2

x2data <-mydata$V4

x3data <-mydata$V10

ydata <-2-mydata$V25

xTx00 <- length(x1data)

xTx01 <- sum(x1data)

xTx10 <- xTx01

xTx02 <- sum(x2data)

xTx20 <- xTx02

xTx11 <- sum(x1data * x1data)

xTx12 <- sum(x1data * x2data)

xTx21 <- xTx12

xTx22 <- sum(x2data * x2data)

xTx33 <- sum(x3data * x3data)

xTx03 <- sum(x3data)

xTx30 <- xTx03

xTx13 <- sum(x1data * x3data)

xTx31 <- xTx13

xTx23 <- sum(x2data * x3data)

xTx32 <- xTx23

XTX <- rbind(c(xTx00, xTx01, xTx02,xTx03),
             
             c(xTx10, xTx11, xTx12,xTx13),
             
             c(xTx20, xTx21, xTx22,xTx23),
             
             c(xTx30, xTx31, xTx32,xTx33))

x0y <- sum(ydata)

x1y <- sum(x1data * ydata)

x2y <- sum(x2data * ydata)

x3y <- sum(x3data * ydata)

XTY <- rbind(c(x0y),
             
             c(x1y),
             
             c(x2y),
             
             c(x3y))

invXTX <- ginv(XTX)

BETA <- invXTX %*% XTY

for (i in 1:nrow(mytest)) {
  
  row <- mytest[i,]
  
  x1 <- row[["V2"]]
  
  x2 <- row[["V4"]]
  
  x3 <- row[["V10"]]
  
  y_model <- BETA[1] + BETA[2] * x1 + BETA[3] * x2 + BETA[4] * x3
  
  mytest[rownames(row),"LinnearR"] <- y_model
  
}

breaks <- c(0.35,0.45,0.55,0.65,0.75,0.85)

bad_test <- mytest[mytest["V25"] == 2,]

good_test <- mytest[mytest["V25"] == 1,]

main <- "Linnear Regression Model"

gbm <- hist(good_test$LinnearR, freq = T, col = rgb(0,0,1,0.5),breaks = breaks, xlim = c(0.35,0.85), xlab = "Linnear Regression value", main = main)

bbm <- hist(bad_test$LinnearR, freq = T, col = rgb(1,0,0,0.5), breaks = breaks, xlim = c(0.35,0.85), add = T)

nbins <- length(breaks)-1

x <- gbm$mids+(0.05) #Tomando el valor máximo del bin

y <- cumsum(gbm$counts)/cumsum(gbm$counts+bbm$counts)

main <- "(Pureza) Fracción de buenos aceptados por corte en el valor de la regresión Lineal"

plot(x,y,type='b',ylab="(Cummulative Good counts)/(Cummulative counts)",xlab="Valor de la regresión Lineal",main=main)

x <- gbm$mids+(0.05) #Tomando el valor máximo del bin

y <- cumsum(gbm$counts+bbm$counts)/sum(gbm$counts+bbm$counts)

main <- "(Eficiencia) Fracción de aceptados por corte en el valor de la regresión Lineal"

plot(x,y,type='b',ylab="(Cummulative counts)/(Total counts)",xlab="Valor de la regresión Lineal",main=main)