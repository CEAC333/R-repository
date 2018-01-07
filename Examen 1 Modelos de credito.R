#setwd("~/clases/iteso/examples")

#print(getwd())

#my_file<-paste(getwd(),"/../exam1/exam.csv",sep='')

#print(my_file)

#mydata <- read.table(my_file, header=TRUE, sep=",")

#print(summary(mydata))

library("randomForest")

mydata<-exam

good <- subset(mydata, SeriousDlqin2yrs==0)

bad <- subset(mydata, SeriousDlqin2yrs==1)

# Esta es la función que define los breaks, y por lo tanto el tamaño de los bins.

# Dentro se define el binsize

breaks_var <- function(df,var,nbins) {
  
  min <- min(df[var])
  
  max <- max(df[var])+abs(max(df[var])/10000) #Le sumo una dezmilésima del máximo
  
  # para incluir a todos los valores
  
  binsize=(max-min)/nbins
  
  breaks <- seq(min,max,by=binsize)
  
  return(breaks)
  
}

freq<-TRUE

# Aqui defino los breaks, con ayuda de la función que definí antes

V1breaks <- breaks_var(mydata,"RevolvingUtilizationOfUnsecuredLines",10)

# Aqui defino el binsize

V1binsize<-(max(mydata["RevolvingUtilizationOfUnsecuredLines"])-min(mydata["RevolvingUtilizationOfUnsecuredLines"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[RevolvingUtilizationOfUnsecuredLines])",V1binsize)

hist(good$RevolvingUtilizationOfUnsecuredLines, breaks=V1breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V1 histogram", xlab="V1",ylab = ylab)

hist(bad$RevolvingUtilizationOfUnsecuredLines, breaks=V1breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V2breaks <- breaks_var(mydata,"age",10)

# Aqui defino el binsize

V2binsize<-(max(mydata["age"])-min(mydata["age"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[age])",V2binsize)

hist(good$age, breaks=V2breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V2 histogram", xlab="V2",ylab = ylab)

hist(bad$age, breaks=V2breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V3breaks <- breaks_var(mydata,"NumberOfTime30.59DaysPastDueNotWorse",10)

# Aqui defino el binsize

V3binsize<-(max(mydata["NumberOfTime30.59DaysPastDueNotWorse"])-min(mydata["NumberOfTime30.59DaysPastDueNotWorse"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberOfTime30.59DaysPastDueNotWorse])",V3binsize)

hist(good$NumberOfTime30.59DaysPastDueNotWorse, breaks=V3breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V3 histogram", xlab="V3",ylab = ylab)

hist(bad$NumberOfTime30.59DaysPastDueNotWorse, breaks=V3breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V4breaks <- breaks_var(mydata,"DebtRatio",10)

# Aqui defino el binsize

V4binsize<-(max(mydata["DebtRatio"])-min(mydata["DebtRatio"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[DebtRatio])",V4binsize)

hist(good$DebtRatio, breaks=V4breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V4 histogram", xlab="V4",ylab = ylab)

hist(bad$DebtRatio, breaks=V4breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V5breaks <- breaks_var(mydata,"MonthlyIncome",10)

# Aqui defino el binsize

V5binsize<-(max(mydata["MonthlyIncome"])-min(mydata["MonthlyIncome"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[MonthlyIncome])",V5binsize)

hist(good$MonthlyIncome, breaks=V5breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V5 histogram", xlab="V5",ylab = ylab)

hist(bad$MonthlyIncome, breaks=V5breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V6breaks <- breaks_var(mydata,"NumberOfOpenCreditLinesAndLoans",10)

# Aqui defino el binsize

V6binsize<-(max(mydata["NumberOfOpenCreditLinesAndLoans"])-min(mydata["NumberOfOpenCreditLinesAndLoans"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberOfOpenCreditLinesAndLoans])",V6binsize)

hist(good$NumberOfOpenCreditLinesAndLoans, breaks=V6breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V6 histogram", xlab="V6",ylab = ylab)

hist(bad$NumberOfOpenCreditLinesAndLoans, breaks=V6breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V7breaks <- breaks_var(mydata,"NumberOfTimes90DaysLate",10)

# Aqui defino el binsize

V7binsize<-(max(mydata["NumberOfTimes90DaysLate"])-min(mydata["NumberOfTimes90DaysLate"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberOfTimes90DaysLate])",V7binsize)

hist(good$NumberOfTimes90DaysLate, breaks=V7breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V7 histogram", xlab="V7",ylab = ylab)

hist(bad$NumberOfTimes90DaysLate, breaks=V7breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V8breaks <- breaks_var(mydata,"NumberRealEstateLoansOrLines",10)

# Aqui defino el binsize

V8binsize<-(max(mydata["NumberRealEstateLoansOrLines"])-min(mydata["NumberRealEstateLoansOrLines"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberRealEstateLoansOrLines])",V8binsize)

hist(good$NumberRealEstateLoansOrLines, breaks=V8breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V8 histogram", xlab="V8",ylab = ylab)

hist(bad$NumberRealEstateLoansOrLines, breaks=V8breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V9breaks <- breaks_var(mydata,"NumberOfTime60.89DaysPastDueNotWorse",10)

# Aqui defino el binsize

V9binsize<-(max(mydata["NumberOfTime60.89DaysPastDueNotWorse"])-min(mydata["NumberOfTime60.89DaysPastDueNotWorse"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberOfTime60.89DaysPastDueNotWorse])",V9binsize)

hist(good$NumberOfTime60.89DaysPastDueNotWorse, breaks=V9breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V9 histogram", xlab="V9",ylab = ylab)

hist(bad$NumberOfTime60.89DaysPastDueNotWorse, breaks=V9breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

V10breaks <- breaks_var(mydata,"NumberOfDependents",10)

# Aqui defino el binsize

V10binsize<-(max(mydata["NumberOfDependents"])-min(mydata["NumberOfDependents"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[NumberOfDependents])",V10binsize)

hist(good$NumberOfDependents, breaks=V10breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V10 histogram", xlab="V10",ylab = ylab)

hist(bad$NumberOfDependents, breaks=V10breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

####################################################################

mydata0 <- exam

set.seed(1324)

train <- sample(1:nrow(mydata0), 1260)

mydata.train <- mydata0[train,]

mydata.test <- mydata0[-train,]

mydata.train$SeriousDlqin2yrs <- as.factor(mydata.train$SeriousDlqin2yrs)

rf <- randomForest(SeriousDlqin2yrs ~ ., data=mydata.train,ntree = 450,mtry=5)

for (i in 1:nrow(mydata.test)){
  
  mydata.test[i,'rf'] <- predict(rf, mydata.test[i,], type="prob")[2]
  
}

bad_test <- mydata.test[mydata.test$SeriousDlqin2yrs==1,]

good_test <- mydata.test[mydata.test$SeriousDlqin2yrs==0,]

rfbreaks <- breaks_var(mydata.test,"rf",10)

rfbinsize<-(max(mydata.test["rf"])-min(mydata.test["rf"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[rf])",rfbinsize)

gh <- hist(good_test$rf, breaks=rfbreaks, col=rgb(0,0,1,0.5),freq = freq, main = "rf histogram", xlab="rf",ylab = ylab)

bh <- hist(bad_test$rf, breaks=rfbreaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

x <- gh$mids

pureza <- cumsum(gh$counts)/(cumsum(gh$counts) + cumsum(bh$counts))

plot(x=x,y=pureza,main ="Pureza")

lines(x,pureza)

valor_modelo <- gh$mids

eficiencia <- cumsum(gh$counts + bh$counts)/sum(gh$counts + bh$counts)

plot(x=x,y=eficiencia,main ="Eficiencia")

lines(x,eficiencia)

pureza_bin <- gh$counts/(gh$counts + bh$counts)

plot(x=x,y=pureza_bin,main ="Pureza por bin")

lines(x,pureza_bin)

eficiencia_bin <- (gh$counts + bh$counts)/sum(gh$counts + bh$counts)

plot(x=x,y=eficiencia_bin,main ="Eficiencia por bin")

lines(x,eficiencia_bin)

gasto_anual_prom<-2300000

num_pros_anual<-1200

credito_prom_anual<-49283.33

int_prom_anual<-.1256

impago_prom_anual<-3116.67

ingreso <- (num_pros_anual*eficiencia)*(credito_prom_anual*int_prom_anual)*pureza

impago <- (num_pros_anual*eficiencia)*(impago_prom_anual)*(1-pureza)

ganancia = (-gasto_anual_prom + ingreso - impago)

main <- "Ganancia por corte en el modelo RF"

plot(x,ganancia, main = main)

max_ganacia<-max(ganancia)

print(max_ganacia)

corte_max_ganacia<-x[which.max(ganacia)]

print(corte_max_ganacia)

#############################################################################

mydata.train$SeriousDlqin2yrs <- as.factor(mydata.train$SeriousDlqin2yrs)

maxganancia<- -1000000 #valor de prueba inicial

max_nvar<-0

max_ntrees<-0

for(nvar in 7:10){
  
  for(ntrees in seq(from=700,to=900,50)){
    
    rf <- randomForest(SeriousDlqin2yrs ~ ., data=mydata.train,ntree = ntrees,mtry=nva)
    
    for (i in 1:nrow(mydata.test)){
      
      mydata.test[i,'rf'] <- predict(rf, mydata.test[i,], type="prob")[2]
      
    }
    
    bad_test <- mydata.test[mydata.test$SeriousDlqin2yrs==1,]
    
    good_test <- mydata.test[mydata.test$SeriousDlqin2yrs==0,]
    
    rfbreaks <- breaks_var(mydata.test,"rf",10)
    
    rfbinsize<-(max(mydata.test["rf"])-min(mydata.test["rf"]))/10
    
    ylab <- sprintf("(Frequency of persons)/(%.1f units[rf])",rfbinsize)
    
    gh <- hist(good_test$rf, breaks=rfbreaks, col=rgb(0,0,1,0.5),freq = freq, main = "rf histogram", xlab="rf",ylab = ylab)
    
    bh <- hist(bad_test$rf, breaks=rfbreaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram
    
    x <- gh$mids
    
    pureza <- cumsum(gh$counts)/(cumsum(gh$counts) + cumsum(bh$counts))
    
    valor_modelo <- gh$mids
    
    eficiencia <- cumsum(gh$counts + bh$counts)/sum(gh$counts + bh$counts)
    
    gasto_anual_prom<-2300000
    
    num_pros_anual<-1200
    
    credito_prom_anual<-49283.33
    
    int_prom_anual<-.1256
    
    impago_prom_anual<-3116.67
    
    ingreso <- (num_pros_anual*eficiencia)*(credito_prom_anual*int_prom_anual)*pureza
    
    impago <- (num_pros_anual*eficiencia)*(impago_prom_anual)*(1-pureza)
    
    ganancia = (-gasto_anual_prom + ingreso - impago)
    
    if(max(ganancia)>maxganancia){
      
      maxganancia<-max(ganancia)
      
      max_nvar<-nvar
      
      max_ntrees<-ntrees
      
    }# cierre if
    
  } #fin cliclo for
  
} #fin ciclo for

print(maxganancia)

print(max_nvar)

print(max_ntrees)

#######################################################################

mydata.train$SeriousDlqin2yrs <- as.factor(mydata.train$SeriousDlqin2yrs)

rf <- randomForest(SeriousDlqin2yrs ~ ., data=mydata.train,ntree = max_ntrees,mtry= max_nvar)

for (i in 1:nrow(mydata.test)){
  
  mydata.test[i,'rf'] <- predict(rf, mydata.test[i,], type="prob")[2]
  
}

bad_test <- mydata.test[mydata.test$SeriousDlqin2yrs==1,]

good_test <- mydata.test[mydata.test$SeriousDlqin2yrs==0,]

rfbreaks <- breaks_var(mydata.test,"rf",10)

rfbinsize<-(max(mydata.test["rf"])-min(mydata.test["rf"]))/10

ylab <- sprintf("(Frequency of persons)/(%.1f units[rf])",rfbinsize)

gh <- hist(good_test$rf, breaks=rfbreaks, col=rgb(0,0,1,0.5),freq = freq, main = "rf histogram", xlab="rf",ylab = ylab)

bh <- hist(bad_test$rf, breaks=rfbreaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

pureza_bin <- gh$counts/(gh$counts + bh$counts)

eficiencia_bin <- (gh$counts + bh$counts)/sum(gh$counts + bh$counts)

cpa_bin <- c(120000,90000,90000,50000,50000,50000,40000,20000,20000,20000)

ipa_bin <- c(0.06,0.12,0.12,0.24,0.24,0.48,0.48,0.65,0.65,0.65)

impa_bin <- c(18000,14000,10000,7000,7000,7000,6000,6000,6000,6000)

ingreso_bin <- (num_pros_anual *eficiencia_bin)*(cpa_bin*ipa_bin)*pureza_bin

impago_bin <- (num_pros_anual *eficiencia_bin)*impa_bin*(1-pureza_bin)

ganancia_bin<-(-gasto_anual_prom + ingreso_bin - impago_bin)

main <- "Ganancia por bin por corte en el modelo RF"

plot(x,ganancia_bin, main = main)

max_ganacia_bin<-max(ganancia_bin)

print(max_ganacia_bin)

corte_max_ganacia_bin<-x[which.max(ganancia_bin)]

print(corte_max_ganacia_bin)