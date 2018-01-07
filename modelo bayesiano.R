setwd("~/clases/iteso/examples")

print(getwd())

my_file<-paste(getwd(),"/../german_data/german.data-numeric.csv",sep='')

print(my_file)

mydata <- read.table(my_file, header=FALSE, sep=",")

print(summary(mydata))

good <- subset(mydata, V25==1) # Al parecer en algunas versiones de R se debe escribir "V25"

# chequen que bad tenga 300 obs. y good 700 obs.

bad <- subset(mydata, V25==2)

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

err_var <- function(df,var,nbins) {
  
  min <- min(df[var])
  
  max <- max(df[var])+abs(max(df[var])/10000)
  
  binsize=(max-min)/nbins
  
  breaks <- seq(min,max,by=binsize)
  
  sd_vector = c()
  
  for (i in breaks){
    
    if (i==max) next
    
    entries <- (df[(df[var]>=i & df[var]<i+binsize),])[[var]]
    
    sd_vector <- c(sd_vector, sqrt(length(entries))/length(entries))
    
  }
  
  return(sd_vector)
  
}

freq<-TRUE

# Aqui defino los breaks, con ayuda de la función que definí antes

v10breaks <- breaks_var(mydata,"V10",4)

# Aqui defino el binsize para v10

v10binsize<-(max(mydata["V10"])-min(mydata["V10"]))/4

ylab <- sprintf("(Frequency of persons)/(%.1f units[V10])",v10binsize)

hist(good$V10, breaks=v10breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V10 histogram", xlab="V10",ylab = ylab)

hist(bad$V10, breaks=v10breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

v4breaks <- breaks_var(mydata,"V4",4)

# Aqui defino el binsize para v4

v4binsize<-(max(mydata["V4"])-min(mydata["V4"]))/4

ylab <- sprintf("(Frequency of persons)/(%.1f units[V4])",v4binsize)

hist(good$V4, breaks=v4breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V4 histogram", xlab="V4",ylab = ylab)

hist(bad$V4, breaks=v4breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

# Aqui defino los breaks, con ayuda de la función que definí antes

v2breaks <- breaks_var(mydata,"V2",4)

# Aqui defino el binsize para v2

v2binsize<-(max(mydata["V2"])-min(mydata["V2"]))/4

ylab <- sprintf("(Frequency of persons)/(%.1f units[V2])",v2binsize)

hist(good$V2, breaks=v2breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V2 histogram", xlab="V2",ylab = ylab)

hist(bad$V2, breaks=v2breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red histogram

freq<-FALSE

bv10<-hist(bad$V10, breaks=v10breaks, col=rgb(1,0,0,0.5),freq = freq, main = "V10 density", xlab="V10") # Red density

gv10<-hist(good$V10, breaks=v10breaks,col=rgb(0,0,1,0.5),freq = freq, add=TRUE)

gv4<-hist(good$V4, breaks=v4breaks, col=rgb(0,0,1,0.5),freq = freq, main = "V4 density", xlab="V4")

bv4<-hist(bad$V4, breaks=v4breaks, col=rgb(1,0,0,0.5),freq = freq, add=TRUE) # Red density

bv2<-hist(bad$V2, breaks=v2breaks, col=rgb(1,0,0,0.5),freq = freq, main = "V2 density", xlab="V2") # Red density

gv2<-hist(good$V2, breaks=v2breaks, col=rgb(0,0,1,0.5),freq = freq, add=TRUE)

x <- gv2$mids

y <- bv2$density/gv2$density

delta <- err_var(bad,2,4)

barwidth <- 0.5 #Tamaño de barritas al final de la linea de incertidumbre,

# si tienes dudas de qué hace, ponlo igual a 0

plot(gv2$mids,bv2$density/gv2$density,type='b',ylab="(Bad density)/(Good density)",xlab="V2 value")

segments(x,y-delta,x,y+delta)

segments(x-barwidth,y+delta,x+barwidth,y+delta)

segments(x-barwidth,y-delta,x+barwidth,y-delta)

x <- gv4$mids

y <- bv4$density/gv4$density

delta <- err_var(bad,4,4)

barwidth <- 1 #Tamaño de barritas al final de la linea de incertidumbre,

# si tienes dudas de qué hace, ponlo igual a 0

plot(gv4$mids,bv4$density/gv4$density,type='b',ylab="(Bad density)/(Good density)",xlab="V4 value")

segments(x,y-delta,x,y+delta)

segments(x-barwidth,y+delta,x+barwidth,y+delta)

segments(x-barwidth,y-delta,x+barwidth,y-delta)

x <- gv10$mids

y <- bv10$density/gv10$density

delta <- err_var(bad,"V10",4)

barwidth <- 0.33 #Tamaño de barritas al final de la linea de incertidumbre,

# si tienes dudas de qué hace, ponlo igual a 0

plot(gv10$mids,bv10$density/gv10$density,type='b',ylab="(Bad density)/(Good density)",xlab="V10 value")

segments(x,y-delta,x,y+delta)

segments(x-barwidth,y+delta,x+barwidth,y+delta)

segments(x-barwidth,y-delta,x+barwidth,y-delta)