library("quantmod", lib.loc="~/R/win-library/3.2")
getSymbols("LALAB.MX", from="2014-05-05", to="2016-05-05")

LALA <- as.vector(LALAB.MX[,6])
Rlala<- diff(log(LALA))
dtrlala<-data.frame(Rlala)
Mlala<-mean(Rlala)

shapiro <-shapiro.test(Rlala)$p.value

densi<-kde(Rlala,binned=TRUE)
plot(densi)


random<-rkde(10000,densi,positive=FALSE)
aleatorios<-density(random)
plot(aleatorios)
#hist(Rlala,freq=FALSE)
lines(density(Rlala),na.rm=TRUE)

#densidad<-density(Rlala)
x<-seq(-.1,.1,.001)
target=function(x){
  s<-dkde(x,densi)
  return(s)
  }
plot(x,target(x))

metropolis=function(x,alpha=1){
  y=runif(1,x-alpha,x+alpha)
  if (runif(1)>(target(y)/target(x))) y=x
  return(y)}

Te=10^4
x=rep(.01,Te)
for (t in 2:Te) x[t]=metropolis(x[t-1])

plot(density(x))

x2<-x[(.5*length(x)):length(x)]
plot(density(x2))
lines(density(Rlala),na.rm=TRUE)

xs=seq(-4,4,.001)
xr=target(xs)


#plot(xs,xr)
lines(xs,xr)