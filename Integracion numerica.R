#integración numérica

##método trapecios

a<- 0
b<- 2
n<- 1000
# f= x^3 + 2
deltax<-(b-a)/n

x<-c()

for(i in 1:(n-1)){
  x[i]<-a+i*deltax
}

funcion<-function(xi){
  y<-(xi)^3+2
  return(y) 
}

integral<-((b-a)/n)*((funcion(a)+funcion(b)/2)+sum(funcion(x)))

##método montecarlo

aleatorio<-runif(n)
integral<-((b-a)/n)*sum(funcion(a+aleatorio*(b-a)))