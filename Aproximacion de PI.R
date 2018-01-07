#aproximación de PI

##método acierto y fallo

n<-10000
x<-runif(n)
y<-runif(n)
w<- sqrt(x^2+y^2)
contador<-length(w[w<=1])

pi<-(contador/n)*4

##método montecarlo

a<- 0
b<- 1
n<- 10000

funcion<-function(xi){
  y<-sqrt(1-(xi)^2)
  return(y) 
}

aleatorio<-runif(n)
integral<-((b-a)/n)*sum(funcion(a+aleatorio*(b-a)))

pi<-integral*4