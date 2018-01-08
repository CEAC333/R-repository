potencia<-5
resultado<-0

funcion<-function(potencia,n){
  for (i in 0:n){
    resultad<-(potencia^i)/(factorial(i))
    resultado<-resultad+resultado
  }
  return(resultado)
}
n<-100
valor<-funcion(potencia,n)