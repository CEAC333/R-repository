#integración 
##método trapecios



trapecios<-function(limiteinf,limitesup,numtrapecios){
a<-limiteinf
b<-limitesup
n<-numtrapecios
deltax<-(b-a)/n
x<-seq(a+deltax,b-deltax,deltax)

# en esta función el usuario debe ingresar la función que desea integrar, por ejemplo f= x^3 + 2
funcion<-function(xi){
  y<-(xi)^3+2
  return(y) 
}

integral<-((b-a)/n)*((funcion(a)+funcion(b)/2)+sum(funcion(x)))

return(integral)
}#fin funcion trapecios 

trapecios(.1,.4,1000)

##método montecarlo

montecarlo<-function(limiteinf,limitesup,n){
 a<-limiteinf
 b<-limitesup
 n<-n
# en esta función el usuario debe ingresar la función que desea integrar, por ejemplo f= x^3 + 2  
funcion<-function(xi){
   y<-(xi)^3+2
  return(y) 
}

aleatorio<-runif(n)
integral<-((b-a)/n)*sum(funcion(a+aleatorio*(b-a)))
return(integral)
}

##método simpson 1/3

simpson<-function(limiteinf,limitesup){
  a<-limiteinf
  b<-limitesup
  
  # en esta función el usuario debe ingresar la función que desea integrar, por ejemplo f= x^3 + 2
  funcion<-function(xi){
    y<-(xi)^3+2
    return(y) 
  }
  
  integral<-((b-a)/6)*(funcion(a)+ 4*funcion((a+b)/2)+funcion(b)) 
return(integral)
}#fin funcion simpson
