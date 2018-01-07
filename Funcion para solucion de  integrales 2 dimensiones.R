#integración 
##método trapecios


trapecios<-function(limiteinf1,limitesup1,limiteinf2,limitesup2,numtrapecios){
a<-limiteinf1
b<-limitesup1
c<-limiteinf2
d<-limitesup2
n<-numtrapecios
deltax<-(b-a)/n
deltay<-(d-c)/n
x<-seq(a,b,deltax)
y<-seq(c,d,deltay)

mesh<-meshgrid(x,y)

y<-exp(cos^2(mesh$X*mesh$Y)) #funcion a integrar

w<-mean(mean(y))
integral<-w*(b-a)*(d-c)

return(integral)

}#fin funcion trapecios 


##método montecarlo

montecarlo<-function(limiteinf1,limitesup1,limiteinf2,limitesup2,n){

 a<-limiteinf1
 b<-limitesup1
 c<-limiteinf2
 d<-limitesup2
 x<-runif(n)*(b-a)+a
 y<-runif(n)*(d-c)+c
 
 mesh<-meshgrid(x,y)

 y<-exp(cos^2(mesh$X*mesh$Y))   #funcion a integrar
  
 w<-mean(mean(y))
 integral<-w*(b-a)*(d-c)
 
return(integral)
}

