matrizQ=matrix(data=c(.9366,.0583,.0040,.0009,.0002,0,0,.0066,.9172,.0694,.0049,.0006,.0009,.0002,.0007,.0225,.9176,.0518,.0049,.0020,.0001,.0003,.0026,.0483,.8924,.0444,.0081,.0016,.0003,.0006,.0044,.0666,.8323,.0746,.0105,0,.0010,.0032,.0046,.0572,.8362,.0384,.0015,0,.0029,.0088,.0191,.1028,.6123),ncol=7,nrow=7,byrow=TRUE)
matriz=matrix(data=c(.9366,.0583,.0040,.0009,.0002,0,0,0,.0066,.9172,.0694,.0049,.0006,.0009,.0002,.0002,.0007,.0225,.9176,.0518,.0049,.0020,.0001,.0004,.0003,.0026,.0483,.8924,.0444,.0081,.0016,.0023,.0003,.0006,.0044,.0666,.8323,.0746,.0105,.0107,0,.0010,.0032,.0046,.0572,.8362,.0384,.0594,.0015,0,.0029,.0088,.0191,.1028,.6123,.2526,0,0,0,0,0,0,0,1),ncol=8,nrow=8,byrow=TRUE)
n=300
numaletorios=1000

#######funcion que dada una matriz de markov entrege la prob de pasar de un estado a otro en n pasos

probmarkv<-function(matriz,n){
  propios=eigen(matriz)
  valorespropios=propios$values
  matrixvecpropios=propios$vectors
  matrixlambda=diag(valorespropios)
  matrixvecpropiosinv=ginv(matrixvecpropios)
  a_ala_n=matrixvecpropios%*%(matrixlambda)^n%*%matrixvecpropiosinv
  return(a_ala_n)
}
talan<-probmarkv(matrizQ,n)
### vectores propios matriz terminal
matriz_a_la_n<-matriz%^%n
propiosterminales=eigen(matriz_a_la_n)
valorespropiosterminales=propiosterminales$values
matrixvecpropiosterminales=propiosterminales$vectors
####### simulacion montecarlo para distribucion termianl

mcprobfinal<-function(matriz,numaletorios,n){
  aleatorios<-matrix(c(runif(numaletorios*7)),nrow=numaletorios,ncol=7)
  sumaaleatorios<-apply(aleatorios,1,sum)
  aleatoriosfinales<-aleatorios/sumaaleatorios
  matriz_a_la_n<-matriz%^%n
  matrizterminal<-aleatoriosfinales%*%matriz_a_la_n
  probterminales<-apply(t(matrizterminal),1,mean)
  return(probterminales)
}


distterminal<-mcprobfinal(matrizQ,numaletorios,n)
########## funncion para calcular la probabilidad de pasar a los otros estados en n pasos
estadoinicial<-matrix(data=c(1,0,0,0,0,0,0,0),ncol=8,nrow=1)
                      
probconvalorinicial<-function(estadoinicial,matrix,n){
  matriz_a_la_n<-matriz%^%n
  prob<-estadoinicial%*%matriz_a_la_n
  return(prob)
}
probtrans<-probconvalorinicial(estadoinicial,matrix,n)

############## representacion gráfica de la cadena

TransMatriz <- new("markovchain", states = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "Default"), transitionMatrix =matriz , name = "Calificaciones Iniciales")
plot(TransMatriz)
plot(TransMatriz, package="diagram", box.size = 0.04)

############funcion matriz N para cantidad de pasos esperados antes de que la cadena sea absorbida
estadoinicial<-7

Qpasos<-function(matrizQ,estadoinicial){
  tamaño<-ncol(matrizQ)
  I<-diag(tamaño)
  restamatrices<-I-matrizQ
  N<-solve(restamatrices)
  pasosesperados<-round(apply(N,1,sum))
  pasosesperados<-pasosesperados[estadoinicial]
  return(pasosesperados) 
}
