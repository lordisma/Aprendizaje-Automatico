
pasoARecta= function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[3]
  b = -w[2]/w[3]
  c(a,b)
}


set.seed(3)	# se establece la semilla
## ------------------------------------------------------------------------
# por defecto genera 2 puntos entre [0,1] de 2 dimensiones 

simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
  m
}

## -----------------------------------------------------------------------

# función simula_gaus(N, dim, sigma) que genera un
# conjunto de longitud N de vectores de dimensión dim, conteniendo números 
# aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
# por defecto genera 2 puntos de 2 dimensiones 

simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

## ------------------------------------------------------------------------
#  simula_recta(intervalo) una funcion que calcula los parámetros
#  de una recta aleatoria, y = ax + b, que corte al cuadrado [-50,50]x[-50,50]
#  (Para calcular la recta se simulan las coordenadas de 2 ptos dentro del 
#  cuadrado y se calcula la recta que pasa por ellos), 
#  se pinta o no segun el valor de parametro visible

simula_recta = function (intervalo = c(-1,1), visible=F){
  
  ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
  b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte
  
  if (visible) {  # pinta la recta y los 2 puntos
    if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    points(ptos,col=3)  #pinta en verde los puntos
    abline(b,a,col=3)   # y la recta
  }
  c(a,b) # devuelve el par pendiente y punto de corte
}

pseudoInverse <- function( X ){
  
  udv <- svd( t(X) %*% X) #udv <- svd( X ) %*% t(X) ?
  tmp <- udv$v %*% diag( 1 / ( udv$d ) ) %*%  t( udv$v )
  tmp %*% t(X)
}

linearRegression <- function( X, y ){
  
  H <- pseudoInverse( X )
  
  H %*% y
}

SGD <- function( X, y, learningRate = 0.05, t = 0.1, itera = 1/t){
  
  N <- length(y)
  T <- as.integer(N * t)
  #w <- as.vector(t(linearRegression(X,y)))
  w <- rnorm(dim(X)[2])
  
  positive_exa <-which(y==1)
  negative_exa <-which(y==-1)
  N_po <- length(positive_exa)
  N_ne <- length(negative_exa)
  
  w_pre = w
  scored = as.numeric(error_in(X,y,w))
  
  for (a in 1:itera) {
    v <- 0  
    
    for ( i in 1:T ){
      
      pos <- ifelse((i %% 2) > 0,sample(N_po,1),sample(N_ne,1))
      pos <- ifelse((i %% 2) > 0,positive_exa[pos],negative_exa[pos])
      h <- t(w) %*% X[pos,]
      er <- (h * y[pos])
      
      if(er < 0){
        v = v + (X[pos,] * as.numeric(h-y[pos]))
      }
      #er <- er %*% t(X[pos,])
      #er <- 2*er/N
      #v  <- v - er
      #v <- -( a / b )
    }
    
    w <- w - learningRate * (v/N)
    w <- as.vector(w)
    # if(scored > error_in(X,y,w)){
    #   scored = as.numeric(error_in(X,y,w))
    #   w_pre = w
    # }else{
    #   w = w_pre
    # }
  }
  #result<-w - learningRate * v
  #as.vector(result)
  w
}

error_in <- function( data, labels, weights ){
  
  sum <- 0
  N <-length(labels)
  
  for ( i in 1:N ){
    h <- ((t(weights) %*% data[i,] ) * labels[i])
    if(h < 0){
      sum = sum +1
    }
  }
  
  sum / N
}

scorer <-function(data, labels, weights){
  sum <- 0
  #errores_A <- 0
  N <-length(labels)
  
  for ( i in 1:N ){
    h <- ( t(weights) %*% data[i,] )
    goal <- h * labels[i]
    sum <- ifelse(goal > 0, sum +1, sum)
    # errores_A <- ifelse((goal < 0) && (h > 0), errores_A +1, errores_A)
  }
  
  sum/N
}


## ------------------------------------------------------------------------


## ------------------------------------------------------------------------ 

noise <- function(label, p){
  result <- data * sample(c(1, -1), size=length(label), replace=TRUE, prob=c(1 - p, p))
  result
}

f <- function( v ){
  
  sign( (v[1]-0.2)^2 + v[2]^2 - 0.6 )
}

randomNoise <- function( labels, amount = 0.1 ){
  
  N <- length(labels)
  altered <- sample( x = N, size = N*amount )
  
  for ( i in 1:length(altered) )
    labels[altered[i]] <- -labels[altered[i]]
  
  labels
}

# Cálculo del error
errorIn <- function( data, weights ){
  
  # estooo es otro desastre
  labelsIn <- apply( data, FUN = f, MARGIN = 1 )
  lineFunc <- function( v ){
    sign( weights[3] + weights[2]*v[1] + weights[3]*v[2] )
  }
  labelsOut <- apply( data, FUN = lineFunc, MARGIN = 1 )
  
  errors <- length( labelsIn != labelsOut )
  print(length(labelsIn[labelsIn != 1]))
  print(length(labelsOut[labelsOut != 1]))
  
  errors / dim(data)[1]
}

# Repetimos 1000 veces el experimento
iterations <- 100
ein <- 0
eout <- 0
ein2 <- 0
eout2 <- 0

#for ( i in 1:iterations ){
  
  # Generamos 1000 puntos en el cuadrado [-1,1]x[-1,1]
  N <- 1000
  size <- 1
  data <- simula_unif( N, rango = c(-size,size) )
  data.test <- simula_unif(N, rango = c(-size,size))
  
  # Generamos las etiquetas
  labels_wo <- apply( data, FUN = f, MARGIN = 1 )
  labels_wo.test <- apply( data.test, FUN = f, MARGIN = 1 )
  
  # Introducimos ruido en un 10% de las muestras
  labels <- randomNoise( labels_wo )
  labels.test <- randomNoise(labels_wo.test)
  
  data = data^2
  data.test = data.test^2
  
  plot(data , col = labels + 5 )
  
  # Ajustamos un modelo de regresión
  data <- cbind( data, 1 )
  data.test <- cbind(data.test,1)
  
  weightsSVD <- linearRegression( data, labels )
  weightsSGD <- SGD( data, labels )
  
  ein = ein + scorer(data,labels,weightsSGD)
  ein2 = ein2 + scorer(data,labels,weightsSVD)
  eout = eout + scorer(data.test,labels,weightsSGD)
  eout2 = eout2 + scorer(data.test,labels,weightsSVD)
  
# #  line <- pasoARecta( weights )
#   
#   # Acumulamos el error de la regresión
#   #ein <- ein + errorIn( data, weights )
#   
#   # Generamos nuevos puntos y calculamos el error
#   # de la regresión sobre los nuevos puntos
#   test <- simula_unif( N, rango = c(-size,size) )
#   testLabels <- apply( test, FUN = f, MARGIN = 1 )
#   test <- cbind( 1, test )
#   testWeights <- linearRegression( test, testLabels )
#   eout <- eout + errorIn( test, testWeights )
#}

 ein <- ein / iterations
 eout <- eout / iterations
