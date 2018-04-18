
# recordarle en la memoria que sabemos que sacar mejor error en training no significa sacar mejor error en test.

# usar pintar_frontera( f, rango de valores )
# mirar ggplot2!
# y contour:
#   https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/contour
#   https://cran.r-project.org/web/packages/ContourFunctions/vignettes/Introduction_to_the_cf_R_package.html
# y cf_data
# y scatter plot:
#   http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

par( mfrow = c( 1, 1 ) )

# Funciones facilitdas

set.seed(3)	# se establece la semilla
## ------------------------------------------------------------------------
# por defecto genera 2 puntos entre [0,1] de 2 dimensiones 

simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
  m
}


simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generaci??n se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensi??n")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}


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


pintar_frontera = function(f,rango=c(-50,50)) {
  x=y=seq(rango[1],rango[2],length.out = 500)
  z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
    plot(1, type="n", xlim=rango, ylim=rango)
  contour(x,y,z, levels = 1:20, xlim =rango, ylim=rango, xlab = "x", ylab = "y")
}


pasoARecta= function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[3]
  b = -w[2]/w[3]
  if ( w[3] == 0 ){
    a = 0
    b = 0
  }
  c(a,b)
}


makeGraph <- function( intensidades, simetrias, digitos, weights, range = c(-1,0) ){
  
  # Initial data
  par(mfrow=c(1,1))
  plot(
    intensidades,
    simetrias,
    col = digitos + 4
  )
  
  # Regression line
  lineParameters <- pasoARecta( weights )
  lineFunc <- function( x ) { lineParameters[1] * x + lineParameters[2] }
  lineX <- range
  lineY <- c( lineFunc(-1), lineFunc(0) )
  lines( lineX, lineY, col = "black" )
  
  print('holi')
  print(weights)
  print( lineParameters )
  print(lineX)
  print(lineY)
  
  #pintar_frontera(lineFunc)
}



## Ejercicio 1


## Apartado 1.1

N <- 50
size <- 50
range = c( -size, size )
sigma <- c( 5, 7 )
dataUnif <- simula_unif( N, rango = range )
dataGaus <- simula_gaus( N, sigma = sigma )

plot(
  rbind( dataUnif, dataGaus ),
  col = c( rep(3,N), rep(5,N) )
)


# Apartado 1.2

line <- simula_recta( intervalo = c( -size, size ) )
a <- line[1]
b <- line[2]

f <- function( v ) {
  sign( v[2] - a * v[1] - b )
}
lineFunc <- function( x ) {
  a * x + b
}
dataAll <- rbind( dataUnif, dataGaus )

labels <- apply( dataAll, FUN = f, MARGIN = 1 )

lineX <- c( -range, range )
lineY <- c( lineFunc(-range), lineFunc(range) )
plot( dataAll, col = labels + 4 )
lines( lineX, lineY, col = "black" )


noise <- function( labels, p = 0.1 ){
  
  # Ver d??nde est?? cada positivo y negativo
  
  positives <- c()
  negatives <- c()
  
  posSize <- length( labels[ labels == 1 ] )
  negSize <- length( labels[ labels == -1 ] )
  for ( i in 1:length(labels) )
    if ( labels[i] == 1 )
      positives <- c( positives, i )
    else
      negatives <- c( negatives, i )
  
  # Generar las posiciones que se alterar??n
  
  posPositions <- sample( 1:posSize, posSize * p )
  negPositions <- sample( 1:negSize, negSize * p )
  
  
  labels[ posPositions ] = -1
  labels[ negPositions ] = 1
  
  print( posPositions )
  print( negPositions )
  
  labels
}

labels <- noise( labels )

plot( dataAll, col = labels + 4 )
lines( lineX, lineY, col = "black" )


# Apartado 1.3

f1 <- function( v ) {
  sign( ( v[1] - 10 ) ^2 + ( v[2] - 20 ) ^2 - 400 )
}
f2 <- function( v ) {
  sign( 0.5 * ( v[1] + 10 ) ^2 + ( v[2] - 20 ) ^2 - 400 )
}
f3 <- function( v ) {
  sign( 0.5 * ( v[1] - 10 ) ^2 - ( v[2] + 20 ) ^2 - 400 )
}
f4 <- function( v ) {
  sign( v[2] - 20 * v[1] ^2 - 5 * v[1] + 3 )
}

par( mfrow = c( 1, 1 ) ) ## ??C??mo se pon??an cuatro a la vez?

labels1 <- apply( dataAll, FUN = f1, MARGIN = 1 )
labels2 <- apply( dataAll, FUN = f2, MARGIN = 1 )
labels3 <- apply( dataAll, FUN = f3, MARGIN = 1 )
labels4 <- apply( dataAll, FUN = f4, MARGIN = 1 )

plot( dataAll, col = labels1 + 4 )
plot( dataAll, col = labels2 + 4 )
plot( dataAll, col = labels3 + 4 )
plot( dataAll, col = labels4 + 4 )



# Ejercicio 2

# Apartado 2.1

dataUnif <- simula_unif( N, rango = range )
dataGaus <- simula_gaus( N, sigma = sigma )
data <- rbind( dataUnif, dataGaus )
data <- cbind(data,1)
labels <- apply( data, FUN = f, MARGIN = 1 )
dims <- 2
weightsZero <- rep( 0, dims + 1 )
weightsUnif <- simula_unif( dims + 1, dims = 1, rango = c(0,1) )
maxIterations = 10

PLA <- function( X, Y, startWeights, learningRate = 0.01, maxIterations ){
  
  i <- 0
  threashold <- 0.01
  ascending <- TRUE
  weights <- startWeights
  prevWeights <- weights
  
  while ( i < maxIterations & ascending ){ #while(abs(w_predecesors - weights))
    
    ascending <- FALSE
    
    for ( elem in 1:length(Y) ){
      
      valor_predict = sign( as.numeric( t(weights) %*% X[elem,] ) )
      wellClassified = valor_predict * Y[elem]
      #print(t(weights))
      #print(X[elem,])
      #print(valor_predict)
      #print(Y[elem])
      #print(" ")
      if ( wellClassified < 0 ){
        weights = weights + learningRate * X[elem,] * wellClassified
        ascending <- TRUE
        #print( "ascending" )
        #print( i )
      }
    }
    
    i <- i + 1
    print(weights)
  }
  
  #print( i )
  
  #weights <- weights / max(abs(weights))
  weights
}

weights <- PLA( data, labels, weightsZero, 1, maxIterations )
makeGraph( data[,1], data[,2], labels, weights, range )

weights <- PLA( data, labels, weightsUnif, 1, maxIterations )
makeGraph( data[,1], data[,2], labels, weights, range )



## Apartado 2.2

error_in <- function( data, labels, weights ){
  
  sum <- 0
  N <-length(labels)
  
  for ( i in 1:N ){
    h <- ((t(weights) %*% data[i,] ) * labels[i])
    if(h > 0){
      sum = sum +1
    }
  }
  
  sum / N
}

LR <- function( X, y, learningRate = 0.05, t = 0.1, itera = 1/t){
  
  N <- length(y)
  T <- as.integer(N * t)
  w <- rnorm(dim(X)[2])
  
  positive_exa <-which(y==1)
  negative_exa <-which(y==-1)
  N_po <- length(positive_exa)
  N_ne <- length(negative_exa)
  
  w_pre = w
  scored = as.numeric(error_in(X,y,w))
  
  for (a in 1:itera) {
    g <- 0  
    
    for ( i in 1:T ){
      
      pos <- ifelse((i %% 2) > 0,sample(N_po,1),sample(N_ne,1))
      pos <- ifelse((i %% 2) > 0,positive_exa[pos],negative_exa[pos])
      h <- t(w) %*% X[pos,]
      er <- (h * y[pos])
      
      g = g + (   ((X[pos,] * y[pos])*-1)/(1 + exp(er))   )
      
    }
    
    w <- w - learningRate * (g/N)
    w <- as.vector(w)
  }
  w <- w /max(abs(w))
  w
}

sigmoid <- function(x){
  1 / (1 + exp(-x))
}

PlotRoc<-function(X,Y,w){
  Prediction = sigmoid(X %*% w)
  Sensibilidad = 1:100
  Expecifidad = 1:100
  
  
  for (i in 1:100) {
    Prediction_pos = Y[which(Prediction >= i/100)]
    Prediction_neg = Y[which(Prediction < i/100)]
    
    VP = Prediction_pos[Prediction_pos > 0]
    FP = Prediction_pos[Prediction_pos < 0]
    
    VN = Prediction_neg[Prediction_neg < 0]
    FN = Prediction_neg[Prediction_neg > 0]
    
    Sensibilidad[i] = length(VP) / (length(VP) + length(FN))
    Expecifidad[i] = 1 - (length(VN) / (length(VN) + length(FP)))
  }
  
  df = data.frame(Sensibilidad, Expecifidad)
  df

}

N <- 100
size <- 2
range = c( -size, size )
data <- simula_unif( N, rango = range )
lineX <- simula_unif( 2, dims = 1, rango = range )
lineY <- simula_unif( 2, dims = 1, rango = range )
f <- function(x){
  ( lineY[2] - lineY[1] ) / ( lineX[2] - lineX[1] )  *  ( x - lineX[1] )  /  lineX[2]
}
#labels <- apply( data, FUN = f, MARGIN = 1 )
labels <- ifelse(data[,2] > 0, 1, -1)
data <- cbind( data, 1 )

weights <- LR( data, labels )
makeGraph( data[,1], data[,2], labels, weights, range )
result = PlotRoc(data,labels,weights)

