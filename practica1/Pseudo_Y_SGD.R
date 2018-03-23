
digit.train <- read.table("zip.train",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)
digit.test <- read.table("zip.test",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)

digitos15.train = digit.train[digit.train$V1==2 | digit.train$V1==8,]
digitos15.test  = digit.test[digit.test$V1==2 | digit.test$V1==8,]
digitos = digitos15.train[,1]  # vector de etiquetas del train
digitos.test = digitos15.test[,1]  # vector de etiquetas del test
ndigitos = nrow(digitos15.train)  # numero de muestras del train
ndigitos.test = nrow(digitos15.test)  # numero de muestras del test
# Etiquetar los 5 como la clase -1
digitos[ digitos == 8 ] = -1
digitos.test[ digitos.test == 8 ] = -1
digitos[ digitos == 2 ] = 1
digitos.test[ digitos.test == 2 ] = 1

# se retira la clase y se monta una matriz 3D: 599*16*16
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))
grises.test = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos.test,16,16))
grises.test = as.numeric(grises.test)
grises.test = array(grises.test,c(ndigitos.test,16,16))

rm(digit.train) 
rm(digitos15.train)
rm(digit.test) 
rm(digitos15.test)

# Para visualizar los 4 primeros
## ------------------------------------------------------------------------

# par(mfrow=c(2,2))
# for(i in 1:4){
#   imagen = grises.test[i,,16:1] # se rota para verlo bien
#   image(z=imagen)
# }

#digitos[1:4] # etiquetas correspondientes a las 4 im??genes


## ------------------------------------------------------------------------
fsimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

## ------------------------------------------------------------------------
# Dado un vector de pesos, se quiere obtener los par??metros de la recta, 
# para ello ser?? necesario calcular pendiente y desplazamiento
# PRECONDICION se considera w[3] como el d+1

pasoARecta= function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[2]
  b = -w[3]/w[2]
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

# funci??n simula_gaus(N, dim, sigma) que genera un
# conjunto de longitud N de vectores de dimensi??n dim, conteniendo n??meros 
# aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
# por defecto genera 2 puntos de 2 dimensiones 

simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generaci??n se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensi??n")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

## ------------------------------------------------------------------------
#  simula_recta(intervalo) una funcion que calcula los par??metros
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

# Para el apartado 3 del Ejercicio 1 
#-------------------------------------------------------------------------------
## funcion para pintar la frontera de la funci??n
# a la que se pueden a??adir puntos, y etiquetas

pintar_frontera = function(f,rango=c(-50,50)) {
  x=y=seq(rango[1],rango[2],length.out = 500)
  z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
    plot(1, type="n", xlim=rango, ylim=rango)
  contour(x,y,z, levels = 1:20, xlim =rango, ylim=rango, xlab = "x", ylab = "y")
}

# Ejemplo de llamada a una funcion f1_xy que hemos de definir
# pintar_frontera(f1_xy) 




## ------------------------------------------------------------------------

# print ("Pulsa RETORNO")
# scan(n=1)


noise <- function(label, p){
  result <- data * sample(c(1, -1), size=length(label), replace=TRUE, prob=c(1 - p, p))
  result
}

makeGraph <- function( intensidades, simetrias, digitos, weights1, weights2 ){
  
  # Initial data
  par(mfrow=c(1,1))
  plot(
    intensidades,
    simetrias,
    col = digitos + 5
  )
  
  # Regression line
  lineParameters <- pasoARecta( weights1 )
  lineParameters2 <- pasoARecta( weights2 )
  #lineFunc <- function( x ) { -52.19644 * x - 122.25713 }
  lineFunc <- function( x ) { lineParameters[1] * x + lineParameters[2] }
  lineFunc2 <- function( x ) { lineParameters2[1] * x + lineParameters2[2] }
  lineX <- c( -1, 0 )
  lineY <- c( lineFunc(-1), lineFunc(0) )
  lineY2 <- c( lineFunc2(-1), lineFunc2(0) )
  lines( lineX, lineY, col = "black" )
  lines( lineX, lineY2, col = "green" )
  
  #pintar_frontera(lineFunc)
}


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





# ----------
# ----------
simetrias <- apply( X = grises, FUN = fsimetria, MARGIN = 1 )
intensidades <- apply( X = grises, FUN = mean, MARGIN = 1 )

simetrias.test <- apply( X = grises.test , FUN = fsimetria, MARGIN = 1)
intensidades.test <- apply( X = grises.test, FUN = mean, MARGIN = 1 )

X <- cbind( intensidades, simetrias, 1 )
X.test <- cbind( intensidades.test, simetrias.test, 1 )
y <- digitos
y.test <- digitos.test

weights <- linearRegression( X, y )
scor<-scorer(X,y,weights)
w <- SGD( X, y,learningRate = 0.05, t=0.04, itera = 100 )
print("SGD (train):")
print(as.numeric(scorer(X,y,w)) )
print("SVD (train):")
print(as.numeric(scorer(X,y,weights = weights)) )
print("SGD (test):")
print(as.numeric(scorer(X.test,y.test,w)) )
print("SVD (test):")
print(as.numeric(scorer(X.test,y.test,weights = weights)) )
makeGraph(intensidades, simetrias,digitos,w,weights)