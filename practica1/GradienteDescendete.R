library(ISLR)

gradiente <- function( x, y, iteration){
  
  w = rnorm(length(x[1,]) + 1)
  x = cbind(x, rep(1,length(x[,1])))

  
  for (i in 1:100) {
    print(w)
    h = w%*%t(x)
  
    err = sum(abs(h - y)) / length(h)
    h = h-y
    theta = (-1) *( err / max(x)) #Mejorable
    
    w = w - theta * (2 / length(x[,1])) * (t(x)%*%t(h))
    w = t(w)
  }
  
  w
}


mm = matrix(1:200, nrow = 10, byrow = T)
res = rep(1,length(mm[,1]))