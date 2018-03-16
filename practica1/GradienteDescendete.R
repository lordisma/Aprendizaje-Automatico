
gradiente <- function(u, v, theta, epsilon){
  expr=expression((  (u^3 * exp(v-2)) - (4 * v^3 * exp(-u))  )^2)
  expr.Du = D(expr, "u")
  expr.Dv = D(expr, "v")
  
  err = abs(eval(expr))
  
  it = 1
  
  while(err > epsilon){
    # Mirar como cambiarlo para varias caracteristicas
    u = u - theta * eval(expr.Du)
    v = v - theta * eval(expr.Dv)
    err = abs(eval(expr))
    it = it +1  #Cuenta iteraciones
  }
  
  c(u,v,it,err)
  
}

gradiente2 <- function(x, y, theta, iteration){
  expr=expression( (x-2)^2 + 2*(y+2) + 2*sin(2*pi*x) * sin(2*pi*y) )
  expr.Dx = D(expr, "x")
  expr.Dy = D(expr, "y")
  
  err = abs(eval(expr))
  it = 1
  
  result = data.frame(x,y,err,it)
  
  for(i in 1:iteration){
    # Mirar como cambiarlo para varias caracteristicas
    x = x - theta * eval(expr.Dx)
    y = y - theta * eval(expr.Dy)
    err = abs(eval(expr))
    it = it +1  #Cuenta iteraciones
    
    result$x=append(result$x , x)
    result$y=append(result$y , y)
    result$err=append(result$err , err)
    result$it=append(result$it , it)
  }
  
  result
  
}

w = gradiente2(1,1,0.01, 10)

