
gradiente <- function(u, v, theta, epsilon){
  expr=expression((  (u^3 * exp(v-2)) - (4 * v^3 * exp(-u))  )^2)
  expr.Du = D(expr, "u")
  expr.Dv = D(expr, "v")
  
  err = abs(eval(expr))
  
  it = 1
  
  valores_u = c(u)
  valores_v = c(v)
  error     = c(err)
  
  while(err > epsilon){
    # Mirar como cambiarlo para varias caracteristicas
    u = u - theta * eval(expr.Du)
    v = v - theta * eval(expr.Dv)

    
    err = abs(eval(expr))
    it = it +1  #Cuenta iteraciones
    
    
    valores_u = append(valores_u,u)
    valores_v = append(valores_v,v)
    error     = append(error,err)
  }
  
  data.frame(valores_u, valores_v,error)
  
}

gradiente2 <- function(x, y, theta, iteration){
  expr=expression( (x-2)^2 + 2*(y+2) + 2*sin(2*pi*x) * sin(2*pi*y) )
  expr.Dx = D(expr, "x")
  expr.Dy = D(expr, "y")
  
  err = abs(eval(expr))

  valores_x = c(x)
  valores_y = c(y)
  error     = c(err)
  
  for(i in 1:iteration){
    # Mirar como cambiarlo para varias caracteristicas
    x = x - theta * eval(expr.Dx)
    y = y - theta * eval(expr.Dy)
    err = abs(eval(expr))
    
    valores_x = append(valores_x,x)
    valores_y = append(valores_y,y)
    error     = append(error,err)
    
  }
  
  data.frame(valores_x, valores_y,error)
  
  
}

ejercicio1 = gradiente(1,1,0.01, 10 * 10^-14)
ejercicio2 = gradiente2(1,1,0.1, 100)

x = matrix(0:20^2,nrow = 21, byrow = T)
y = matrix(0:20^2,nrow = 21)
#mm = eval(expr)
#scatter3D(x,y,mm,colvar = x,colkey = T)

