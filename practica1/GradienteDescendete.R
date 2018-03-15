
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

w = gradiente(1,1,0.1, 10 * 10^-14)

expr=expression((  (u^3 * exp(v-2)) - (4 * v^3 * exp(-u))  )^2)


