gradiente <-function(theta, w, x, y)
{
  w = w - theta * (2/length(x) * (x * ( det(t(w)%*%x) - y )) )
  
}