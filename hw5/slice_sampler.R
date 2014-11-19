
slice = function(n, dfunc, range, mc=FALSE)
{
  w = 1

  x0 = runif(1, range[1], range[2])
  Y = matrix(rep(0,2*n),ncol=2)
  for (i in 1:n)
  {
    y = runif(1, 0, dfunc(x0))
    range2 = c((x0-w), (x0+w))
    x1 = runif(1, range2[1], range2[2])
    if (y<=dfunc(x1))
    {
      Y[i,] = c(x1, dfunc(x1))
      x0=x1
    }else
    {
      Y[i,] = c(x0, dfunc(x0))
    }
        
  }
  return(Y)
}

### Tests


dfunc = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}


dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.4999683)
}


A = slice(1000, dfunc, c(-4,4), mc=FALSE)
