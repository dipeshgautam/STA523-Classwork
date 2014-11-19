
slice = function(n, dfunc, range, mc=FALSE)
{
  ## Define initial width
  w = (range[2]-range[1])/20
  
  ## Sample initial x0 within the range
  x0 = runif(1, range[1], range[2])
  Y = matrix(rep(0,2*n),ncol=2)
  for (i in 1:n)
  {
    ## Sample y from the range (0, f(x0))
    y = runif(1, 0, dfunc(x0))
    
    ## Create initial area around x0
    range2 = c(x0-w,x0+w)
    
    ## Check if the left end is just outside the slice, if not expand the interval until it is
    while (y<=dfunc(range2[1])){
      range2[1] = range2[1]-w
    }
    ## Check if the right end is just outside the slice, if not expand the interval until it is
    while (y<=dfunc(range2[2])){
      range2[2] = range2[2]+w
    }
    
    
    ## Sample x1 from the interval
    x1 = runif(1, range2[1], range2[2])
    
    ## Check if the sampled x1 is outside the range, it is sample again with x1 as the boundary of the range
    while (dfunc(x1)<=y){
      if (x1<x0){
        range2[1] = x1
        
      } else
      {
        range2[2] = x1        
      }
      x1 = runif(1, range2[1], range2[2])
    }
    
    ## Set reset x0 to the newly sampled value and store the result.
    x0=x1
    Y[i,] = c(x1, dfunc(x1))       
  }
  return(Y)
}

#### Scores
score = function(x, dfunc) 
{
  stopifnot(is.numeric(x) & length(x))
  
  x = sort(x)
  n = length(x)
  
  ex = ecdf(x)(x)
  
  dx = dfunc(x)
  ed = cumsum(c(0, (x[-1]-x[-n])*(dx[-1]+dx[-n])/2))
  
  return( sqrt(sum((ex-ed)^2)/n) )
}




### Tests

N = 10000

## Beta
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}
A = slice(1000, dbetann, c(0,1), mc=FALSE)
score(A[,1], dbetann)


## Truncated Normal

dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}
B = slice(N, dtnorm, c(-3,3), mc=FALSE)
score(B[,1], dtnorm)


## Truncated Exponential
dtexp = function(x)
{
  ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)
}
C = slice(N, dtexp, c(0,6), mc=FALSE)
score(C[,1], dtexp)


## Uniform Mixture
dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}
D = slice(N, dunif_mix, c(-3,4), mc=FALSE)
score(D[,1], dunif_mix)



## Truncated Normal Mixture 1
dtnorm_mix1 = function(x)
{
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.90059152)
}
E = slice(N, dtnorm_mix1, c(-0,10), mc=FALSE)
score(E[,1], dtnorm_mix1)



## Truncated Normal Mixture 2
dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.4999683)
}
F = slice(N, dtnorm_mix2, c(-4,4), mc=FALSE)
score(F[,1], dtnorm_mix2)


