
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

N = 1000

## Beta
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}
A = slice(N, dbetann, c(0,1), mc=FALSE)
score(A, dbetann)
system.time(slice(N, dbetann, c(0,1), mc=FALSE))


## Truncated Normal

dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}
B = slice(N, dtnorm, c(-3,3), mc=FALSE)
score(B, dtnorm)
system.time(slice(N, dtnorm, c(-3,3), mc=FALSE))


## Truncated Exponential
dtexp = function(x)
{
  ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)
}
C = slice(N, dtexp, c(0,6), mc=FALSE)
score(C, dtexp)
system.time(slice(N, dtexp, c(0,6), mc=FALSE))


## Uniform Mixture
dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}
D = slice(N, dunif_mix, c(-3,4), mc=FALSE)
score(D, dunif_mix)
system.time(slice(N, dunif_mix, c(-3,4), mc=FALSE))



## Truncated Normal Mixture 1
dtnorm_mix1 = function(x)
{
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.90059152)
}
E = slice(N, dtnorm_mix1, c(0,10), mc=FALSE)
score(E, dtnorm_mix1)
system.time(slice(N, dtnorm_mix1, c(0,10), mc=FALSE))


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
score(F, dtnorm_mix2)
system.time(slice(N, dtnorm_mix2, c(-4,4), mc=FALSE))
