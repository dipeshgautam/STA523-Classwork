reject = function(n, dfunc, range, mc=FALSE)
{ ... }

mh = function(n, dfunc, range, mc=FALSE)
{ ... }

slice = function(n, dfunc, range, mc=FALSE)
{ ... }

R = function(n, dfunc, range, mc=FALSE)
{ ... }

###### Testing ######
  
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}

dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}

rnorm(1)
# mcparallel: try for MH and slice sampler
# The OS is responsible for determining the number of cores to use.
m = mcparallel(rnorm(1e6))
n = mcparallel(rbeta(1e6, 1, 1))
o = mcparallel(rbeta(1e6, 1, 1))
str(mccollect(list(m, n, o)))



detectCores()
library(parallel)
detectCores() # 24
system.time(pvec(1:1e7, sqrt, mc.cores = 1)) # There is a fixed cost: splitting the data into contiguous pieces then putting them back together. 
## If the function is more complicated (e.g. string matching for a list of 1million strings). 
## Mainly use pvec() for large input item. 
## Very rare to run a large enough vector for this to make sense.
## Sweet spot: use 1/2 to 3/4 of your cores, never all. Using all is more susceptible to performance degration and external disruptions.
