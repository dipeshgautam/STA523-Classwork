reject <- function(n, dfunc, range, mc=FALSE) {
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
    # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  return(dfunc(seq(range[1], range[2], length = n)))
  M <- 1 # Tuning parameter (scalar) that lifts uniform over function's pdf.
  samples.uniform <- NULL # Create NULL list to store accepted proposal values under function's pdf.
  for (i in 1:n) {
    proposal <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value with [range] defining min and max.
    # Calculate the ratio of the densities: the density of the function, and 
    # the density of a uniform distribution.
    density.ratio <- dfunc(proposal)/(M*dunif(proposal, min = range[1], max = range[2]))
    if (runif(1) < density.ratio)
      samples.uniform <- c(samples.uniform, proposal) # Add newly accepted proposal value.
  }
  range <- c(0,1)
  proposal <- runif(1, min = range[1], max = range[2])
  proposal
  dbetann(proposal)
  # x <- seq(range[1], range[2], length = n)
  # Generate distribution that covers
  # envelope.pdf <- runif(n, min = 0, max = 1) # Generate envelope distribution that encompasses unknown distribution over its support.
  # dfunc(x, mean = 0, sd = 1) 
}
proposal <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value with [range] defining min and max.
test <- function(n) {
  samples.uniform <- NULL
  for (i in 1:n) {
    proposal <- runif(1, min = 0, max = 10)
    samples.uniform <- c(samples.uniform, dbetann(proposal))
  }
  return(samples.uniform)
}
hist(test(5000))
hist(samples.uniform)
samples.uniform
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}
samp <- NULL
for (i in 1:500) {
  proposal <- runif(1, min = 0, max = 1)
  samp <- c(samp, dbetann(proposal))
}
plot(samp)
reject(10, dnorm, c(0,1)) # Works.
reject(100, dnorm, c(0,1)) # Works.
reject(1000, dnorm, c(0,1)) # Works.
reject(5000, dnorm, c(0,1)) # Works.
plot(reject(5000, dnorm, c(0,1))) # Works.
hist(reject(5000, dnorm, c(0,1))) # Works.

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
plot(dbetann(seq(0,1,length=100)))
plot(dbetann(seq(0,2,length=100))) # Undefined beyond max of 1.


dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}

rnorm(1)
# mcparallel: makes sense for multiple tasks
# mcparallel: try for MH and slice sampler
# The OS is responsible for determining the number of cores to use.
m = mcparallel(rnorm(1e6))
n = mcparallel(rbeta(1e6, 1, 1))
o = mcparallel(rbeta(1e6, 1, 1))
str(mccollect(list(m, n, o)))

for(i in 1:10) {temp <- sqrt(i);  print(temp)}

detectCores()
library(parallel)
detectCores() # 24
system.time(pvec(1:1e7, sqrt, mc.cores = 1)) # There is a fixed cost: splitting the data into contiguous pieces then putting them back together. 
## If the function is more complicated (e.g. string matching for a list of 1million strings). 
## Mainly use pvec() for large input item. 
## Very rare to run a large enough vector for this to make sense.
## Sweet spot: use 1/2 to 3/4 of your cores, never all. Using all is more susceptible to performance degration and external disruptions.
