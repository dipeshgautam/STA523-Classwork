reject <- function(n, dfunc, range, mc=FALSE) {
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
    # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  M = min(100,max(dfunc(seq(range[1], range[2], length = 1e6)))) # Find the absolute max for the function
  #M <- 100 # Tuning parameter (scalar) that lifts uniform over function's pdf.
  samples.uniform <- NULL # Create NULL list to store accepted proposal values under function's pdf.
  for (i in 1:n) {
    proposal_x <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value for x within range.
    # Calculate the ratio of the densities: the density of the function, and 
    # the density of a uniform distribution.
    
    proposal_y = runif(1, min=0, max = M)
    
    if (dfunc(proposal_x) > proposal_y)
      samples.uniform <- c(samples.uniform, proposal_x) # If u < f(x) / M*g(x), then accept x as realization of f(x). Else, reject x and sample again.
    
    #density.ratio <- dfunc(proposal)/(M*dunif(proposal, min = range[1], max = range[2])) # f(x) / M*g(x)
    ## f(x) == target distribution
    ## g(x) == instrumental distribution
    ## Sample x from g(x) and u from U(0,1), the uniform distro over the unit interval.
    #if (runif(1) < density.ratio)  # Check whether or not u < f(x) / M*g(x)
    #  samples.uniform <- c(samples.uniform, proposal) # If u < f(x) / M*g(x), then accept x as realization of f(x). Else, reject x and sample again.
  }
  return(samples.uniform)
  #range <- c(0,1)
#   proposal <- runif(1, min = range[1], max = range[2])
#   proposal
#   dbetann(proposal)
  # x <- seq(range[1], range[2], length = n)
  # Generate distribution that covers
  # envelope.pdf <- runif(n, min = 0, max = 1) # Generate envelope distribution that encompasses unknown distribution over its support.
  # dfunc(x, mean = 0, sd = 1) 
}

range <- c(0, 1)
proposal <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value with [range] defining min and max.
proposal
dfunc <- dbetann
density.ratio <- dfunc(proposal)/(dunif(proposal, min = range[1], max = range[2])) # f(x) / M*g(x)
density.ratio
runif(1) < density.ratio


head(reject(100, dbetann, c(0,1) ))
plot(reject(100, dbetann, c(0,1) ))
## Colin: reject(100, dunif, c(0,1) )

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
