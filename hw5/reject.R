reject <- function(n, dfunc, range, mc = FALSE) {
  ## Check Conditions
  stopifnot(is.numeric(n) && n%%1==0) # Ensure number of samples, n, is numeric and an integer.
  #   stopifnot(is.function(dfunc))
  stopifnot(is.vector(range))
  stopifnot(is.logical(mc))
  
  ## Initialize variables
  M <- min(100, max(dfunc(seq(range[1], range[2], length = 1e6)))) # Find the absolute max for the function. Use 100 for beta distribution.
  accepted <- c() # Initialize with uniform distribution rather than NA's to prevent accepting NA's. Initialized values overwritten anyway.
  
  ## Rejection sampler
  sample <- function (n, dfunc, range) {
    while (length(accepted) < n) {
      ## Vectorize. 
      proposal_x <- runif(trunc(1.3*n), min = range[1], max = range[2]) # Generate 1.3*n proposal values, proposal_x, within arbitrary distribution's domain.
      proposal_y <- runif(trunc(1.3*n), min = 0, max = M) # Sample 1.3*n y-values along 1.3*n vertical lines at 1.3 x's, where x = proposal_x. 
      accepted <- c(accepted, proposal_x[dfunc(proposal_x) > proposal_y]) # Subset using a conditional statement.
    }
    return(accepted[1:n]) ## Return accepted samples equal to desired amount, n.
  }
  ## Use-case for multi-core
  if (mc == TRUE && n > 1000){
    cores = 8 # Suggested by Dr. Rundel since we are using a shared resource.
    return(unlist(mclapply(1:cores, function(x) sample(ceiling(n/cores), dfunc, range),
                           mc.cores = cores) ) )
  }
  else{
    return(sample(n, dfunc, range))
  }
}


## Testing/Validation
score(reject(100000, dbetann, c(0, 1), mc = TRUE), dbetann)
hist(reject(100000, dbetann, c(0, 1), mc = TRUE))
score(reject(100000, dtnorm, c(-3, 3), mc = TRUE), dtnorm)
hist(reject(100000, dtnorm, c(-3, 3), mc = TRUE))
score(reject(100000, dtexp, c(0, 6), mc = TRUE), dtexp)
hist(reject(100000, dtexp, c(0, 6), mc = TRUE))
score(reject(100000, dunif_mix, c(-3, 4), mc = TRUE), dunif_mix)
hist(reject(100000, dunif_mix, c(-3, 4), mc = TRUE))
score(reject(100000, dtnorm_mix1, c(0, 10), mc = TRUE), dtnorm_mix1)
hist(reject(100000, dtnorm_mix1, c(0, 10), mc = TRUE))
score(reject(100000, dtnorm_mix2, c(-4, 4), mc = TRUE), dtnorm_mix2)
hist(reject(100000, dtnorm_mix2, c(-4, 4), mc = TRUE))


score(reject(100000, dtexp, c(0, 6), mc = TRUE), dtexp)
dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}

dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}

score(reject(100000, dtnorm, c(-3, 3)), dtnorm)

dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}

dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}

dtexp = function(x)
{
  ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)
}

dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}

dtnorm_mix1 = function(x)
{
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.9206407)
}


dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.55)
}


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