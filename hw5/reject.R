library(parallel)

reject <- function(n, dfunc, range, mc = FALSE) {
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
  # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  ## Use multiple cores?
  if (mc == TRUE & n >= 1000000) {
    mc.cores = 8 # Use 8 instead of detectCores()/2 because using a shared resource. 
  }
  
  ## Check Conditions
  stopifnot(is.numeric(n) && n%%1==0) # Ensure number of samples, n, is numeric and an integer.
  stopifnot(is.function(dfunc))
  stopifnot(is.vector(range))
  stopifnot(is.logical(mc))
  
  ## Initialize variables
  M <- min(100, max(dfunc(seq(range[1], range[2], length = 1e6)))) # Find the absolute max for the function.
  samples.uniform <- NULL # Create NULL list to store accepted proposal values under function's pdf.
  
  ## Visualization of rejection sampler.
  xseq <- seq(from = range[1], to = range[2], by = 0.01)
  plot(xseq, dfunc(xseq), type = "l", col = "blue")
  lines(xseq, M*dunif(xseq, range[1], range[2]), col = "red")
  
  ## Rejection sampler
  sample <- function (n, dfunc, range) {
    for (i in 1:n) {
      ## Vectorize. 
      proposal_x <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value for x within range.
      proposal_y <- runif(1, min = 0, max = M) # Generate 1 y-value for threshold of y.    
      if (dfunc(proposal_x) > proposal_y)
        samples.uniform <- c(samples.uniform, proposal_x) # If u < f(x) / M*g(x), then accept x as realization of f(x). Else, reject x and sample again.
    }
    return(samples.uniform)
  }
  
  ## Use-case for multi-core
  if (mc == TRUE && n > 1000){
    cores = 8 # Suggested by Dr. Rundel since we are using a shared resource.
    return(unlist(mclapply(1:cores, function(x) sample(ceiling(n/cores), dfunc, range),
                           mc.cores = cores) ) )
  }
  else{
    sample(n, dfunc, range)
  }
}

# plot(xseq, reject(xseq, dbetann, c(0,1), mc=TRUE), type = "l", col = "blue")
density.function <- function(x) {
  return(x^4*(15*exp(1)^(-(x/2)^5) + (5/81)*exp(1)^(-(x/6)^5) ))
}

reject(1000000, dtnorm, c(0,1), mc=TRUE)

score(reject(1000000, dbetann, c(0,1), mc=TRUE), dbetann) # 0.00249
score(reject(1000000, dtnorm, c(-3,3), mc=TRUE), dtnorm) # 0.0000821
score(reject(1000000, dtexp, c(0,7), mc=TRUE), dtexp) # 0.000587
score(reject(1000000, dunif_mix, c(-3,4), mc=TRUE), dunif_mix) # 0.000671
score(reject(1000000, dtnorm_mix1, c(0,10), mc=TRUE), dtnorm_mix1) # 0.000399
score(reject(1000000, dtnorm_mix2, c(-4,4), mc=TRUE), dtnorm_mix2) # 0.000419

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

## Single-core
times <- c(100, 10000, 1000000, 10000000)
dfunc <- c("dbetann", "dtnorm", "dtexp", "dunif_mix", "dtnorm_mix1", "dtnorm_mix2")

for (i in length(times)) {
  for (j in length(dfunc)) {
    system.time(reject(100, dbetann, c(0, 1) ) ) # 1.087
    system.time(reject(10000, dbetann, c(0, 1) ) ) # 1.514
    system.time(reject(1000000, dbetann, c(0, 1) ) ) # 36.237
    system.time(reject(10000000, dbetann, c(0, 1) ) ) # 328.539
    
    
    ## Multiple cores
    system.time(reject(100, dbetann, c(0, 1), mc = TRUE) ) # 0.734
    system.time(reject(10000, dbetann, c(0, 1), mc = TRUE) ) # 0.796
    system.time(reject(1000000, dbetann, c(0, 1), mc = TRUE) ) # 3.086
    system.time(reject(10000000, dbetann, c(0, 1), mc = TRUE) ) # 23.793
  }
}

