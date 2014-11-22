library(parallel)
reject <- function(n, dfunc, range, mc = FALSE) {
  set.seed(13)
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
  # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  ## Use multiple cores?
  if (mc == TRUE & n >= 1000000) {
    mc.cores = 8 # Use 8 instead of detectCores()/2 because using a shared resource. 
  }
  
  if (mc == TRUE && n > 1000){
    cores = 8
    return(unlist(mclapply(1:cores, function(x) sample(ceiling(n/cores), dfunc, range, w),
                           mc.cores = cores) ) )
  }else{
    sample(n, dfunc, range, w)
  }
  M <- min(100,max(dfunc(seq(range[1], range[2], length = 1e6)))) # Find the absolute max for the function.
  samples.uniform <- NULL # Create NULL list to store accepted proposal values under function's pdf.
  for (i in 1:n) {
    ## Vectorize. 
    proposal_x <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value for x within range.
    proposal_y = runif(1, min = 0, max = M) # Generate 1 y-value for threshold of y.    
    if (dfunc(proposal_x) > proposal_y)
      samples.uniform <- c(samples.uniform, proposal_x) # If u < f(x) / M*g(x), then accept x as realization of f(x). Else, reject x and sample again.
  }
  return(samples.uniform)
}

range <- c(0, 1)
proposal <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value with [range] defining min and max.
proposal
dfunc <- dbetann
min(100,max(dfunc(seq(range[1], range[2], length = 1e6))))
runif(1) < density.ratio


score(reject(1000000, dbetann, c(0,1), mc=TRUE), dbetann) # Score is close to zero.

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
    system.time(reject(100, dbetann, c(0, 1), mc = TRUE) ) # 0.811
    system.time(reject(10000, dbetann, c(0, 1), mc = TRUE) ) # 1.532
    system.time(reject(1000000, dbetann, c(0, 1), mc = TRUE) ) # 27.423
    system.time(reject(10000000, dbetann, c(0, 1), mc = TRUE) ) # 282.693
  }
}

