reject <- function(n, dfunc, range, mc = FALSE) {
  
  ## Check Conditions
  stopifnot(is.numeric(n) && n%%1==0) # Ensure number of samples, n, is numeric and an integer.
  #   stopifnot(is.function(dfunc))
  stopifnot(is.vector(range))
  stopifnot(is.logical(mc))
  
  ## Initialize variables
  M <- min(100, max(dfunc(seq(range[1], range[2], length = 1e6)))) # Find the absolute max for the function.
  samples.uniform <- rep(NA,n) # Create NULL list to store accepted proposal values under function's pdf.
  
  ## Rejection sampler
  sample <- function (n, dfunc, range) {
    for (i in 1:n) {
      ## Vectorize. 
      proposal_x <- runif(1, min = range[1], max = range[2]) # Generate 1 proposal value for x within range.
      proposal_y <- runif(1, min = 0, max = M) # Generate 1 y-value for threshold of y.    
      if (dfunc(proposal_x) > proposal_y){
        samples.uniform[i] <- proposal_x # If u < f(x) / M*g(x), then accept x as realization of f(x). Else, reject x and sample again.
      }
      else{
        if(i>1){
          samples.uniform[i] <- samples.uniform[i-1]
        }
        
      }
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
    return(sample(n, dfunc, range))
  }
}



