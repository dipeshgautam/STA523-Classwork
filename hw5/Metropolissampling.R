## draw a theta star value from proposal distribution which depends on previous one
## calculate the acceptance ratio r, if runif(1,0,1) < r, theta next = theta star; else theta next = theta.
## ensure we have enough sample size

library(parallel)

mh <- function(n, dfunc, range, mc)
{ 
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
  # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  
  mhsampler <- function(n, dfunc, range, mc = FALSE){
  
  sample <- matrix(NA, nrow = n, ncol = 1) ## create a matrix to save the results
  num <- 0 ## used for calculating acceptance ratio, ratio should be within 25%-40% 
  ## starting point
  sample[1,1] <- theta <- runif(1,range[1],range[2]) ## starting point needs tuning too?
  tweaking1 <- 0.5
  tweaking2 <- 2
  ratio <- -Inf
    while(ratio > 0.4 |ratio < 0.25){
      ##starting point
      b.tweak <- 0.75     
      
      
      for(i in 2:round(n/5)){
        theta.star <- rnorm(1,mean = theta, sd = b.tweak) ## draw a value from proposal distribution, b is the tuning parameter
        acceptance <- dfunc(theta.star)/dfunc(theta) ## calculate acceptance ratio
        if(runif(1,0,1) < min(acceptance,1)){
          theta = theta.star
          num = num + 1
        }
        
        sample[i,1] <- theta
        ratio <- num/(n - 1) 
      }
      
        
      ##tuning parameter
      if(ratio > 0.4){
        b.tweak <- b.tweak*tweaking1
        tweaking1 <- 2
      } 
      
      if(ratio < 0.25){
        b.tweak <- b.tweak*tweaking2
        tweaking2 <- 0.5
      } 
      
      if(ratio >0.25 && ratio < 0.4){
        
        ##save the tuning parameter
        b.final <- b.tweak
        break
      }
    }
  
  for(i in 2:n){
    theta.star <- rnorm(1,mean = theta, sd = b.final) ## draw a value from proposal distribution, b is the tuning parameter
    acceptance <- dfunc(theta.star)/dfunc(theta) ## calculate acceptance ratio
    if(runif(1,0,1) < min(acceptance,1)){
      theta = theta.star
      num = num + 1
    }
    
    sample[i,1] <- theta
    ratio <- num/(n - 1) 
    cat(ratio,'\n')
  }  
  
  return(sample)
}  
  
  
  if(mc == TRUE && n > 1000){
    cores = 8
      return(unlist(mclapply(1:cores, function(x) mhsampler(ceiling(n/cores), dfunc, range, w),
                         mc.cores = cores) ) )
  
    }else{
      mhsampler(n, dfunc, range, w)
    }
}
