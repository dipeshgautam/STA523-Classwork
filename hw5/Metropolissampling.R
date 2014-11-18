## draw a theta star value from proposal distribution which depends on previous one
## calculate the acceptance ratio r, if runif(1,0,1) < r, theta next = theta star; else theta next = theta.
## ensure we have enough sample size

mh <- function(n, dfunc, range, mc=FALSE)
{ 
  # n = samples
  # dfunc = density function
  # range = numeric vector defining the min & max range of the pdf (e.g.: range = c(0, 1) )
  # Used for the vector of quantiles: x <- seq(range[1], range[2], length = n))
  # mc = multicore
  
  sample <- matrix(NA, nrow = n, ncol = 1) ## create a matrix to save the results
  num <- 0 ## used for calculating acceptance ratio, ratio should be within 25%-40% 
  ## starting point
  sample[1,1] <- theta <- 1 ## starting point needs tuning too?
  b <- (range[2] - range[1])/6 ## (max - min) is about 6 standard deviation? 
  for(i in 2:n){
    theta.star <- rnorm(1,mean = theta, sd = b) ## draw a value from proposal distribution, b is the tuning parameter
    acceptance <- dfunc(theta.star)/dfunc(theta) ## calculate acceptance ratio
    if(runif(1,0,1) < min(acceptance,1)){
      sample[i,1] <- theta.star
      num = num + 1
    } else {
      sample[i,1] <- theta
    }    
  }
  
  ratio <- num/(n - 1)
  
  return(sample)
}


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