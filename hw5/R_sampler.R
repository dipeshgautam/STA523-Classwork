R = function(n, dfunc, range, mc)
{
  library(parallel)
  library(truncnorm)
  stopifnot(is.numeric(n)&&n%%1==0)
  stopifnot(is.function(dfunc))
  stopifnot(is.vector(range))
  stopifnot(is.logical(mc))
  
  ## Beta
  if(substitute(dfunc)=="dbetann"){
    if (mc==TRUE && n>1000000){
      cores=8
      return(unlist(mclapply(1:cores, function(x) rbeta( ceiling(n/cores), .9, .9 ),
                             mc.cores = cores) ) )
    }else{
      return(rbeta(n, .9, .9))
    } 
    
    ## Truncated Normal  
  }else if(substitute(dfunc)=="dtnorm")
  {
    if (mc==TRUE && n>1000000){
      return(unlist(mclapply(1:cores, function(x) rtruncnorm(ceiling(n/cores),-3,3),
                             mc.cores = cores) ) )
    }else
    {
      return(rtruncnorm(n,-3,3))      
    }
    
    ## Truncated Exponential  
  }else if(substitute(dfunc)=="dtexp"){
    if (mc==TRUE && n>1000000){
      cores=8
      return(unlist(mclapply(1:cores, function(x) rexp( ceiling(n/cores*1.015)),
                             mc.cores = cores) ) )
    }else
    {
      return(rexp(ceiling(n*1.015)))
    }
    
    ## Uniform Mixture  
  }else if(substitute(dfunc)=="dunif_mix"){
    if (mc==TRUE && n>1000000)
    {
      cores=8
      return(unlist(mclapply(1:cores, function(x) c(runif(ceiling(0.6*n/cores),-3,-1),
                                                    runif(ceiling(0.1*n/cores),-1, 1),
                                                    runif(ceiling(0.3*n/cores), 1, 4)),
                             mc.cores = cores) ) )
    }else
    {
      return(c(runif(ceiling(0.6*n),-3,-1),
               runif(ceiling(0.1*n),-1, 1),
               runif(ceiling(0.3*n), 1, 4)))
    }
    
    ## Truncated Normal Mixture 1  
  }else if(substitute(dfunc)=="dtnorm_mix1"){
    if (mc==TRUE && n>=1000000){
      cores=8
      return(unlist(mclapply(1:cores, function(x) c(rtruncnorm(ceiling(0.5*n/cores),0,10,2,2),
                                                    rtruncnorm(ceiling(0.5*n/cores),0,10,6,1)),
                             mc.cores = cores) ) )
      
    }else
    {
      return(c(rtruncnorm(ceiling(0.5*n),0,10,2,2),
               rtruncnorm(ceiling(0.5*n),0,10,6,1)))
    }
    
    ## Truncated Normal Mixture 2  
  }else if(substitute(dfunc)=="dtnorm_mix2"){
    if (mc==TRUE && n>1000000){
      cores=8
      return(unlist(mclapply(1:cores,
                             function(x) c(rtruncnorm(ceiling(0.45*n/cores),-4,4,-4),
                                           rtruncnorm(ceiling(0.45*n/cores),-4,4,4),
                                           rtruncnorm(ceiling(0.1*n/cores),-4,4,0,0.5)),
                             mc.cores = cores) ))
      
    }else
    {
      return(c(rtruncnorm(ceiling(0.45*n),-4,4,-4),
               rtruncnorm(ceiling(0.45*n),-4,4,4),
               rtruncnorm(ceiling(.1*n),-4,4,0,.5)))
    }
    
    
  }                             
  
}