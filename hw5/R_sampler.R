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
      a = unlist(mclapply(1:cores, function(x) rexp( ceiling(n/cores*1.015, 1/3)),
                             mc.cores = cores) )
      return(a[a<6])
    }else
    {
      a=rexp(ceiling(n*1.015, 1/3))
      return(a[a<6])
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


table = as.data.frame(matrix(rep(0,12*6), ncol=12))
names(table)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")

times <- c(100, 10000, 1000000, 10000000)
dfunc <- list(dbetann, "dtnorm", "dtexp", "dunif_mix", "dtnorm_mix1", "dtnorm_mix2")
range <- list( c(0,1), c(-3,3), c(0,6), c(-3, 4), c(0,10), c(-4,4))
table[,2] = "R"
for (i in 1:length(times)) {
  for (j in 1:length(dfunc)) {
    table[j,1]=dfunc[j]
    table[j, 2+i] = system.time(R(times[i], dfunc[j], range[[j]], mc = FALSE ) )[3] # 1.087
    print(c(j,i))   
    
    ## Multiple cores
    table[j, 6+i]= system.time(R(times[i], get(eval(dfunc[j])), range[[j]] , mc = TRUE) )[3] # 0.734
  }
}