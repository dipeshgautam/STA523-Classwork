source("R_sampler.R")
source("slice_sampler.R")
source("Metropolissampling.R")
source("reject.R")
suppressMessages(install.packages("truncnorm"))
library(parallel)
library(truncnorm)

getTable= function(name, func){
  table = as.data.frame(matrix(rep(0,12*6), ncol=12))
  names(table)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")
  
  times <- c(100, 10000, 1000000, 10000000)
  
  dfunc <- list("dbetann", "dtnorm", "dtexp", "dunif_mix", "dtnorm_mix1", "dtnorm_mix2")
  range <- list( c(0,1), c(-3,3), c(0,6), c(-3, 4), c(0,10), c(-4,4))
  table[,2] = name
  for (i in 1:length(times)) {
    for (j in 1:length(dfunc)) {
      table[j,1]=dfunc[[j]]
      table[j, 2+i] = (system.time(func(times[i],dfunc[[j]], range[[j]], mc = FALSE ) )[3])/times[i]
      print(c(j,i))   
      
      ## Multiple cores
      table[j, 6+i]= (system.time(func(times[i],eval(dfunc[[j]]), range[[j]] , mc = TRUE) )[3])/times[i]
      if (i==3){
        ## Single Core
        x = func(times[i],eval(dfunc[[j]]), range[[j]], mc = FALSE )
        table[j, 11] = score(x,get(dfunc[[j]]))
        
        ## Multi Cores
        x = func(times[i],eval(dfunc[[j]]), range[[j]], mc = TRUE )
        table[j, 12] = score(x, get(dfunc[[j]]))    
      } 
      
    }
    
  }
  return(table)
}


## Define distributions and scoring function

## Beta
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}


## Truncated Normal
dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}


## Truncated Exponential
dtexp = function(x)
{
  ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)
}


## Uniform Mixture 
dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}


## Truncated Normal Mixture 1
dtnorm_mix1 = function(x)
{
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.9206407)
}


## Truncated Normal Mixture 2
dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.55)
}

## Score function
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


tabs = as.data.frame(matrix(rep(0,12), ncol=12))
names(tabs)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")


funcs = c(R,mh,reject,slice) 
names=  c("R","mh","reject","slice") 
for (i in 1:length(names)){
  tabs = rbind(tabs,getTable(names[[i]],eval(funcs[[i]])))
}
tabs=tabs[-1,]
saveRDS(tabs, "table.Rdata")