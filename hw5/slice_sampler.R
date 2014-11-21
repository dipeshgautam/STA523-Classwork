
slice = function(n, dfunc, range, mc=FALSE)
{
  set.seed(1000)
  ## Define initial width
  w = (range[2]-range[1])/10
  
  sample = function(m, dfunc, range, w){
    ## Sample initial x0 within the range
    x0 = runif(1, range[1], range[2])
    X = rep(0,n)
    
    for (i in 1:m)
    {
      ## Sample y from the range (0, f(x0))
      y = runif(1, 0, dfunc(x0))
      
      ## Create initial area around x0
      range2 = c(x0-w,x0+w)
      
      ## Check if the left end is just outside the slice, if not expand the interval until it is
      while (y<=dfunc(range2[1])){
        range2[1] = range2[1]-w
      }
      ## Check if the right end is just outside the slice, if not expand the interval until it is
      while (y<=dfunc(range2[2])){
        range2[2] = range2[2]+w
      }
      
      
      ## Sample x1 from the interval
      x1 = runif(1, range2[1], range2[2])
      
      ## Check if the sampled x1 is outside the range, it is sample again with x1 as the boundary of the range
      while (dfunc(x1)<=y){
        if (x1<x0){
          range2[1] = x1
          
        } else
        {
          range2[2] = x1        
        }
        x1 = runif(1, range2[1], range2[2])
      }
      
      ## Set reset x0 to the newly sampled value and store the result.
      x0 = x1
      X[i] = x1    
    }
    return(X)
  }
  
  if (mc==TRUE){
    
    
  }else{
    sample(n, dfunc, range, w)
  }
  
  
}
