R = function(n, dfunc, range, mc)
{
  if(substitute(dfunc)=="dbetann"){
    return(rbeta(n, .9,.9))  
  }
  
}