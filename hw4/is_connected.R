is_connected <- function(g,v1,v2){
  
  ##first ensure the graph is valid
  if(!is_valid(g)) stop('error')
  
  ##second find out which one is v1
  
  for(i in length(g)){
    if(names(g[i]) == v1){
      index = i
      break
    }
  }
  
  ##traverse function searching through points, v is the index
  traverse = function(g, v, visited = integer())
  {
    visited = c(visited, v)
    
    o1 = order(g[[v]]$edges)
    
    for(i in 1:length(g[[v]]$edges)){
      
      visited = c(visited, g[[v]]$edges[o1][i])
      
      if(any(names(g[[v]]$edges[o1])) == v2){
        return(TRUE)
      } else {
        traverse(g,g[[v]]$edges[o1])
      }
    }
  }
  
  ##traverse from v1
  traverse(g, index)
  
  ##if there is not true, then it should be false
  return(FALSE)
    
}