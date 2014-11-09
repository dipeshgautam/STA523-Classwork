is_connected <- function(g,v1,v2){
  
  ##first ensure the graph is valid
  if(!is_valid(g)) stop('error')
  
  ##second check that v1 or v2 is in g or not
  findv1 = 0
  findv2 = 0
  flag = 0
  
  ##since v1,v2 can be numeric, integer and chracter, check which type of the v1 and v2 and transform them into character
  if(is.logical(v1) | is.logical(v2)){
    stop('error')    
  }
  
  if(!is.character(v1)){
    v1 = names(g[v1])
  }
  
  if(!is.character(v2)){
    v2 = names(g[v2])
  }
  
  for(i in 1:length(g)){
    if(names(g[i]) == v1){
      findv1 = 1
    }
    
    if(names(g[i]) == v2){
      findv2 = 1
    }
  }
  
  if(findv1 == 0|findv2 == 0){
    stop('error')
  }
  
  if(findv1 == 1 && findv2 == 1){
    flag = 1
  }
  
  if(!flag)
    return(FALSE)
  
  ##third find out which one is v1 and which one is v2, record their index
  for(i in 1:length(g)){
    if(names(g[i]) == v1){
      index1 = i
    }
    if(names(g[i]) == v2){
      index2 = i      
    }
  }
  
  #special case:v1 = v2
  if(v1 == v2){
    if(length(g[[index1]]$edges) == 0){ 
      return(FALSE)
    } else {
      if(any(g[[index1]]$edges == index2) ){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  
  ##initialize a queue to save all the points
  queue = integer()
  num = 0 ##calculate the number of points in the queue
  point = 1 ##record the first point
  
  ##traverse function searching through points, v is the index of v1
  traverse = function(g, v, visited = integer())
  {
    
    if(v == index2)
      return(TRUE)
    
    visited = c(visited, v)
    
    queue = c(queue, v)
    num = num + 1
    
    while(num != 0){
      a <- queue[point]
      
      point <- point + 1
      
      ##v1 is the last point and have no related points
      if(length(g[[a]]$edges) == 0)
        return(FALSE)
      
      for(i in 1:length(g[[a]]$edges)){
        if(g[[a]]$edges[i] %in% setdiff(g[[a]]$edges, visited)){
          
          if(g[[a]]$edges[i] == index2) return(TRUE)
          visited <- c(visited, g[[a]]$edges[i])
          queue <- c(queue,g[[a]]$edges[i])
          num = num + 1   
          
          ##cat("a, i, point, num",a,i,g[[a]]$edge[i],num,"\n")
        } 
      }
    }  
  }
  
  ##traverse from v1
  if(traverse(g, index1)) return(TRUE)
  
  ##if there is not true, then it should be false
  return(FALSE)
  
}

