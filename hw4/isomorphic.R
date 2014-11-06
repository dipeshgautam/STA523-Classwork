is_isomorphic <- function(g1,g2) {
  
  ##check the graph whether it is valid or not
  if(!is_valid(g1)) stop('error')
  if(!is_valid(g2)) stop('error')
  
  ##first check whether it is empty or not
  if(length(g1) == 1 && length(g1[[1]]$edges) == 0) {
    if(length(g2) == 1 && length(g2[[1]]$edges) == 0)
      return(TRUE)
    
    return(FALSE)
  }
  
  ##second calculate number of vertices, if not equal, return false
  if(length(g1) != length(g2))
    return(FALSE)
  
  ##then compare each vertix and its related edges and weights
  for(i in 1:length(g1)){
    
    flag = 0
    for(j in 1:length(g2)){
      if(names(g1[i]) == names(g2[j])) {
        flag = 1
        
        o1 = order(g1[[i]]$edges)
        o2 = order(g2[[i]]$edges)
        
        if(any(names(g1)[g1[[i]]$edges[o1]] != names(g2)[g2[[j]]$edges[o2]]))
          return(FALSE)
        
        if(any(g1[[i]]$weights[o1] != g2[[j]]$weights[o2]))
          return(FALSE)
      }
    }
  }
  
  if(flag != 1){
    return(FALSE)
  }
  
  return(TRUE)
}




