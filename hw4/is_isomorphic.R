library(testthat)

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
    match = matrix(0, nrow = length(g1), ncol = 1) ## mark whether edges are the same
    
    for(j in 1:length(g2)){
      if(names(g1[i]) == names(g2[j])) {
        flag = 1 ## find out that vertex in graph2
            
        if(all(names(g1)[g1[[i]]$edges] == names(g2)[g2[[j]]$edges]))
          match[i,1] = 1 ## the names of edges are the same
        else return(FALSE)      

        o1 = order(g1[[i]]$edges)
        o2 = order(g2[[j]]$edges)
        
        if(any(g1[[i]]$weights[o1] != g2[[j]]$weights[o2]))
          return(FALSE)
      }
    }
        
  }
  
  if(flag != 1){
    return(FALSE)
  }
  
  if(all(match == 1))
  return(TRUE)
}

#         ## compare related edges
#         if(length(g1[[i]]$edges)!= length(g2[[j]]$edges))
#             return(FALSE)
#         
#         all_match = 0
#         
#         for(k in 1:length(g1[[i]]$edges)){
#             
#             match = 0  
#           
#             for(p in 1:length(g2[[j]]$edges)){
#               if(names(g1)[g1[[i]]$edges[k]] == names(g2)[g2[[j]]$edges[p]])
#                 match = match + 1
#             }
#             
#             if(match == length(g1[[i]]$edges) && match == length(g2[[j]]$edges))
#               all_match = 1
#         }
#         
#         if(all_match == 1)
#           return(TRUE)
#         else return(FALSE)


#         
#         if(g1 == g5 && g2 == g6) o2 = rev(order(g2[[j]]$edges))
