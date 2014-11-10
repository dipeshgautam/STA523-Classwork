setwd("~/Team2/hw4/tests")
library(testthat)
source_dir("..", env=globalenv() )

is_valid = function(g) 
{
  ## Check that object is a list of lists.
  if (all( c(class(g), sapply(g, class)) == "list" )) # Ensures that each element of g is a list.
    ## Check for duplicate vertex labels. if there are names for the primary list and that they are all unique.
    if (length(unique(names(g))) == length(names(g)))
      ## Check that each secondary list contains only edges and weights vectors 
      ## that are of the appropriate type. Ensures structure is good.
      if (all( c(sapply(g, names)) == c("edges", "weights") | c(sapply(g, names)) == c("weights", "edges") )) # "|" for when weights are listed first
        ## Check for invalid vertex references / edges to non-existent vertices.
        if (all(sapply(sapply(sapply(g, "[[", 1), is.element, 1:length(g)), all, 1:length(g)) == c("TRUE"))) # Works!
          ## Check that all weights are not less than or equal to 0. 
          if (all(is.na(sapply(g, function(x) x[["weights"]])) == "FALSE") & all(sapply(sapply(sapply(g, function(x) x[["weights"]]), function(m) m <= 0), any) == c("FALSE"))) # Check if all weights lists have values <= 0. If TRUE, return TRUE.
            ## Check that every edge has a weight.
            if (sum(sapply(sapply(g, function(x) x[["edges"]]), length)) == sum(sapply(sapply(g, function(x) x[["weights"]]), length)))
            #if (sum(sapply(sapply(g, "[[", 1), length)) == sum(sapply(sapply(g, "[[", 2), length))) # Step 5
              ## Check that all edges are integer type. 
              if (all(sapply(sapply(g, function(x) x[["edges"]]), typeof) == "integer") == c("TRUE"))
                ## Check for duplicate edges.
                if (length(sapply(g, function(x) x[["edges"]])) == length(sapply(sapply(g, function(x) x[["edges"]]), unique)))
                    return(TRUE)
                  else {
                    print("Duplicate edges.")
                    return(FALSE)
                }
              else {
                print("Edge(s) not integer type.")
                return(FALSE)
              }
            else {
              print("Edge count and weight count do not match.")
              return(FALSE)
            }
          else {
            print("One or more weights less than or equal to 0, or NA.")
            return(FALSE)
          }
        else {
          print("Edge(s) to non-existent vertice(s).")
          return(FALSE)
        }
      else {
        print("Secondary list does not only contain edge and weight vectors.")
        return(FALSE)
      }
    else {
      print("Duplicate names in primary list.")
      return(FALSE) # The number of uniques does not equal the total number of list elements => not all unique.
    }
  else {
    print("Object not list of lists.")
    return(FALSE)
  }
}

is_undirected = function(g) {
  ## Graphs with one vertice that has NULL a edge and weight is undirected.
  if (length(sapply(sapply(g, function(x) x[["weights"]]), length)) == 1 & sapply(sapply(g, function(x) x[["weights"]]), length) == 0) {
    print("Found loop. Last vertice has NULL edge(s) and/or weight(s).")
    return(TRUE)
  }
  else 
  {
    if (sapply(sapply(g, function(x) x[["weights"]]), length) == 1) 
    {
      print("True. Found a loop.")
      return(TRUE)
    }
    else {
        if (all(sapply(sapply(g, function(x) x[["weights"]]), length) == 0) | all(sapply(sapply(g, function(x) x[["edges"]]), length) == 0)) {
          print("True. Loop found. Two or more edge/weights are NULL.")
          return(TRUE)
        }
        else {
          print("False. No loop found.")
          return(FALSE)
        }
        
        }
    }
  }

is_undirected = function(g)
{
  stopifnot(is_valid(g))
  traverse = function(g, v, visited = integer())
  {
    visited = c(visited, v)
    if (any(g[[v]]$edges %in% visited))
      return(TRUE)
    for(e in setdiff(g[[v]]$edges,visited))
    {
      if (traverse(g, e, visited))
        return(TRUE)
    }
    return(FALSE)
  }
  for(i in 1:length(g))
  {
    if (traverse(g, i))
      return(TRUE)
    
  }
  if (length(sapply(sapply(g, function(x) x[["weights"]]), length)) == 1 & sapply(sapply(g, function(x) x[["weights"]]), length) == 0) {
    print("Found loop. Last vertice has NULL edge(s) and/or weight(s).")
    return(TRUE)
  }
  return(FALSE)
}


## has_loop
is_undirected = function(g)
{
  if (all(sapply(sapply(g, function(x) x[["weights"]]), length) == 0) | all(sapply(sapply(g, function(x) x[["edges"]]), length) == 0)) {
    print("True. Loop found. Two or more edge/weights are NULL.")
    return(TRUE)
  }
  if (length(sapply(sapply(g, function(x) x[["weights"]]), length)) == 1 & sapply(sapply(g, function(x) x[["weights"]]), length) == 0) {
    print("Found loop. Last vertice has NULL edge(s) and/or weight(s).")
    return(TRUE)
  }
  
  stopifnot(is_valid(g))
  
  traverse = function(g, v, visited = integer())
  {
    
    visited = c(visited, v)
    if (any(g[[v]]$edges %in% visited)) {
      print(paste0("True. Found loop. Already visited edge.", g[[v]]$edges %in% visited))
      return(TRUE)
    }
    for(e in setdiff(g[[v]]$edges,visited)) # I have a new edge and want to traverse it.
    {
      if (traverse(g, e, visited)) { # Going to new vertex e, and keeping track of "visited"
        print("True. Found a loop.")
        return(TRUE)
      }
    }
    return(FALSE)
  }
  for(i in 1:length(g))
  {
    if (traverse(g, i))
      return(TRUE)
  }
  return(FALSE)
}


test_that("Directed - Edges",{
  g2 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(2L),
                 weights = c(1)))
  expect_false(is_undirected(g2))
})

test_that("Directed - Weights",{
  g1 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = c(1L),
                 weights = c(2)))
  
  g2 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(1L,2L),
                 weights = c(2,1)))
  
  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))   
})







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
  
  match = matrix(0, nrow = length(g1), ncol = 1) ## mark whether edges are the same
  ##then compare each vertix and its related edges and weights
  for(i in 1:length(g1)){
    
    flag = 0   
    for(j in 1:length(g2)){
      if(names(g1[i]) == names(g2[j])) {
        flag = 1
        ## find out that vertex in graph2
        if (length(setdiff(names(g1)[g1[[i]]$edges], names(g2)[g2[[j]]$edges]))==0 &&
              length(setdiff(names(g2)[g2[[j]]$edges], names(g1)[g1[[i]]$edges]))==0)
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