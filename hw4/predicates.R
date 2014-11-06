## Testing
graph1 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,-1,0)))


graph2 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,22,11)))
is_valid(graph1)
test(graph1)

g11 <- sapply(graph1, "[[", 1) # Grab first list element of n number of secondary lists (edges list). 
g12 <- sapply(graph1, "[[", 2) # Grab first list element of n number of secondary lists (edges list). 
g22 <- sapply(graph2, "[[", 1) # Grab first list element of n number of secondary lists (weights list). 
g22 <- sapply(graph2, "[[", 2) # Grab first list element of n number of secondary lists (weights list). 
test(g12)
is_valid(graph1) # Should be false
is_valid(graph2) # Should be true

sapply(sapply(y, function(m) m <= 0), all, <=0) # Doesn't work.
sapply(y, function(m) m <= 0) == c("TRUE") # Doesn't work.

sapply(y, function(m) m <= 0) # Evaluates each element of each secondary list correctly.
sapply(sapply(y, function(m) m <= 0), any) # Returns consensus value for each secondary list. 
all(sapply(sapply(g12, function(m) m <= 0), any) == c("FALSE")) # Check if all weights lists have values <= 0. If TRUE, return TRUE.
test(g12)

is_valid(graph1)
all(sapply(sapply(graph2, function(m) m <= 0), any) == c("FALSE"))
test(y)
test <- function(a) {
  if (all(sapply(sapply(a, function(m) m <= 0), any) == c("FALSE"))) # Check if all weights lists have values <= 0.
    return(TRUE)
  return(FALSE)
}

is_valid(graph1)

is_valid(graph2)

is_valid = function(g) 
{
  ## Check that object is a list of lists.
  if (all( c(class(g), sapply(g, class)) == "list" )) # Ensures that each element of g is a list.
    ## Check if there are names for the primary list and that they are all unique.
    if (length(unique(names(g))) == length(names(g)))
      ## Check that each secondary list contains only edges and weights vectors 
      ## that are of the appropriate type.
      if (all( c(sapply(g, names)) == c("edges", "weights")))  
        ## Check that there are not any edges to non-existent vertices. 
        if (all(sapply(sapply(sapply(g, "[[", 1), is.element, 1:length(g)), all, 1:length(g)) == c("TRUE"))) # Works!
          ## Check that all weights are not less than or equal to 0. 
          if (all(sapply(sapply(sapply(g, "[[", 2), function(m) m <= 0), any) == c("FALSE"))) # Check if all weights lists have values <= 0.
            ## Check that every edge has a weight.
            return(TRUE)
          else
            return(FALSE)
        else
          return(FALSE)
            
      else
        # print("Secondary list does not only contain edge and weight vectors.") # Can't include print() --> error msg: unexpected 'else'.
        return(FALSE)
    else 
      return(FALSE) # The number of uniques does not equal the total number of list elements => not all unique.
  else
    return(FALSE)
}

is_undirected = function(g)
{
  ## Check if the graph object is undirected, this is true if all directed
  ## edges have a complementary directed edge with the same weight in the 
  ## opposite direction.
  TRUE
}

is_isomorphic = function(g1, g2)
{
  TRUE
}

is_connected = function(g, v1, v2)
{
  TRUE
}