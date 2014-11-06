## Testing
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
is_valid(graph2)
class(a)
c(class(a), sapply(a, class))

c(names(a), sapply(a, names))
c(names(graph2), sapply(graph2, names))
c(sapply(graph2, "[[", 1)) # Extract the first element of each secondary-list.
all( c(sapply(graph2, "[[", 1)) %in% 1:length(graph2))

all( (sapply(graph2, "[[", 1)) %in% 1:5) # Doesn't work.
all( (sapply(graph2, "[[", 1)) 1:5) # Doesn't work.


z <- sapply(graph2, "[[", 1)
sapply(sapply(graph2, "[[", 1), is.element, 1:5) == c("TRUE")

sapply(graph2, "[[", 1) # Grab first of n number of secondary lists (edges list). 

all(sapply(z, function(z) z %in% 1:5)) # Doesn't work.
all(sapply(z, function(z) z %in% 1:5) == c("TRUE")) # Doesn't work.

sapply(z, function(z) z %in% 1:5) # Works.
all(sapply(z[2], is.element, 1:5) == c("TRUE")) # Works.
all(sapply(z[1], is.element, 1:5) == c("TRUE")) # Substitute [1], 1:5
all(vapply(z, is.element, 1:5) == c("TRUE")) # Substitute [1], 1:5
all(sapply(z, is.element, 1:5) == c("TRUE")) # Substitute [1], 1:5
?vapply

all(z[[2]] %in% 1:5) # Works.


is_valid = function(g) 
{
  ## Check that object is a list of lists.
  if (all( c(class(g), sapply(g, class)) == "list" )) # Ensures that each element of g is a list.
    ## Check if there are names for the primary list and that they are all unique.
    if (length(unique(names(g))) == length(names(g)))
      ## Check that each secondary list contains only edges and weights vectors 
      ## that are of the appropriate type.
      if (all( c(sapply(g, names)) == c("edges", "weights")))  
        return(TRUE)
        ## Check that there are not any edges to non-existent vertices. 
        ## Check that all weights are not less than or equal to 0. 
        ## Check that every edge has a weight.  
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