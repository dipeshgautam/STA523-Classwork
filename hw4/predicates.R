setwd("~/Team2/hw4/tests")
library(testthat)
source_dir("..", env=globalenv() )

##################### Testing ####################################################################################
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

graph3 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,22)))

is_valid(graph1) # Should be false
is_valid(graph2) # Should be true
is_valid(graph3) # Should be false
#########################################################################################################
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
                if (length(sapply(g, function(x) x[["edges"]])) == length(unique(sapply(g, function(x) x[["edges"]])))) # Use length() instead of sum().
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

is_undirected = function(g)
{
  ## Graphs with NULL edges and weights are undirected.
  test <- function(g) g["weights"]
  if (test(g) == c("NULL")) {
    print("Found loop.")
    return(TRUE)
  }
  ## Check if the graph object is undirected, this is true if all directed
  ## edges have a complementary directed edge with the same weight in the 
  ## opposite direction.
  traverse = function(g, v, visited = integer())
  {
    for (v in 1:length(g) ) {
      visited <- c(visited, g[[v]]$edges)
      if (any(g[[v]]$edges %in% visited)) {
        print("Found loop.")
        return(TRUE)
      }
      for (e in g[[v]]$edges) { # I have a new edge and want to traverse it.
        if (traverse(g, e, visited)) { # Going to new vertex e, and keeping track of "visited"
          print("Found a loop.")
          return(TRUE) # Found a loop. Exit if().
        }
      }
      print("No loop found.")
      return(FALSE) # If I haven't found a loop above, there is no loop.
    } 
  }
}

## Failing the following tests:
test_that("Directed - Edges",{
  g1 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = integer(),
                 weights = numeric()))
  
  g2 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(2L),
                 weights = c(1)))
  
  expect_false(is_undirected(g1))
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

is_isomorphic = function(g1, g2)
{
  TRUE
}

is_connected = function(g, v1, v2)
{
  TRUE
}