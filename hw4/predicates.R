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
                  else
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
  if (length(sapply(sapply(g, function(x) x[["weights"]]), length)) == 1 & sapply(sapply(g0, function(x) x[["weights"]]), length) == 0) {
    print("Found loop. Last vertice has NULL edge(s) and/or weight(s).")
    return(TRUE)
  }
  else {
  ## Check if the graph object is undirected, this is true if all directed
  ## edges have a complementary directed edge with the same weight in the 
  ## opposite direction.
    traverse = function(g, v, visited = integer())
    {
      for (v in 1:length(g) ) {
        visited <- c(visited, g[[v]]$edges)
        if (any(g[[v]]$edges %in% visited)) {
          print(paste0("True. Found loop. Already visited edge.", g[[v]]$edges %in% visited))
          return(TRUE)
        }
        for (e in g[[v]]$edges) { # I have a new edge and want to traverse it.
          if (traverse(g, e, visited)) { # Going to new vertex e, and keeping track of "visited"
            print("True. Found a loop.")
            return(TRUE) # Found a loop. Exit if().
          }
        }
        if (length(sapply(sapply(g, function(x) x[["weights"]]), length)) == 1 & sapply(sapply(g0, function(x) x[["weights"]]), length) == 0) {
          print("True. Found a loop.")
          return(TRUE)
        } 
        print("False. No loop found.")
        return(FALSE) # If I haven't found a loop above, there is no loop.
      } 
    }
    print("False. No loop found.")
    return(FALSE)
  }
}

is_isomorphic = function(g1, g2)
{
  TRUE
}

is_connected = function(g, v1, v2)
{
  TRUE
}