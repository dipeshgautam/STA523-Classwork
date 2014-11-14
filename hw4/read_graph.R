library(stringr)
read_graph <- function(file)
{
  ## Check if the file exists.
  stopifnot(file.exists(file))
  a <- readLines(file)
  ## Create a list of approved structure for a line of dot file
  valid.format <- c("^[A-Z0-9]+ -> [A-Z0-9]+ \\[weight=[0-9e.+]+\\];", 
                    "^[A-Z0-9]+ -> [A-Z0-9]+;",
                    "^[A-Z0-9]+;",
                    "^\".+\" -> \".+\" \\[weight=[0-9e.+]+\\];",
                    "^\".+\" -> [A-Z0-9]+ \\[weight=[0-9e.+]+\\];",
                    "^[A-Z0-9]+ -> \".+\" \\[weight=[0-9e.+]+\\];",
                    "^\".+\" -> \".+\";", 
                    "^\".+\" -> [A-Z0-9]+;", 
                    "^[A-Z0-9]+ -> \".+\";",
                    "^\".+\";"
  )
  
  ## Check if each line is within given specification
  for (k in 1:length(a))
  {
    valid <- FALSE
    for (l in 1:length(valid.format))
    {
      if (!is.na(str_match(a[k], valid.format[l]) ) )
      {
        valid <- TRUE
        break
      }
    }
    if(!valid)
    {
      stop("The file is not properly formatted!")
    }
  }
  
  ## extract from and to vertices and weights as lists from the file
  from <- NULL
  to <- NULL
  weights <- NULL
  for(k in 1:length(a)){
    if(!is.na(str_match(a[k], "^.+ -> .+ \\[weight=[0-9e.+]*\\];") ))
    {
      from[k] <- unlist(str_match(a[k], "^(.+) ->"))[2]
      to[k] <- unlist(str_match(a[k], "-> (.+) \\["))[2]
      weights[k] <- unlist(str_match(a[k], "weight=([0-9e.+]*)\\]"))[2]
    } else if (!is.na(str_match(a[k], "^.+ -> .+;") ))
    {
      from[k] <- unlist(str_match(a[k], "^(.+) ->"))[2]
      to[k] <- unlist(str_match(a[k], "-> (.+);"))[2]
      weights[k] <- 1
    } else 
    {
      from[k] <- unlist(str_match(a[k], "^(.+);"))[2]
      to[k] <- ""
      weights[k] <- ""
    }
  }
  
  ## If there is edge from vertex A to B but B doesn't have any edges, it's valid to not
  ## include it in the file and it's manually added below.
  empty.to <- rep("", length(setdiff(to[!to==""],from) ) ) # Check if there are unaccounted destination vertices, if there are get their number
  from <- c(from, setdiff(to[!to==""],from)) # Add the unaccounted edge as a vertix
  to <- c(to, empty.to)
  
  ## Create a list of unique starting vertices
  unique.from <- unique(from)
  
  ## Convert names of destination vertices into numeric indices
  for (j in 1:length(to))
  {
    if (!to[j]=="")
    {
      to[j] <- as.integer(which(unique.from==to[j]))
    }
  }
  
  graph <- list(list(list(), list() ))
  for (i in 1:length(unique.from))
  {
    to.i <- to[which(from==unique.from[i])]
    weights.i <- as.numeric(weights[which(from==unique.from[i])])
    
    if (length(to.i) == 1 && to.i== "")
    {
      list.i <- list(list(edges = integer(),
                          weights = numeric()))
    } else
    {
      list.i <- list(list(edges = as.integer(c(to.i)),
                          weights = c(weights.i)))
    }
    
    graph[i] <- list.i
    names(graph)[i] <- str_extract(unique.from[i], ignore.case("[^\"]+")) 
    
  }
  return(graph)  
}