library(stringr)
write_graph <- function(graph, file)
{
  ## Check if the graph is valid
  stopifnot(is_valid(graph))
  ## Check if the directory exists, stop if it doesn't.
  dir <- str_extract(file, '^.+\\/')
  if (!is.na(dir) && !file.exists(dir))
  {
    stop("The directory you're trying to write to does not exist!")
  }
  ## Check if the file exists, stop if it does.
  if (file.exists(file))
  {
    stop("The file already exists! Cannot overwrite!")
  }
  
  file.create(file, showWarnings = TRUE)
  
  ## Check if the vertices are labelled, if not label them with their indices
  for (k in 1:length(graph))
  {
    if (names(graph)[k] == ""|is.null(names(graph)[k])) 
    {
      names(graph)[k] <- as.character(k)
    } else if (!is.na(str_match(names(graph)[k], " "))) # If the names are multiple words, put them in quotes
    {
      names(graph)[k] <- paste0('"',names(graph)[k],'"' )
    }
    
  }
  
  ## Check if the edges are weighted. Write into different formats accordingly.
  weights <- NULL
  for ( i in 1:length(graph))
  {
    weights <- c(weights, graph[[i]]$weights)
  }
  
  ## If all the weights are equal to 1, write the lines without weights
  if (length(unique(weights))==1 && unique(weights)==1)
  {
    for (i in 1:length(graph))
    {
      ## Check if the vertex has edges
      if (length(graph[[i]]$edges) > 0)
      {
        for ( j in 1:length(graph[[i]]$edges) )
        {
          from <- names(graph)[i]      
          to <- names(graph)[as.numeric(graph[[i]]$edges[j])]
          line <- paste0(from, " -> ",to,";")
          write(line, file = file,  append=TRUE)
        }
      }   
    }
  } else # If the edges are weighted, include weight in the lines
  {
    for (i in 1:length(graph))
    {
      ## Check if the vertex has edges
      if (length(graph[[i]]$edges) > 0)
      {
        for ( j in 1:length(graph[[i]]$edges) )
        {
          from <- names(graph)[i]      
          to <- names(graph)[as.numeric(graph[[i]]$edges[j])]
          weight <- as.numeric(graph[[i]]$weights[j])
          line <- paste0(from, " -> ",to,
                        " " ,"[weight=", weight,
                        "];")
          write(line, file=file,  append=TRUE)
        }
      } else
      {
        from <- names(graph)[i]
        line <- paste0(from,";")
        write(line, file=file,  append=TRUE)
      }
    }
  }
}