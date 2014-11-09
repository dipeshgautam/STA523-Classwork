g2 = list(A = list(edges   = c(2L,4L,5L),
                   weights = c(1 ,4 ,3 )),
          B = list(edges   = c(1L,4L,5L),
                   weights = c(1 ,4 ,2 )),
          C = list(edges   = c(5L,6L),
                   weights = c(4 ,5 )),
          D = list(edges   = c(1L,2L,5L),
                   weights = c(4 ,4 ,4 )),
          E = list(edges   = c(1L,2L,3L,4L,6L),
                   weights = c(3 ,2 ,4 ,4 ,7 )),
          F = list(edges   = c(3L,5L),
                   weights = c(5 ,7 )))

shortest_path = function(g, v1, v2)
{
  x=function(){
    #if(is_valid(g,v1,v2)){
    if(check_vertex(g,v1,v2)){
      if(v1==v2){ ##check to see if start and finish are the same
        return(0)
      }
      else{
        dist=list()
        paths=find_path(g,v1,v2)
        valid=list()
        for(i in paths){
          for (j in i){
            x=verify_path(g,j)
            if(x[1]!=FALSE){
              valid=append(valid,list(j))
            }
          }
          
        }
        valid=unique(valid)
        for(j in valid){
          dist=append(dist,get_distance(g,j))
        }
        mini = match(lapply(dist,min),dist)
        fin= valid[mini]
        return(fin)
      }
    }
    stop("Vertex labels")
    #}
    #stop("Not a valid graph")
  }
  return(x())
}

get_distance= function(g,lst){
  L=lst
  loop=function(lst,dist){
    if(length(lst)==1){
      return(dist)
    }  
    else{
      indice= match(lst[2], names(g))
      edgeIndex=match(indice,g[[lst[1]]]$edges)
      dist= dist+ g[[lst[1]]]$weights[edgeIndex]
      loop(lst[2:length(lst)],dist)
    }
  }
  loop(L,0)
}

verify_path= function(g,lst){
  if(length(lst)==0){
    return(FALSE)
  }
  else{
    loop=function(L){
      if (length(L)==1){
        return(TRUE)
      }
      else{
        if(is.element(match(L[[2]],names(g)),g[[L[[1]]]]$edges)){
          loop(L[2:length(L)])
        }
        else{
          return(FALSE)
        }
      }
    }
    loop(lst)
  }
}

find_path = function(g,v1,v2){
  if (v1==v2){
    return(v2)
  }
  else{
    all=all_combos(names(g))
    match = find_matches(all,v1,v2)
    return(match)
  }
}

find_matches=function(set, v1,v2){
  x= lapply(set,v1,v2, FUN= matches)
  return(unique(x))
}


matches= function(g,v1,v2){
  m = lapply(as.list(data.frame(t(g))), function(x){
    if(length(x[[1]])>1){ 
      #       print(x[[1]])
      #       print(c("this is v1",v1))
      #       print(x[[1]][[1]])
      #       print(c("this is v2",v2))
      #       print(x[[1]][[length(x[[1]])]])
      if(x[[1]][[1]]==v1 && x[[1]][[length(x[[1]])]]==v2){
        #         print("YO")
        return(x[[1]])
      }
    }
  })
  if(length(m)>0){
    m=m[-(which(sapply(m,is.null),arr.ind=TRUE))]
    return(m)
  }
}

check_vertex = function(g,v1,v2){
  if(is.element(v1, names(g)) && is.element(v1, names(g))){
    return(TRUE)
  }
  return(FALSE)
}

all_combos=function(set){
  return(lapply(power_set(set), FUN=permutations))
}

power_set <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

permutations=function(lst){
  return(expand.grid(rep(list(power_set(lst)))))
}

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
