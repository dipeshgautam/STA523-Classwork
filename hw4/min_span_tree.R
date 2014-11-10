g = list(A = list(edges   = c(2L,4L,5L),
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

min_span_tree=function(g){
  x=function(){
    #if(is_valid(g,v1,v2)){
    minDist=0
    dist=list()
    pathsFull=list()
    
    if(length(names(g))==0){
      names(g)=LETTERS[1:length(g)]
    }
    for (i in two_combos(names(g))){      
      if(length(i)>1){
        v1=as.character(i[1])
        v2=as.character(i[2])
        if(check_vertex(g,v1,v2)){
          if(v1!=v2){ 
            paths=find_path(g,v1,v2)
            valid=list()
            for(i in paths){
              for (j in i){
                x=verify_path(g,as.vector(j))
                if(x!=FALSE){
                  valid=append(valid,list(j))
                }
              }
            }
            valid=unique(valid)
            for(j in valid){
              if(length(j)==length(names(g))){
                dist=append(dist,get_distance(g,j))
                pathsFull=append(pathsFull,list(j))
              }
            }
          }
        }
      }
    }
    if(length(dist)>0){
      minDist= min(rapply(dist,min))
      mini = match(minDist,dist)
      fin= pathsFull[mini]
      return(fin)
    }
  }
  
  
  r=x()[[1]]
  if(length(r)==0){
    if(is.element(1,g[[1]]$edges)){
      g[[1]]$edges=integer()
      g[[1]]$weights=numeric()
    }
    return(g)
  }
  else{
    return(create_graph(g,r))
  }
}
create_graph= function(g,r){
  tmp=g
  if(length(names(g))==0){
    names(g)=LETTERS[1:length(g)]
  }
  for(i in 1:length(r)){
    nextVal= match(r[i+1], names(g))
    prevVal = match(r[i-1], names(g))
    ind= match(nextVal, g[[r[i]]]$edges)
    indPrev=match(prevVal, g[[r[i]]]$edges)
    if(i>1 &&i<length(r)){
      tmp[[i]]$edges=c(nextVal,prevVal)
      tmp[[i]]$weights= c(g[[r[i]]]$weights[ind],g[[r[i]]]$weights[indPrev])
    }
    else if(i==length(r) ){
      tmp[[i]]$edges=prevVal
      tmp[[i]]$weights= g[[r[i]]]$weights[indPrev]
    }
    else if(i==1){
      tmp[[i]]$edges=nextVal
      tmp[[i]]$weights= g[[r[i]]]$weights[ind]
    }
  }
  names(tmp)=r
  return(tmp)
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
  temp= lapply(set, FUN= function(x){
    if(length(x)!=0){
      return(matches(x,v1,v2))
    }})
  return(unique(temp))
}


matches= function(g,v1,v2){
  m = lapply(as.list(data.frame(t(g))), function(x){
    if(length(x)>1){ 
      if(x[1]==v1 && x[length(x)]==v2){
        return(x)
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
  return(lapply(power_set(set), FUN=function(x){
    if(length(x)>0){
      return(matrix(x[permutations(length(x))],ncol=length(x)))
    }
    
  }))
}

power_set <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}


permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}
two_combos=function(set){
  all = all_combos(set)
  twos=lapply(all, FUN=function(x){
    if(length(x)>1){
      for (i in as.list(as.data.frame(t(x)))){
        if(length(i)==2){
          return(i)
        }
      }
    }
  })
  return(unique(twos))
}

x=lapply(all, FUN=function(x){
  if(length(x)>1){
    for (i in as.list(as.data.frame(t(x)))){
      if(length(i)==2){
        return(i)
      }
    }
  }
})
