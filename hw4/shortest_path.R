shortest_path = function(g, v1, v2)
{
  if(is_valid(g,v1,v2)){
    if(check_vertex(g,v1,v2)){
      if(v1==v2){ ##check to see if start and finish are the same
        return(c(v1,v2))
      }
    }
    return(FALSE)
  }
}

check_vertex = function(g,v1,v2){
  if(is.element(v1, names(g)) && is.element(v1, names(g))){
    return(TRUE)
  }
  return(FALSE)
}

(define lookup-vertex
 (lambda (vname vlist)
  (cond ((null? vlist) #f)
         ((equal? vname (name (first vlist))) (first vlist)) ;replaced car with first
         (else (lookup-vertex vname (rest vlist)))))) ;replaced cdr with rest
 
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

