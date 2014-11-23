getTable= function(name, func){
  table = as.data.frame(matrix(rep(0,12*6), ncol=12))
  names(table)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")
  
  times <- c(100, 10000, 1000000, 10000000)
  dfunc <- list("dbetann", "dtnorm", "dtexp", "dunif_mix", "dtnorm_mix1", "dtnorm_mix2")
  range <- list( c(0,1), c(-3,3), c(0,6), c(-3, 4), c(0,10), c(-4,4))
  table[,2] = name
  for (i in 1:length(times)) {
    for (j in 1:length(dfunc)) {
      table[j,1]=dfunc[[j]]
      table[j, 2+i] = system.time(func(times[i],dfunc[[j]], range[[j]], mc = FALSE ) )[3] # 1.087
      print(c(j,i))   
      
      ## Multiple cores
      table[j, 6+i]= system.time(func(times[i],eval(dfunc[[j]]), range[[j]] , mc = TRUE) )[3] # 0.734
      if (i==3){
        ## Single Core
        x = func(times[i],eval(dfunc[[j]]), range[[j]], mc = FALSE )
        table[j, 11] = score(x,get(dfunc[[j]]))
        
        ## Multi Cores
        x = func(times[i],eval(dfunc[[j]]), range[[j]], mc = TRUE )
        table[j, 12] = score(x, get(dfunc[[j]]))    
      } 
      
    }
    
  }
  return(table)
}

tabs = as.data.frame(matrix(rep(0,12), ncol=12))
names(tabs)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")


funcs = c(R,mh,reject,slice) 
names=  c("R","mh","reject","slice") 
for (i in 1:length(names)){
  tabs = rbind(tabs,getTable(names[[i]],eval(funcs[[i]])))
}
