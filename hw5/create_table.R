getTable= function(func){
  table = as.data.frame(matrix(rep(0,12*6), ncol=12))
  names(table)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")
  
  times <- c(100, 10000, 1000000, 10000000)
  dfunc <- list("dbetann", "dtnorm", "dtexp", "dunif_mix", "dtnorm_mix1", "dtnorm_mix2")
  range <- list( c(0,1), c(-3,3), c(0,6), c(-3, 4), c(0,10), c(-4,4))
  table[,2] = deparse(substitute(R))
  for (i in 1:length(times)) {
    for (j in 1:length(dfunc)) {
      table[j,1]=dfunc[j]
      table[j, 2+i] = system.time(func(times[i],dfunc[[j]],get(dfunc[[j]]), range[[j]], mc = FALSE,dfunc[[j]] ) )[3] # 1.087
      print(c(j,i))   
      
      ## Multiple cores
      table[j, 6+i]= system.time(func(times[i],get(dfunc[[j]]), range[[j]] , mc = TRUE,dfunc[[j]]) )[3] # 0.734
    }
  }
  return(table)
}

tabs = as.data.frame(matrix(rep(0,12*6), ncol=12))
names(tabs)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")


names = c(R,mh,reject,slice)
getTable(eval(names[[1]]))

for (i in 1:1){
  tabs = rbind(tabs,getTable(eval(names[[i]])))
}