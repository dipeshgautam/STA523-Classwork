source("test_metropolishasting_sampler.R")

table = as.data.frame(matrix(rep(0,12*6), ncol=12))
names(table)= c("Distribution","Sampler","SC 100", "SC 10,000", "SC 1,000,000", "SC 10,000,000","MC 100", "MC 10,000", "MC 1,000,000", "MC 10,000,000", "SC Score","MC Score")

table[,1] = c("dbetann","dtnorm","dtexp","dunif_mix","dtnorm_mix1","dtnorm_mix2")
table[,2] = rep("metropolis",6)
range <- list(c(0,1),c(-3,3),c(0,6),c(-3,4),c(0,10),c(-4,4))

##loop through each distribution

##loop through each single core
for(i in 1:6){

  for(j in 3:6){
      n <- 100
      table[i,j] <- system.time(mh(n*100^(3-2), get(eval(table[i,1])), range[i], mc=FALSE))[3]
      table[i,j+4] <- system.time(mh(n*100^(j-2), get(eval(table[i,1])), c(0,1), mc=TRUE))[3]
    
      x <- mh(1000,dbetann,c(0,1),mc = FALSE) 
      table[i,11] <- score(x,get(eval(table[i,1])))
      y <- mh(1000,dbetann,c(0,1),mc = TRUE)   
      table[i,12] <- score(y,get(eval(table[i,1])))
  }

}


