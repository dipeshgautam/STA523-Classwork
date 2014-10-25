ptm=proc.time() # Start time

source("check_packages.R") # Load check_packages function.
check_packages(c('e1071','rgdal','raster',"devtools", "stringr", "rgeos", "maptools", "ggplot2", "plyr")) # Ensures listed packages are installed and load them. 

load("manh.RData")
z.sub <-z.sub[-1]

nybb = readOGR(path.expand("/home/vis/cr173/Sta523/data/parking/nybb/"),"nybb",stringsAsFactors=FALSE)
manh = nybb[2,]


## training with 2/3 of the data and try to test the model on the rest 
index <- 1:nrow(z.sub)
testindex <- sample(index, trunc(length(index)/200))
z.sub.test <- z.sub[testindex,]
z.sub.train <- rbind(z.sub[testindex,], z.sub[z.sub$Violation.Precinct==22,]) # Since Central Park precinct has very few points before subsetting, we're includeing all of them to get a better prediction for the precinct.


##model <- svm(Violation.Precinct ~., data = z.sub.train)
tuned <- tune.svm(as.factor(Violation.Precinct)~., data = z.sub.train, gamma = 2^(-1:1), cost = 2^(2:4))
summary(tuned)
best.model <- svm(as.factor(Violation.Precinct)~., data=z.sub.train, cost=tuned$best.parameters[[2]], gamma=tuned$best.parameters[[1]])

library(raster)
r = rasterize(manh, raster(ncols=500,nrows=1000,ext=extent(bbox(manh))))

cells = which(!is.na(r[]))
crds = xyFromCell(r,cells)

z = predict(best.model,crds)

r[cells] = as.numeric(as.character(z))

police.precincts <- c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

l=list()
for(i in seq_along(police.precincts))
{
  l[[i]] = rasterToPolygons(r, function(x) x==police.precincts[i], dissolve=TRUE)
  l[[i]]@polygons[[1]]@ID = as.character(police.precincts[i])
  rownames(l[[i]]@data) = police.precincts[i]
  colnames(l[[i]]@data) = "Violation.Precinct"
}

pd = do.call(rbind, l)
par(mfrow=c(1,4), mar=c(1,1,4,1))
default_plot = function(main="")
  plot(0,0,type='n', 
       xlim=c(-74.01812,-73.90853), 
       ylim=c(40.70196, 40.87733),
       main = main, axes=FALSE,
       xlab="", ylab="")

# SVM Prediction
default_plot("SVM")
# cols = c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17","#DA1414", "#1D622B", "#F1651F", "#2C446B",
#          "#DAE7D9", "#DB35D0", "#050005", "#04FD14", "#1A6C70", "#FC0543", "#819794", "#DA1414", "#49BBC1", "#975684")

plot(pd, col = "blue", add=TRUE)

#cols[pd@data$Violation.Precinct]

writeOGR(pd, "./out", "", driver="GeoJSON")
file.rename("./out", "./precinct_svm.json")

## calculating time taken, need to delete
proc.time()-ptm # total time taken
