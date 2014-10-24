library(e1071)
load("manh.RData")
z.sub <-z.sub[-1]

library(rgdal)
nybb = readOGR(path.expand("/home/vis/cr173/Sta523/data/parking/nybb/"),"nybb",stringsAsFactors=FALSE)
manh = nybb[2,]


## training with 2/3 of the data and try to test the model on the rest 
index <- 1:nrow(z.sub)
testindex <- sample(index, trunc(length(index)/3))
z.sub.test <- z.sub[testindex,]
z.sub.train <- z.sub[-testindex,]


# ## trainging the model with 10% of data
# trainingsize <- 0.1*nrow(z.sub)
# z.sub.train <- z.sub[1:trainingsize,]
# z.sub.test <- z.sub[trainingsize + 1:nrow(z.sub),]

  ##model <- svm(Violation.Precinct ~., data = z.sub.train)
  tuned <- tune.svm(as.factor(Violation.Precinct)~., data = z.sub.train, gamma = 2^(-1:1), cost = 2^(2:4))
  summary(tuned)
  best.model <- svm(as.factor(Violation.Precinct)~., data=z.sub.train, cost=tuned$best.parameters[[2]], gamma=tuned$best.parameters[[1]])
  ##svm.pred <- predict(best.model, z.sub)

##best.foo()

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

# SVM Prediction
default_plot("SVM")
plot(pd, col = cols[pd@data$Violation.Precinct], add=TRUE)

writeOGR(pd, "./out", "", driver="GeoJSON")
file.rename("./out", "./precinct_svm.json")
