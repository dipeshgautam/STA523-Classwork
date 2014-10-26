source("check_packages.R") # Load check_packages function.
suppressMessages(check_packages(c('e1071','rgdal','raster',"devtools", "leafletR", "stringr", "rgeos", "fields","maptools", "ggplot2", "plyr"))) # Ensures listed packages are installed and load them. 

load("manh.RData")
z.sub <-z.sub[-1]


nybb <- readOGR(path.expand("/home/vis/cr173/Sta523/data/parking/nybb/"),"nybb",stringsAsFactors=FALSE)
manh <- nybb[2,]

## training with 2% of the data
set.seed(1000)
index <- 1:nrow(z.sub)
testindex <- sample(index, trunc(length(index)/50))
z.sub.train <- z.sub[testindex,] 

rm(nybb, index)

## Create random points in the area of central park to take into account the lack of any points actually inside central park.
z22 <- z.sub[z.sub$Violation.Precinct==22,]
z22 <- z22[z22$x > quantile(z22$x,.35) & z22$x < quantile(z22$x,.99) &
             z22$y > quantile(z22$y,.10) & z22$y < quantile(z22$y,.99),]
plot(z22$x,z22$y)

x <- -rgamma(round(nrow(z.sub.train)/21), abs(mean(z22$x-.001))^2/(0.005)^2, abs(mean(z22$x))/(0.005)^2)
y <- rgamma(round(nrow(z.sub.train)/21), (mean(z22$y))^2/(0.005)^2, (mean(z22$y))/(0.005)^2)
Violation.Precinct <- rep(22, round(nrow(z.sub.train)/21))
z22 <- data.frame(cbind(x,y,Violation.Precinct))

## Add those randomly generated points to the training data set.
z.sub.train <- rbind(z.sub.train, z22) 


## calculate the best parameters for SVM by tuning with 10-fold cross validation and use those parameters.
tuned <- tune.svm(as.factor(Violation.Precinct)~., data = z.sub.train, gamma = 2^(-1:1), cost = 2^(2:4))
summary(tuned)
best.model <- svm(as.factor(Violation.Precinct)~., data=z.sub.train, cost=tuned$best.parameters[[2]], gamma=tuned$best.parameters[[1]])

r <- rasterize(manh, raster(ncols=500,nrows=1000,ext=extent(bbox(manh))))

cells <- which(!is.na(r[]))
crds <- xyFromCell(r,cells)

z <- predict(best.model,crds) # Predict the precinct for coordinates in Manhattan using the model developed using SVM

r[cells] <- as.numeric(as.character(z))

police.precincts <- c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

l <- list()
for(i in seq_along(police.precincts))
{
  l[[i]] <- rasterToPolygons(r, function(x) x==police.precincts[i], dissolve=TRUE)
  l[[i]]@polygons[[1]]@ID <- as.character(police.precincts[i])
  rownames(l[[i]]@data) <- police.precincts[i]
  colnames(l[[i]]@data) <- "Violation.Precinct"
}

pd <- do.call(rbind, l)
par(mfrow=c(1,4), mar=c(1,1,4,1))
default_plot <- function(main="")
  plot(0,0,type='n', 
       xlim=c(-74.01812,-73.90853), 
       ylim=c(40.70196, 40.87733),
       main = main, axes=FALSE,
       xlab="", ylab="")

## Plot the Predicted map
png("plot.png")
default_plot("Police Precincts in Manhattan")
plot(pd, col = police.precincts, add=TRUE)
dev.off()

## Write JSON to a file
source("write_json.R")
writeGeoJSON(pd, "./precinct.json")


##Create interactive leaflet to differentiate between the precincts
# sp.style=  styleCat(prop="precinct", val=levels(as.factor(hulls$Violation.Precinct)),
#                     style.val=tim.colors(length(levels(as.factor(hulls$Violation.Precinct)))), leg="Precinct")
# 
# sp.map = leaflet(data="precinct.geojson",base.map="osm",style = sp.style,popup= c("precinct"))
# 
# sp.map

