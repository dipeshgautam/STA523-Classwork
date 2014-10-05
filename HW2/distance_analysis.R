source("check_packages.R")
check_packages(c("pracma","fields", "ggmap", "mapproj", "maps","geosphere"))
#library(pracma) ## Gives deg2rad() to convert latitudes and longitudes from degrees to radians.

dennys <- readRDS("~/Desktop/stat programming/Team2/HW2/dennys/dennys_data.Rdata") 
dennys.data <- subset(dennys, select = -c(12, 14, 15, 16))
colnames(dennys.data) <- c("name", "uid", "lati", "long", "city", "add1", "add2", "post", "state", "country", "phone", "fax")
## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
dennys.lati <- as.matrix(as.numeric(dennys.data$lati))
dennys.long <- as.matrix(as.numeric(dennys.data$long))
dennys.loc <- matrix(c(dennys.long, dennys.lati), ncol = 2)

## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
## Need different code to retrieve lat. & long from hotels, since La Quinta data is stored as a data.frame.
lq <- readRDS("~/Desktop/stat programming/Team2/HW2/lq/hotel_list_final.Rdata") # Create data.frame.
lq.data <- subset(lq, select = -c(5, 6)) # Drop "ImagePath", "isInnAndSuites" columns from data.frame
colnames(lq.data) <- c("name", "uid", "lati", "long", "add1", "add2", "city", "state", "post", "country", "phone", "fax")
lq.data$lati <- as.numeric(levels(lq.data$lati))[lq.data$lati]
lq.data$long <- as.numeric(levels(lq.data$long))[lq.data$long]
lq.data$post <- as.numeric(levels(lq.data$post))[lq.data$post]
lq.data$city <- as.character(levels(lq.data$city))[lq.data$city]
lq.data$state <- as.character(levels(lq.data$state))[lq.data$state]
lq.data$country <- as.character(levels(lq.data$country))[lq.data$country]

lq.lati <- lq.data$lati
lq.long <- lq.data$long
lq.loc <- matrix(c(lq.long, lq.lati), ncol = 2)

## rdist.earth() source code assumes the earth’s radius to be 6378.388 km. 
## According to Wikipedia this number is the equitorial radius, which assumes a perfect sphere. Because earth is not a perfect sphere, however, the radius declines as one moves to the poles reaching a polar minimum of about 6,357 km. The mean radius is 6371 km and is what is used.
  distances.select <- NULL
  distances.min <- NULL
  index <- NULL
  distances.all <- NULL
  index.loc <- NULL
  for (j in 1:dim(lq.loc)[1]) {
    for (i in 1:dim(dennys.loc)[1]) { # Find the distance between LQ[j] and 1691 Denny's, store the minimum.
      if (i %% 800 == 0) {
        cat("LQ:", j, "/", dim(lq.loc)[1]," | ", "Denny's:", i, "/", dim(dennys.loc)[1], "\n")
      }
      distances.one <- rdist.earth(matrix(lq.loc[j,], ncol = 2), matrix(dennys.loc[i,], ncol = 2), miles = FALSE, R = 6371)
      distances.all <- matrix(c(distances.all, distances.one))
    }
    distances.min <- matrix(c(distances.min, min(distances.all))) # returns matrix size of # dennys
    index <- matrix(c(index, which.min(distances.all))) # returns matrix size = # LQ stores. row # = dennys row # in original df.
    distances.all <- NULL
  }
  all.df = as.data.frame(distances.min)
  all.df$dennysLat= dennys.data$lati[index]
  all.df$dennysLong= dennys.data$long[index]

for (i in 1:nrow(lq.data)){
  all.df$lqLat[i]=lq.data$lati[i]
  all.df$lqLong[i]=lq.data$long[i]
}
all.df$dennys= index

all.df$distanceMiles= all.df$V1*.6214
#examples

pal <- colorRampPalette(c("#f2f2f2", "black"))
pal <- colorRampPalette(c("#f2f2f2", "red"))
colors <- pal(100)

map('state', region= "Louisiana", col="white", fill=TRUE, bg="lightblue", lwd=0.05)
map('state',col="white", fill=TRUE, bg="lightblue", lwd=0.05)

max = max(count(all.df$dennys)$freq)
max = count(all.df$dennys)[count(all.df$dennys)$freq==max,]
dsub <- all.df[all.df$dennys == max$x,]


for (j in 1:nrow(dsub$dennys)) {
  inter <-gcIntermediate(c(as.numeric(dsub$dennysLong[j]),as.numeric(dsub$dennysLat[j])), c(as.numeric(dsub$lqLong[j]),as.numeric(dsub$lqLat[j])), n=100,addStartEnd=TRUE)  
  lines(inter, col="black", lwd=0.8)
}
