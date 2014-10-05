source("check_packages.R")
check_packages(c("pracma","fields", "ggmap", "mapproj", "maps","geosphere"))
#library(pracma) ## Gives deg2rad() to convert latitudes and longitudes from degrees to radians.

dennys.data= readRDS("~/Team2/HW2/dennys/dennys_data.Rdata")
## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
dennys.lati <- NULL
dennys.long <- NULL
dennys.loc <- NULL
dennys.lati <- as.matrix(as.numeric(dennys_datamod$latitude))
dennys.lati <- deg2rad(dennys.lati)
dennys.long <- as.matrix(as.numeric(dennys_datamod$longitude))
dennys.long <- deg2rad(dennys.long)
dennys.loc <- matrix(c(dennys.lati, dennys.long), ncol = 2)

## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
## Need different code to retrieve lat. & long from hotels, since La Quinta data is stored as a data.frame.
hotels <- readRDS("~/Team2/HW2/lq/hotel_list_final.Rdata") # Create data.frame.
hotels.data <- subset(hotels, select = -c(5, 6)) # Drop "ImagePath", "isInnAndSuites" columns from data.frame
colnames(hotels.data) <- c("name", "uid", "lati", "long", "add1", "add2", "city", "state", "post", "country", "phone", "fax")
hotels.data$lati <- as.numeric(levels(hotels.data$lati))[hotels.data$lati]
hotels.data$long <- as.numeric(levels(hotels.data$long))[hotels.data$long]
hotels.data$post <- as.numeric(levels(hotels.data$long))[hotels.data$post]

hotels.lati <- NULL
hotels.long <- NULL
hotels.loc <- NULL
for (i in 1:dim(hotels.data)[1]) {
  hotels.lati <- matrix(c(hotels.lati, hotels.data$lati[i]))
  hotels.long <- matrix(c(hotels.long, hotels.data$long[i]))
}

#for (i in 1:dim(as.matrix(hotels$lati))[1]) {
#  hotels.lati <- matrix(c(hotels.lati, as.numeric(as.matrix(hotels$Latitude)[i])))
#  hotels.long <- matrix(c(hotels.long, as.numeric(as.matrix(hotels$Longitude)[i])))
#}
head(hotels.lati)
head(hotels.long)
hotels.lati <- deg2rad(hotels.lati)
hotels.long <- deg2rad(hotels.long)
head(hotels.lati)
head(hotels.long)
hotels.loc <- matrix(c(hotels.lati, hotels.long), ncol = 2)
head(hotels.loc)

## The following code works
## rdist.earth(matrix(dennys.loc[1,],ncol=2),matrix(hotels.loc[1,], ncol=2), miles = FALSE, R = 6371)
## rdist.earth(matrix(c(dennys.lati[1], dennys.long[1]), ncol=2),matrix(c(hotels.lati[1],hotels.long[1]), ncol=2), miles = FALSE, R = 6371)
## rdist.earth() source code assumes the earth’s radius to be 6378.388 km. 
## According to Wikipedia this number seems to be the equatorial radius (the maximum radius). Because earth is not a perfect sphere, however, the radius declines as one moves to the poles reaching a polar minimum of about 6,357 km. The mean radius is 6371 km and is what I have been using in my calculations
#library(fields)
#distance <- function (dennys.loc, hotels.loc) {
  distances.select <- NULL
  distances.min <- NULL
  index <- NULL
  distances.all <- NULL
  index.loc <- NULL
  #for (j in 1:dim(hotels.loc)[1]) {
  for (j in 1:1) { # testing purposes
    #for (i in 1:dim(dennys.loc)[1]) { # Find the distance between LQ[j] and 1691 Denny's, store the minimum.
    for (i in 1:dim(dennys.loc)[1]) { # testing purposes
      if (i %% 400 == 0) {
        cat("LQ:", j, "/", dim(hotels.loc)[1], "Denny's:", i, "/", dim(dennys.loc)[1], "\n")
      }
      distances.one <- rdist.earth(matrix(hotels.loc[j,], ncol = 2), matrix(dennys.loc[i,], ncol = 2), miles = FALSE, R = 6371)
      distances.all <- matrix(c(distances.all, distances.one))
    }
    distances.min <- matrix(c(distances.min, min(distances.all))) # returns matrix size of # dennys
    index <- matrix(c(index, which.min(distances.all))) # returns matrix size = # LQ stores. row # = dennys row # in original df.
    distances.all <- NULL
  }
  
  # Find latitude and longitude of matched Denny's.
  dennys.index.loc <- NULL
  for (j in 1:dim(index)[1]) { # 1:875
    dennys.index.loc <- matrix(c(dennys.index.loc, dennys.loc[index[j],]), ncol = 2)
  }
  dim(dennys.index.loc)  

# Find latitude and longitude of matched La Quinta.
  hotels.index.loc <- NULL
  #for (i in 1:dim(hotels.loc)[1]) { # 1:875
  for (i in 1:dim(index)[1]) { # 1:875
    hotels.index.loc <- matrix(c(hotels.index.loc, as.numeric(hotels.data$lati[i]), as.numeric(hotels.data$lati[i])), ncol = 2)
  }
  dim(hotels.index.loc)
  head(hotels.index.loc)

  distances.min.km <- signif(distances.min*6371, 4)
  distances.min.miles <- signif(distances.min*6371*0.621371, 3)
  all.matrix <- NULL
  #all.matrix <- matrix(c(seq(1, 875, by=1), index, distances.min, distances.min.km, distances.min.miles, dennys.index.loc, hotels.index.loc), ncol = 9)
  all.matrix <- matrix(c(seq(1, dim(index)[1], by=1), index, distances.min, distances.min.km, distances.min.miles, dennys.index.loc, hotels.index.loc), ncol = 9)
  # s = r*theta where s is arc length, r is radius and theta is the subtended angle in radians.
  colnames(all.matrix) <- c("Dennys", "LQ", "DLQ_Dist_Radians", "DLQ_Dist_km", "DLQ_Dist_mi", "dennys.lati", "dennys.long", "lq.lati", "lq.long")
  head(all.matrix)

# Troubleshooting functions
  lqInfo <- function (id) {
    cat(hotels.data[id,"city"],",", hotels.data[id,"state"], "\n")
    which(dennys.data$state == hotels.data[id,"state"])
  }
  
  dInfo <- function (id) {
    cat(dennys.data$city[id],", ", dennys.data$state[id])
  }
#}
#library(ggmap)
#library(mapproj)
map <- get_map(location = "United States" # google search string
  , zoom = 3 # larger is closer
  , maptype = "hybrid" # map type
)


#examples
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
map("world", col="white", fill=TRUE, bg="lightblue", lwd=0.05, xlim=xlim, ylim=ylim)
hotels$lat


