source("check_packages.R")
check.packages(c("pracma","fields"))
library(pracma) ## Gives deg2rad() to convert latitudes and longitudes from degrees to radians.

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
hotels.lati <- NULL
hotels.long <- NULL
hotels.loc <- NULL
for (i in 1:dim(as.matrix(hotels$Latitude))[1]) {
  hotels.lati <- matrix(c(hotels.lati, as.numeric(as.matrix(hotels$Latitude)[i])))
  hotels.long <- matrix(c(hotels.long, as.numeric(as.matrix(hotels$Longitude)[i])))
}
hotels.lati <- deg2rad(hotels.lati)
hotels.long <- deg2rad(hotels.long)
hotels.loc <- matrix(c(hotels.lati, hotels.long), ncol = 2)

## The following code works
## rdist.earth(matrix(dennys.loc[1,],ncol=2),matrix(hotels.loc[1,], ncol=2), miles = FALSE, R = 6371)
## rdist.earth(matrix(c(dennys.lati[1], dennys.long[1]), ncol=2),matrix(c(hotels.lati[1],hotels.long[1]), ncol=2), miles = FALSE, R = 6371)
## rdist.earth() source code assumes the earth’s radius to be 6378.388 km. 
## According to Wikipedia this number seems to be the equatorial radius (the maximum radius). Because earth is not a perfect sphere, however, the radius declines as one moves to the poles reaching a polar minimum of about 6,357 km. The mean radius is 6371 km and is what I have been using in my calculations
library(fields)
#distance <- function (dennys.loc, hotels.loc) {
  distances.select <- NULL
  distances.min <- NULL
  index <- NULL
  distances.all <- NULL
  for (j in 1:dim(dennys.loc)[1]) {
  #for (j in 1:6) { # testing purposes
    ## Given a specific La Quinta, calculate its distance from each Denny's. 
    for (i in 1:dim(hotels.loc)[1]) {
    #for (i in 1:500) { # testing purposes
      print(i)
      distances.one <- rdist.earth(matrix(dennys.loc[j,], ncol = 2), matrix(hotels.loc[i,], ncol = 2), miles = FALSE, R = 6371)
      distances.all <- matrix(c(distances.all, distances.one))
      print(distances.one)
    }
    distances.min <- matrix(c(distances.min, min(distances.all))) # returns matrix size of # dennys
    index <- matrix(c(index, which.min(distances.all))) # returns matrix size of # dennys
    distances.all <- NULL
  }
#  return distances.min
  distances.min.miles <- distances.min
  all.matrix <- matrix(c(seq(1, 1691, by=1), index, distances.min, distances.min*6371, distances.min*6371*0.621371), ncol = 5)
  # s = r*theta where s is arc length, r is radius and theta is the subtended angle in radians.
  colnames(all.matrix) <- c("Dennys", "LQ", "DLQ_Dist_Radians", "DLQ_Dist_km", "DLQ_Dist_mi")
#}
library(ggmap)
library(mapproj)
map <- get_map(location = "United States" # google search string
  , zoom = 3 # larger is closer
  , maptype = "hybrid" # map type
)

