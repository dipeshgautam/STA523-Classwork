source("check_packages.R")
check_packages(c("pracma","fields", "ggmap", "mapproj", "maps","geosphere"))
#library(pracma) ## Gives deg2rad() to convert latitudes and longitudes from degrees to radians.

dennys <- readRDS("~/Team2/HW2/dennys/dennys_data.Rdata")
dennys.data <- subset(dennys, select = -c(12, 14, 15, 16))
colnames(dennys.data) <- c("name", "uid", "lati", "long", "city", "add1", "add2", "post", "state", "country", "phone", "fax")
## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
dennys.lati <- NULL
dennys.long <- NULL
dennys.loc <- NULL
dennys.lati <- as.matrix(as.numeric(dennys_datamod$latitude))
dennys.long <- as.matrix(as.numeric(dennys_datamod$longitude))
dennys.loc <- matrix(c(dennys.long, dennys.lati), ncol = 2)

## Store latitude and longitude, convert from degrees to radians for rdist.earth(), combine into matrix.
## Need different code to retrieve lat. & long from hotels, since La Quinta data is stored as a data.frame.
lq <- readRDS("~/Team2/HW2/lq/hotel_list_final.Rdata") # Create data.frame.
lq.data <- subset(hotels, select = -c(5, 6)) # Drop "ImagePath", "isInnAndSuites" columns from data.frame
colnames(lq.data) <- c("name", "uid", "lati", "long", "add1", "add2", "city", "state", "post", "country", "phone", "fax")
lq.data$lati <- as.numeric(levels(lq.data$lati))[lq.data$lati]
lq.data$long <- as.numeric(levels(lq.data$long))[lq.data$long]
lq.data$post <- as.numeric(levels(lq.data$post))[lq.data$post]
lq.data$city <- as.character(levels(lq.data$city))[lq.data$city]
lq.data$state <- as.character(levels(lq.data$state))[lq.data$state]
lq.data$country <- as.character(levels(lq.data$country))[lq.data$country]

lq.lati <- NULL
lq.long <- NULL
lq.loc <- NULL
for (i in 1:dim(lq.data)[1]) {
  lq.long <- matrix(c(lq.long, lq.data$long[i]))
  lq.lati <- matrix(c(lq.lati, lq.data$lati[i]))
}
lq.loc <- matrix(c(lq.long, lq.lati), ncol = 2)

## rdist.earth() source code assumes the earth’s radius to be 6378.388 km. 
## According to Wikipedia this number is the equitorial radius, which assumes a perfect sphere. Because earth is not a perfect sphere, however, the radius declines as one moves to the poles reaching a polar minimum of about 6,357 km. The mean radius is 6371 km and is what is used.
library(fields)
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

  ## Find latitude and longitude of matched Denny's.
  dennys.index.loc <- NULL
  for (j in 1:dim(index)[1]) { # 1:875
    dennys.index.loc <- matrix(c(dennys.index.loc, dennys.loc[index[j],]), ncol = 2)
  }
  dim(dennys.index.loc)  

  ## Find latitude and longitude of matched La Quinta.
  lq.index.loc <- NULL
  for (i in 1:dim(index)[1]) { # 1:875
    lq.index.loc <- matrix(c(lq.index.loc, as.numeric(lq.data$lati[i]), as.numeric(lq.data$lati[i])), ncol = 2)
  }
  dim(lq.index.loc)

  distances.km <- NULL
  distances.miles <- NULL
  distances.km <- signif(distances.min, 4)
  distances.miles <- signif(distances.min*0.621371, 4)
  all.matrix <- NULL
  all.matrix <- matrix(c(seq(1, dim(index)[1], by=1), index, distances.km, distances.miles, dennys.index.loc, lq.index.loc), ncol = 8)
  colnames(all.matrix) <- c("LQ", "Dennys", "DLQ_Dist_km", "DLQ_Dist_mi", "dennys.lati", "dennys.long", "lq.lati", "lq.long")
  all.df <- as.data.frame(all.matrix)
  all.df <- c(all.df, lq.data)

# Troubleshooting functions
  lqInfo <- function (id) {
    cat(lq.data$city[id],",", lq.data$state[id], "\n")
    cat(lq.data$long[id],",", lq.data$lati[id], "\n")
  }
  
  dInfo <- function (id) {
    cat(dennys.data$city[id],", ", dennys.data$state[id], "\n")
    cat(dennys.data$long[id],", ", dennys.data$lati[id])
  }

library(ggmap)
library(mapproj)
map <- get_map(
  location = "United States", # google search string
  zoom = 4, # larger is closer
  maptype = "hybrid" # map type
)
p <- ggmap(map)
p <- p + geom_text(data = all.df, aes(x = all.df$dennys.long, y = all.df$dennys.lati), label = index)
print(p)


#examples
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
map("world", col="white", fill=TRUE, bg="lightblue", lwd=0.05, xlim=xlim, ylim=ylim)

