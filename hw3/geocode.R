setwd("~/Team2/HW3") # Ensures check_packages.R is found.
source("check_packages.R") # Load check_packages function.
check_packages(c('ggmap','maptools','maps',"devtools", "stringr", "rgdal", "rgeos", "data.table", "maptools", "ggplot2", "plyr")) # Ensures listed packages are installed and load them. 
install_github("hadley/dplyr") # Install github version of dplyr instead of CRAN version so inner.join() will not crash.
library(dplyr) # 

base <- '/home/vis/cr173/Sta523/data/parking' # Set to Dr. Rundel's Saxon directory containing NYParking data

## Full data.
park.full <- tbl_df(fread(paste0(base,"/NYParkingViolations.csv"), stringsAsFactors = FALSE)) # Full data set using fread() for speed and convenience 
park.full$'Summons Number' <- as.numeric(park.full$'Summons Number') # Need to set as numeric if using fread() on data. 
setnames(park.full, 'Violation Precinct', 'Violation.Precinct') # Rename variable.
setnames(park.full, 'House Number', 'House.Number')
setnames(park.full, 'Street Name', 'Street.Name')


addr <- filter(park.full, Violation.Precinct <= 34) %>% # Send subset: park's violation precincts less than or equal to precinct 34.
  mutate(House.Number = str_trim(House.Number), Street.Name = str_trim(Street.Name)) %>% # Add new columns without white space.
  filter(House.Number != "" & Street.Name != "") %>% # Subset excluding entries with missing house numbers and street names.
  filter(str_detect(House.Number,"[0-9]+")) %>% # Subset only including house numbers with digits
  transmute(Violation.Precinct = Violation.Precinct, addr = paste(House.Number, Street.Name)) %>% # 
  mutate(addr = tolower(addr)) # 

# saveRDS(addr, ) # Save addr data frame locally.

# rm(park) # Improve speed by removing data set from memory.
rm(park.full) # Improve speed by removing data set from memory.

pl <- readOGR(paste0(base,"/pluto/Manhattan/"),"MNMapPLUTO") # Takes 2.5 minutes. Manhattan shapefile connecting property boundary polygons and addresses. 
pt <- gCentroid(pl,byid = TRUE) # Store the centroid of the given geometry in pt data frame.
tax <- cbind(data.frame(pt@coords), tolower(as.character(pl@data$Address))) # Add centroid coordinates and lowercase address of pl shapefile and police precinct.
names(tax)[3] <- "addr" # Rename third column of tax data frame to addr.
# names(tax)[4] <- "Violation.Precinct" # Rename fourth column of tax data frame to Violation.Precinct.
# pl <- readShapeSpatial(paste0(base,"/pluto/Manhattan/","MNMapPLUTO"))
# tax <- data.frame(addr=pl$Address, precinct= pl$PolicePrct, coordinates(pl))

## Rename observations to follow arbitrary naming conventions and improve matches between tax and addr data frames.
tax$addr <- str_replace_all(tax$addr, "street", "st")
tax$addr <- str_replace_all(tax$addr, "avenue", "ave")
tax$addr <- str_replace_all(tax$addr, "lane", "ln")
tax$addr <- str_replace_all(tax$addr, "place", "pl")
tax$addr <- str_replace_all(tax$addr, "drive", "dr")
tax$addr <- str_replace_all(tax$addr, "east", "e")
tax$addr <- str_replace_all(tax$addr, "west", "w")
tax$addr <- str_replace_all(tax$addr, "south", "s")
tax$addr <- str_replace_all(tax$addr, "north", "n")

addr$addr <- str_replace_all(addr$addr, "1st", "1")
addr$addr <- str_replace_all(addr$addr, "2nd", "2")
addr$addr <- str_replace_all(addr$addr, "3rd", "3")
addr$addr <- str_replace_all(addr$addr, "5th", "5")
addr$addr <- str_replace_all(addr$addr, "29th", "29")
addr$addr <- str_replace_all(addr$addr, "163rd", "163")
addr$addr <- str_replace_all(addr$addr, "east", "e")
addr$addr <- str_replace_all(addr$addr, "west", "w")
addr$addr <- str_replace_all(addr$addr, "south", "s")
addr$addr <- str_replace_all(addr$addr, "north", "n")
addr$addr <- str_replace_all(addr$addr, "street", "st")
addr$addr <- str_replace_all(addr$addr, "avenue", "ave")
addr$addr <- str_replace_all(addr$addr, "place", "pl") # Confirmed that "pl" is used for "place" rather than "plaza"
addr$addr <- str_replace_all(addr$addr, "boulevard", "blvd")
addr$addr <- str_replace_all(addr$addr, "drive", "dr")
addr$addr <- str_replace_all(addr$addr, "lane", "ln")
addr$addr <- str_replace_all(addr$addr, "bway", "broadway")
addr$addr <- str_replace_all(addr$addr, "e.broadway", "e broadway")
addr$addr <- str_replace_all(addr$addr, "boradwya", "broadway")
addr$addr <- str_replace_all(addr$addr, "th", "")

z <- inner_join(tax, addr) # Store matching addresses in "addr" and "tax" data frames.

police.precincts <- c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34) # Assume: Violation Precinct for Midtown So. Pct ==  14, Midtown No. Pct == 18, Central Park Pct == 22: see http://unhp.org/crg/indy-maps_police_mn.html

## Plot police precincts.
z.sub <- subset(z, (z$Violation.Precinct %in% police.precincts)) # Create a subset of data frame z with violation precincts matching police precincts. 

## remove outliers
z.final <- NULL
for (p in 1:length(police.precincts)){
  k <- police.precincts[p]
  z1 <- z.sub[z.sub$Violation.Precinct==k,]
  z1 <- z1[z1$x > quantile(z1$x,.005) & z1$x < quantile(z1$x,.995) &
             z1$y > quantile(z1$y,.005) & z1$y < quantile(z1$y,.995),]
  z.final <- rbind(z.final, z1)
}
z.sub <- z.final

## save the data
save(z.sub, file = "manh.RData")

# 
# 
# N <- 33
# for(n in 1:N){
#   
#   z.final <- NULL
#   
#   for (l in 1:length(police.precincts)){
#     k <- police.precincts[l]
#     cat(n, k, "\n")
#     ## Subset the precinct we're looking at
#     z1 <- z.sub[z.sub$Violation.Precinct==k,]
#     ## Remove .5% outliers in each direction
#     #     z1 <- z1[z1$x > quantile(z1$x,.005) & z1$x < quantile(z1$x,.995) &
#     #                z1$y > quantile(z1$y,.005) & z1$y < quantile(z1$y,.995),]
#     
#     ## Check if the any points are within inner central 10-20th percentile of any of the other precincts
#     z1.rejected.outliers <- NULL
#     police.prct=police.precincts[-l]
#     for ( j in 1:length(police.prct)){
#       z.p <- z.sub[z.sub$Violation.Precinct==police.prct[j],]
#       z1.rejected.outliers <- rbind(z1.rejected.outliers, 
#                                     z1[z1$x > quantile(z.p$x, (.35-.01*(n-1))) & z1$x < quantile(z.p$x, (.65+.01*(n-1))) &
#                                          z1$y > quantile(z.p$y, (.35-.01*(n-1))) & z1$y < quantile(z.p$y, (.65+.01*(n-1))),])
#     }
#     ## Get rid of potential outliers that happened to be within certain increasing percentile of at least one other precinct
#     z1.rejected.outliers <- unique(z1.rejected.outliers)
#     z1.final <- setdiff(z1, z1.rejected.outliers)
#     #     z.final <-rbind(z.final, z1.final)   
#     z.without.k <- setdiff(z.sub, z1)
#     z.with.cleaned.k <- union(z.without.k, z1.final)
#     z.sub <- z.with.cleaned.k
#     
#   }
#   #   z.sub <- z.final
# }
# 
# 
# z.final <- NULL
# for (p in 1:length(police.precincts)){
#   k <- police.precincts[p]
#   z1 <- z.sub[z.sub$Violation.Precinct==k,]
#   z1 <- z1[z1$x > quantile(z1$x,.02) & z1$x < quantile(z1$x,.98) &
#              z1$y > quantile(z1$y,.02) & z1$y < quantile(z1$y,.98),]
#   z.final <- rbind(z.final, z1)
# }
# 
# z.sub1 <- z.final
# 
# 
# ## chull() calculates the center of a set of points, then finds the furthest points. This gives a convex hull.
# find_hull <- function(z) z[chull(z$x, z$y), ] # Create find_hull function using chull(x, y) to compute the convex hull of a set of points with argument x as coordinate vectors of points. 
# hulls <- ddply(z.sub1, "Violation.Precinct", find_hull) # 
# 
# ##creating geojson boundaries
# for (i in levels(as.factor(hulls$Violation.Precinct))){
#   precName = paste("p",i, sep= "")
#   assign(precName, list())
#   for (j in 1:nrow(hulls)){
#     if (hulls$Violation.Precinct[j]==i){
#       toAdd =paste('[',hulls$x[j],",",hulls$y[j],']', sep = "")
#       lst = get(precName)
#       lst[[length(lst)+1]] <- toAdd
#       assign(precName, lst)
#     }
#   }
# }
# 
# geoFile = '{
# "type": "FeatureCollection",
# "features": ['
# for (i in levels(as.factor(hulls$Violation.Precinct))){
#   precName = paste("p",i, sep= "")
#   lst = get(precName)
#   lst = paste(unlist(lst), collapse=",")
#   lst = paste("[
#               ",lst,"
#               ]")
#   geoFile=paste(geoFile,
#                 '
# {
#                 "type": "Feature",
#                 "geometry": {
#                 "type": "Polygon",
#                 "coordinates": [
#                 ', lst,'
#                 ]
#                 },
#                 "properties": {
#                 "precinct":', i,'
#                 }
# },', sep= "")
# }
# 
# geoFile=substring(geoFile,1, nchar(geoFile)-1)         
# geoFile=paste(geoFile,"
#               ]
# }", sep= "")
# 
# write(geoFile,'precinct.geojson')



