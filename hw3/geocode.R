# setwd("~/Team2/HW3") # Ensures check_packages.R is found.
source("check_packages.R") # Load check_packages function.
suppressMessages(check_packages(c('ggmap','maptools','maps',"devtools", "stringr", "rgdal", "rgeos", "data.table", "maptools", "ggplot2", "plyr","dplyr"))) # Ensures listed packages are installed and load them. 
# remove.packages("dplyr", "/home/grad/dg156/R/x86_64-redhat-linux-gnu-library/3.1/00LOCK-hadley-dplyr-e864107")
# install_github("hadley/dplyr") # Install github version of dplyr instead of CRAN version so inner.join() will not crash.
# suppressMessages(library(dplyr)) # 

base <- '/home/vis/cr173/Sta523/data/parking' # Set to Dr. Rundel's Saxon directory containing NYParking data

## Read parking violations data and rename the required columns.
park.full <- suppressWarnings(tbl_df(fread(paste0(base,"/NYParkingViolations.csv"), stringsAsFactors = FALSE))) # Full data set using fread() for speed and convenience 
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

rm(park.full) # Improve speed by removing data set from memory.

pl <- readOGR(paste0(base,"/pluto/Manhattan/"),"MNMapPLUTO") # Takes 2.5 minutes. Manhattan shapefile connecting property boundary polygons and addresses. 
pt <- gCentroid(pl,byid = TRUE) # Store the centroid of the given geometry in pt data frame.
tax <- cbind(data.frame(pt@coords), tolower(as.character(pl@data$Address))) # Add centroid coordinates and lowercase address of pl shapefile and police precinct.
names(tax)[3] <- "addr" # Rename third column of tax data frame to addr.

## Remove the pluto data
rm(pl, pt)

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

## remove extreme outliers
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
## Remove unnecessary data from memory
rm(addr, z.sub, z.final, tax, z, police.precincts)