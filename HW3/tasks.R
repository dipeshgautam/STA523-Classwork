setwd("~/Team2/HW3") # Ensures check_packages.R is found.
source("check_packages.R")
check_packages(c("devtools", "stringr", "rgdal", "rgeos", "data.table"))
install_github("hadley/dplyr")
library(dplyr)

base = '/home/vis/cr173/Sta523/data/parking' # Set to Dr. Rundel's Saxon directory containing NYParking data
# Small Data, Use this for testing
# park = tbl_df(read.csv(paste0(base,"/NYParkingViolations_small.csv"), stringsAsFactors=FALSE)) # Create subset for proof-of-concept testing
# addr = filter(park, Violation.Precinct <= 34) %>% 
#   mutate(House.Number = str_trim(House.Number), Street.Name = str_trim(Street.Name)) %>%
#   filter(House.Number != "" & Street.Name != "") %>%
#   filter(str_detect(House.Number,"[0-9]+")) %>%
#   transmute(Violation.Precinct = Violation.Precinct, addr = paste(House.Number, Street.Name)) %>%
#   mutate(addr = tolower(addr))


#Full Data
park.full = tbl_df(fread(paste0(base,"/NYParkingViolations.csv"), stringsAsFactors=FALSE)) # Full data set using fread() for speed and convenience 
park.full$'Summons Number'= as.numeric(park.full$'Summons Number')
setnames(park.full, 'Violation Precinct', 'Violation.Precinct')
setnames(park.full, 'House Number', 'House.Number')
setnames(park.full, 'Street Name', 'Street.Name')

addr = filter(park.full, Violation.Precinct <= 34) %>% 
  mutate(House.Number = str_trim(House.Number), Street.Name = str_trim(Street.Name)) %>%
  filter(House.Number != "" & Street.Name != "") %>%
  filter(str_detect(House.Number,"[0-9]+")) %>%
  transmute(Violation.Precinct = Violation.Precinct, addr = paste(House.Number, Street.Name)) %>%
  mutate(addr = tolower(addr))

# addr1 <- addr
rm(park)
rm(park.full)

pl = readOGR(paste0(base,"/pluto/Manhattan/"),"MNMapPLUTO") # Takes 3 minutes. Manhattan shapefile connecting property boundary polygons and addresses. 

pt = gCentroid(pl,byid=TRUE) 

tax = cbind(data.frame(pt@coords), tolower(as.character(pl@data$Address)))
names(tax)[3] = "addr"
tax$addr = str_replace_all(tax$addr, "street", "st")
tax$addr = str_replace_all(tax$addr, "avenue", "ave")
tax$addr = str_replace_all(tax$addr, "lane", "ln")
tax$addr = str_replace_all(tax$addr, "place", "pl")
tax$addr = str_replace_all(tax$addr, "drive", "dr")
tax$addr = str_replace_all(tax$addr, "east", "e")
tax$addr = str_replace_all(tax$addr, "west", "w")
tax$addr = str_replace_all(tax$addr, "south", "s")
tax$addr = str_replace_all(tax$addr, "north", "n")


addr$addr = str_replace_all(addr$addr, "th", "")
addr$addr = str_replace_all(addr$addr, "[0-9]*1st", "1")
addr$addr = str_replace_all(addr$addr, "2nd", "2")
addr$addr = str_replace_all(addr$addr, "3rd", "3")
addr$addr = str_replace_all(addr$addr, "east", "e")
addr$addr = str_replace_all(addr$addr, "west", "w")
addr$addr = str_replace_all(addr$addr, "south", "s")
addr$addr = str_replace_all(addr$addr, "north", "n")
addr$addr = str_replace_all(addr$addr, "street", "st")
addr$addr = str_replace_all(addr$addr, "avenue", "ave")
addr$addr = str_replace_all(addr$addr, "place", "pl")
addr$addr = str_replace_all(addr$addr, "drive", "dr")
addr$addr = str_replace_all(addr$addr, "lane", "ln")
addr$addr = str_replace_all(addr$addr, "boradwya", "broadway")

z = inner_join(tax, addr)
#plot(z$y, z$x)
