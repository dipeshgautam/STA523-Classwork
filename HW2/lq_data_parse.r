source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

s = readLines("lq/laqinta_list.txt")

name <- unlist(str_match_all(s, "title: \"([a-zA-Z0-9 &-/]*)\", "))[seq(2,1758,2)]
innNumber <- unlist(str_match_all(s, "innNumber: \"([0-9]*)\", "))[seq(2,1758,2)]
lat <- unlist(str_match_all(s, "\"([0-9.]*)\", longitude:"))[seq(2,1758,2)]
long <- unlist(str_match_all(s, "longitude: \"(-[0-9.]*)"))[seq(2,1758,2)]
street <- unlist(str_match_all(s, "street: \"([0-9a-zA-Z -.]*)\", "))[seq(2,1758,2)]
city <- unlist(str_match_all(s, "city: \"([a-zA-Z]*)"))[seq(2,1758,2)]
stateProv <- unlist(str_match_all(s, "stateProv: \"([a-zA-Z]*)"))[seq(2,1758,2)]
postalCode <- unlist(str_match_all(s, "postalCode: \"([0-9A-Z-]*)"))[seq(2,1758,2)]
country <- unlist(str_match_all(s, "countryDisplay: \"([a-zA-Z ]*)"))[seq(2,1758,2)]

lq_data <- data.frame(name=name, 
                      innNumber=innNumber,
                      lat=lat,
                      long=long,
                      street=street,
                      city=city,
                      postalCode=postalCode,
                      stateProv=stateProv,
                      country=country
)

