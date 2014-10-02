source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

dir.create("lq/", showWarnings = FALSE)

url= "http://www.lq.com/en/findandbook.html"

s = content(GET(url), as="text")

name <- unlist(str_match_all(s, "title: \"([a-zA-Z0-9 &-/]*)\", "))[880:1758]
innNumber <- unlist(str_match_all(s, "innNumber: \"([0-9]*)\", "))[880:1758]
lat <- unlist(str_match_all(s, "\"([0-9.]*)\", longitude:"))[880:1758]
long <- unlist(str_match_all(s, "longitude: \"(-[0-9.]*)"))[880:1758]
street <- unlist(str_match_all(s, "street: \"([0-9a-zA-Z -.]*)\", "))[880:1758]
city <- unlist(str_match_all(s, "city: \"([a-zA-Z]*)"))[880:1758]
stateProv <- unlist(str_match_all(s, "stateProv: \"([a-zA-Z]*)"))[880:1758]
postalCode <- unlist(str_match_all(s, "postalCode: \"([0-9A-Z-]*)"))[880:1758]
country <- unlist(str_match_all(s, "countryDisplay: \"([a-zA-Z ]*)"))[880:1758]

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

write(s, file="lq/laqinta_list.html")
