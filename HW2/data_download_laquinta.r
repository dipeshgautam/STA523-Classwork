source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))
url= "http://www.lq.com/en/findandbook.html"
d=as.vector(GET(url))
a <- str_match_all(d, "push(.*)hotelDetails") 

ID <- unlist(str_match_all(d," id=\"g([0-9]*)\""))[seq(2,386,2)]
  