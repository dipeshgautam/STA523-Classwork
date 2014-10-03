source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))


hotels =readRDS("~/Desktop/stat programming/Team2/HW2/lq/lq_URLS.Rdata")


urlFirst = "http://www.lq.com/content/lq/lq-com/en/navigation/findandbook/dynamic-pages/hotel-details."
urlLast= ".address.html"
hotels$urls= paste0(urlFirst,hotels$InnNumber,urlLast)

s= matrix(nrow=length(hotels))
for (i in 1:nrow(hotels)){
  s[i]= content(GET(hotels$urls[i]),as = "text")
#  Sys.sleep(rexp(1,1/5))
}

for (j in 1:nrow(hotels)){
  hotels$phone[j]= str_match_all(s, "Phone: ([0-9-]*)")[[1]][2]
  hotels$fax[j]= str_match_all(s, "Fax: ([0-9-]*)")[[1]][2]
}