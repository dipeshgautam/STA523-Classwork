source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))


hotels =readRDS("~/Desktop/stat programming/Team2/HW2/lq/lq_URLS.Rdata")
urls = readRDS("~/Desktop/stat programming/Team2/HW2/lq/all_urls.Rdata")

for (i in 1:nrow(hotels)){
  hotels$phone[i]= str_match_all(urls[i], "Phone: ([0-9-]*)")[[1]][2]
  hotels$fax[i]= str_match_all(urls[i], "Fax: ([0-9-]*)")[[1]][2]
}

saveRDS(hotels, "lq/hotel_list_final.Rdata")
