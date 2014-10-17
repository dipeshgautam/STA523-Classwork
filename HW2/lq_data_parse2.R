source("check_packages.R")
check_packages(c("httr", "stringr"))


hotels <- readRDS("lq/lq_URLS.Rdata")
urls <- readRDS("lq/all_urls.Rdata")

for (i in 1:nrow(hotels)){
  hotels$phone[i] <- str_match_all(urls[i], "Phone: ([0-9-]*)")[[1]][2]
  hotels$fax[i] <- str_match_all(urls[i], "Fax: ([0-9-]*)")[[1]][2]
}

saveRDS(hotels, "lq/hotel_list_final.Rdata")
