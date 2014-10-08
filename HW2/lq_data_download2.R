source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

hotels <- readRDS("lq/lq_URLS.Rdata")

s <- vector(length=nrow(hotels))
for(j in 1:nrow(hotels)){
  s[j] <- RCurl::httpGET(hotels$urls[j])
  if (j%%50 == 0){
  cat("Downloading LQ:",j,"\n")
  }
  Sys.sleep(runif(1))
}

saveRDS(s, file =  "lq/all_urls.Rdata")
