source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr", "RJSONIO"))

push = readRDS("~/Desktop/stat programming/Team2/HW2/lq/lq_list.Rdata")
hotels <- data.frame(Title=character(), InnNumber=character(), Latitude=character(),Longitude= character(),
                     ImagePath=character(), SearchImage = character(), isInnAndSuites= character(),   
                     street = character(), street2=character(), city = character(), stateProv=character(),
                     postalCode = character(), countryDisplay=character(),
                     stringsAsFactors=FALSE) 


json_file = fromJSON(push[1])
json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
tmp = data.frame(do.call("cbind", json_file))
tmp = rbind(tmp,data.frame(do.call("cbind", json_file)))

for (i in 2:length(push)){
  json_file <- fromJSON(push[i])
  json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  tmp = rbind(tmp,data.frame(do.call("cbind",json_file)))
}

hotels = tmp[!duplicated(tmp$nnNumbe),]
colnames(hotels)= c("Title","InnNumber","Latitude","Longitude", "ImagePath", "isInnAndSuites","street","street2", "city", "stateProv",
                    "postalCode", "countryDisplay")

saveRDS(hotels,file = "lq/lq_URLS.Rdata")
