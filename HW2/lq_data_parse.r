source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr", "RJSONIO"))

s = readLines("lq/laqinta_list.txt")

push = unlist(str_match_all(s, "hotelList.push?.+?}"))
push=substring(push,16, length(push))

hotels <- data.frame(Title=character(), InnNumber=character(), Latitude=character(),Longitude= character(),
                     ImagePath=character(), SearchImage = character(), isInnAndSuites= character(),   
                     street = character(), street2=character(), city = character(), stateProv=character(),
                     postalCode = character(), countryDisplay=character(),
                     stringsAsFactors=FALSE) 

json_file = fromJSON(push[1])
tmp =data.frame(Title=character(), InnNumber=character(), Latitude=character(),Longitude= character(),
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

save(hotels,file = "lq/lq_list.Rdata")
