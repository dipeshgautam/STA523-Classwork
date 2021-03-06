# Load Required packages
source("check_packages.R")
check_packages(c("httr","XML","stringr"))

# Get files
files <- dir("dennys",pattern = "[a-z]*.xml",full.names=TRUE)
dennys_data <- NULL

N <- length(files)

for(k in 1:N){
  data <- xmlParse(files[k])
  xml_data <- xmlToList(data)
  
  n <- length(xml_data$collection)-1
  
  list <- matrix(NA, nrow = n, ncol = 16)
  list <- as.data.frame(list)
  colnames(list) <- c("name","uid","latitude","longitude","city","address1","address2","postalcode","state","country","phone","clientkey","fax","other","status","travelplaza")
  
  for(i in 1:n){
    for(j in 1:dim(list)[2]){
      
      ##check data whether it is NULL
      if (xml_data$collection[i]$poi[names(xml_data$collection[i]$poi) == colnames(list)[j]] == "NULL")
      {   
        list[i,j] <- NA
      } else {
        list[i,j] <- xml_data$collection[i]$poi[names(xml_data$collection[i]$poi) == colnames(list)[j]]
      }
    }
  }
  dennys_data <- rbind(dennys_data, list)
}

dennys_datamod <- dennys_data[!duplicated(dennys_data$uid),]
dennys_datamod <- dennys_datamod[dennys_datamod$country != "DO" & dennys_datamod$country!="PR", ]


saveRDS(dennys_datamod, file="dennys/dennys_data.Rdata")
