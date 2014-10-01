# Load Required packages
source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools"))


# Get files
files = dir("json/",full.names=TRUE)





dennys = data.frame("DATA!!!")

save(dennys, file="dennys/dennys_data.Rdata")