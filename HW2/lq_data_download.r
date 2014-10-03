source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

dir.create("lq/", showWarnings = FALSE)

url= "http://www.lq.com/en/findandbook.html"
page = GET(url)
s = content(page, as="text")
write(s, file="lq/lq_list.txt")




