source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

dir.create("lq/", showWarnings = FALSE)

url= "http://www.lq.com/en/findandbook.html"
d=GET(url)
s = content(GET(url), as="text")

write(s, file="lq/lq_list.txt")