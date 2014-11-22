source("check_packages.R")
check_packages(c("httr","stringr"))

dir.create("lq/", showWarnings = FALSE)

url <- "http://www.lq.com/en/findandbook.html"
page <- GET(url)
s <- content(page, as="text")

push <- unlist(str_match_all(s, "hotelList.push?.+?}"))
push <- substring(push,16, length(push))

saveRDS(push, file="lq/lq_list.Rdata")




