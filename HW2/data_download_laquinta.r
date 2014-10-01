source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite","rgeos","maptools","stringr"))

dir.create("lq/", showWarnings = FALSE)


url= "http://www.lq.com/en/findandbook.html"
d=GET(url)
s = content(d, as="text")
# a <- str_match_all(s, "push(.*countryDisplay: \"[a-zA-Z ]*\")") 
file = paste0(laquinta.txt")
write(s, file="lq/laqinta_list.html")

      
{title: "La Quinta Inn & Suites St. George",
 innNumber: "6024",
 latitude:  "37.06301", longitude: "-113.58283",
 imagePath: "/bin/lq-com/hotelSearchImage.6024.jpg",
 isInnAndSuites: "true",
 street: "91 East 2680 South",
 street2: "",
 city: "Saint George",
 stateProv: "UT",
 postalCode: "84790",
 countryDisplay: "United States"
}