## load required packages
library(rvest)
library(plyr)

## for beta testing, stored manual query URLs as the vector 'lists'
lists = c("http://shop.nordstrom.com/c/womens-dresses-shop?dept=8000001&origin=topnav#category=b2374331&type=category&marketingslots=2&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/womens-tops-tees?dept=8000001&origin=topnav#category=b2379676&type=category&marketingslots=2&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/womens-shoes?dept=8000001&origin=topnav#category=b60128263&type=category&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=8&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/womens-skirts?dept=8000001&origin=topnav#category=b60140331&type=category&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/sale-womens-clothing-dresses?origin=leftnav#category=b60136820&type=category&marketingslots=1&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/sale-womens-tops?origin=leftnav#category=b60140497&type=category&marketingslots=2&defaultsize3=&size=regularxs&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=size&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1&partial=1&pagesize=100&contextualsortcategoryid=0","http://shop.nordstrom.com/c/sale-womens-denim?origin=leftnav#category=b60140424&type=category&marketingslots=1&defaultsize3=&size=regularxs&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=size&sizeFinderId=2&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1&partial=1&pagesize=100&contextualsortcategoryid=0","http://shop.nordstrom.com/c/sale-womens-shoes?origin=leftnav#category=b60140511&type=category&marketingslots=1&defaultsize3=&size=&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=&sizeFinderId=8&resultsmode=&segmentId=0&sort=newest&sortreverse=0&page=1","http://shop.nordstrom.com/c/sale-womens-skirts?origin=leftnav#category=b60140496&type=category&defaultsize3=&size=regularxs&width=&color=&price=&brand=&stores=&instoreavailability=false&lastfilter=size&sizeFinderId=2&resultsmode=&segmentId=0&page=1&partial=1&pagesize=100&contextualsortcategoryid=0&sort=newest&sortreverse=0")
types =c("dresses","tops","shoes","skirts","dresses","tops","jeans","shoes","skirts")

nordstrom = NULL

## loop through each search URL and pull first page data

for (i in 1:length(lists)){
html = html(lists[i])

#get name / link
cast = html_nodes(html, ".title")
name = html_text(cast)
link = paste0("http://shop.nordstrom.com",html_attr(cast,"href"))


#get image
cast2 = html_nodes(html, ".fashion-photo img")
image = html_attr(cast2, "data-original")

#get current price
cast3 = html_nodes(html, ".regular")
price1 = html_text(cast3)


#get original price
cast4 = html_nodes(html, ".sale")
price2 = html_text(cast4)

combined = data.frame(cbind(name,link,image, price1, price2),stringsAsFactors = FALSE)
combined$type = types[i]
combined$date = Sys.time()

nordstrom = rbind(nordstrom, combined)
}

str(nordstrom)

## go through each idem and add product details
# If this takes too long, comment it out

html_sub = lapply(as.character(nordstrom$link), html)
cast_product = lapply(html_sub, html_nodes, css = ".details-and-care")
test = lapply(cast_product, html_nodes, css = "p, li")
attributes = lapply(test, html_text)
text = data.frame(unlist(lapply(attributes, paste, collapse = " ")), stringsAsFactors = F)

nordstrom = cbind(nordstrom, text)

str(nordstrom)

colnames(nordstrom)[8] = c("text")

### clean up dataframe

nordstrom$discount = grepl("Was", ignore.case = T, nordstrom$price1)
nordstrom$price1 = as.numeric(gsub("\\,","",gsub("\\-.*","",gsub('^.*?[$]','',ignore.case = TRUE, nordstrom$price1))))
nordstrom$price2 = as.numeric(gsub("\\,","",gsub("\\-.*","",gsub('^.*?[$]','',ignore.case = TRUE, nordstrom$price2))))
nordstrom$type = as.factor(nordstrom$type)
nordstrom[is.na(nordstrom)] <- 0
nordstrom$sale = pmin(nordstrom$price1, nordstrom$price2)
nordstrom$regular = pmax(nordstrom$price1, nordstrom$price2)
nordstrom$retailer = "nordstrom"

# # save dataframe as .Rda object

str(nordstrom)
saveRDS(nordstrom, file="nordstrom.Rda")


