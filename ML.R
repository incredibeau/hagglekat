
###Load Required Packages
library(RCurl)
library(randomForest)
library(partykit)
library(rpart)
library(RWordPress)
library(rvest)

###Login to wordpress to get post data
options(WordpressLogin = c(username = 'login'), WordpressURL = 'url')

# Grab last 1000 posts
allposts = getRecentPostTitles(1000)

postid = allposts$postid

###Pull BitLy tracking information for each url

bitlys = NULL

for (i in 1:1000){
	grab = getPost(as.character(postid[i]))
	link = html(grab$permaLink)
	bly = (link %>% html_nodes("a") %>% html_attr("href"))
	test = ifelse(is.na(bly[[8]]),
		NULL,
		bly[[8]]
	)
	bitlys = rbind(bitlys, test)
	
}

# grab = getPost(as.character(postid[2]))
	# link = html(grab$permaLink)
	# bly = (link %>% html_nodes("a") %>% html_attr("href"))
	# bitlys = rbind(bitlys, bly[[8]])
	
rownames(bitlys) = NULL
all = cbind(allposts, bitlys)

colnames(all)[4] = "name"
combined = merge(all,old, by="name")
str(combined)
combined = droplevels(combined)


# get bitly stats. Insert access token ?????????

bitly.api = "https://api-ssl.bitly.com/v3/link/clicks?access_token=?????????&link="; bitly.text = "&format=txt"
combined$response = sapply(as.character(combined$bitlys), function(x) getURL(paste0(bitly.api,URLencode(x, reserved = T),bitly.text)))

# Train RF algorithm

keep = c("name","brand","price","salePrice","retailer","percentage","response","categories")
analyze = combined[keep]
str(analyze)
analyze = droplevels(analyze)
analyze$response =as.numeric(analyze$response)
analyze = analyze[complete.cases(analyze),]

half1 = head(analyze, 483)
half1 = droplevels(half1)
str(half1)

model1 = randomForest(response ~ brand + price + salePrice + retailer + percentage, data = half1)
varImpPlot(model1)

model2 = ctree(response ~ brand + price + salePrice + retailer + percentage, data = half1)
plot(model2)

library(e1071)

model3 = svm(response ~ brand + price + salePrice + retailer + percentage, data = analyze)
tuneResult <- tune(svm, response ~ brand + price + salePrice + retailer + percentage, data = analyze,
              ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tuneResult)

plot(tuneResult)
# choose best model
tunedModel <- tuneResult$best.model

#bring in data
database = readRDS("database.Rda")
str(database)

keep2 = c("brand", "price", "salePrice", "retailer", "percentage")
data = database[keep2]
data = droplevels(data)
str(data)

table(analyze$categories, responseName = analyze$response)


# predict!!!
tunedModelY <- predict(tunedModel, data) 