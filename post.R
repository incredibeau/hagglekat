library(RWordPress)
library(jsonlite)
library(knitr)
library(RCurl)
# create staging doc

#read in recent deals
database = readRDS("nordstrom.Rda")
#read in already posted deals
posted = readRDS("posted.Rda")


database = subset(database, !(name %in% posted$name))
#database = database[complete.cases(database),]

raw = database[sample(1:nrow(database), 1, replace = F),]

#####
# Disabled to preserve TradeSecret
# Machine Learning Sorting Algorithm code goes here. See ML.R code for example RandomForest implementation
####

final = raw

options(WordpressLogin = c(username = 'login'),
        WordpressURL = 'url')

#post to WP and save out new 
stage$postid = knit2wp('newpost.Rmd', title = as.character(stage$name), action = "newPost", publish = T, mt_keywords = c("Train",as.character(stage$categories),as.character(stage$retailer),as.character(stage$brand))) 
old = rbind(posted, final); saveRDS(old, file = "posted.Rda" )