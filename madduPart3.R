install.packages('rtweet')
library(rtweet)
library(twitteR)
library(twitteR)
library(ggplot2)
library(ggmap)
library(maps)
library(RJSONIO)
library(plyr)
library(dplyr)

## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

## install dev version of rtweet from github
devtools::install_github("mkearney/rtweet")

## load rtweet package
library(rtweet)
install.packages("openssl")
install.packages("httpuv")
library(openssl)
library(httpuv)
# 
# api_key = ""
# api_secret = ""
# access_token = ""
# access_secret = ""
# 
# setup_twitter_oauth(api_key, api_secret, access_token=NULL, access_secret= NULL)
# 
# 

# user_input <- "#influenza OR influenza OR flu OR H3 OR H1N1"
# user_input_number <- 10000
# ## For USA geocode=39.8,-95.583068847656,2500km
# tweets2 <- search_tweets(n=user_input_number,lang='en',q=user_input,retryOnRateLimit=120, geocode=lookup_coords("usa"))
# tweets2
# 
# user_input <- "#influenza OR influenza OR flu"
# tweets1<-  search_tweets(n=user_input_number,lang='en',q=user_input,retryOnRateLimit=120, geocode=lookup_coords("usa"))
# tweets1
# 
# allTweets <- rbind(tweets1, tweets2)
# 
# save_as_csv(allTweets,  file="/media/parik/New Volume/DIC/Lab1Part3/PNEWTweets1.csv")
# #m1<- read.csv(file.choose())
# 
# latLong<- lat_lng(allTweets)
# latLong<- latLong[!(is.na(latLong$lat)),]
# 
# 
# ################
# 
# 
# revAll2 <- data.frame(address=character(), stringsAsFactors = FALSE)
# colnames(revAll2)<- c("address")
# 
# for (i in 1:nrow(latLong)) {
#   revGeo<-revgeocode(c(as.numeric(latLong$lng[i]), as.numeric(latLong$lat[i])), output = "address")
#   revAll2[nrow(revAll2)+1,] <- c(as.character(revGeo))
#   #revAll<- rbind(revAll, revGeo)
#   #revAll$address[i] <- revGeo$address
#   #revAll$locality[i] <- revGeo$locality
#   #revAll$administrative_area_level_2[i] <- revGeo$administrative_area_level_2
#   #revAll$administrative_area_level_1[i] <- revGeo$administrative_area_level_1
# }
# 
# revAll2 <- data.frame(revAll2$address)
# 
# 
# revAll2 <-revAll2[revAll2$revAll2.address!="NA",]
# 
# ################Saving location of tweets for USA
# write.csv(revAll2, file="/media/parik/New Volume/DIC/Lab1Part3/final1_LocationsOfTweets.csv", row.names = F )

###############states Data extraction
newState2 <- data.frame(address=character(), stringsAsFactors = FALSE)
colnames(newState2)<- c("state_data")
#revAll<-revaAll 

####Loading the saved locations data for tweets
revAllNew <- as.data.frame("final1_LocationsOfTweets.csv", stringsAsFactors = F))

for (i in 1:nrow(revAllNew)) {
  t<- as.data.frame(strsplit(as.character(revAllNew$revAll.address[i]),"[,]"))
  if(!(is.na(t[4,]))){
    if(trimws(t[4,])=="USA"){
      t <- gsub("[0-9]","",t[3,])
      t<-trimws(t)
      newState2[nrow(newState2)+1,] <- c(as.character(t))
    }
  }
  
}


#load statecode n full name file
statesnCodes<- read.csv(file.choose(), stringsAsFactors = FALSE)
mergedStates<- merge(newState2, statesnCodes, by.x="state_data", by.y="Abbreviation.", all.x=TRUE)

freq<-as.data.frame(table(mergedStates$us.state.))

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)


data("fifty_states") # this line is optional due to lazy data loading

df <- dplyr::select(input, 1, 4:5)
levels(df$STATENAME) <- tolower(levels(df$STATENAME))
DF <- df[c(1:8, 10:51),]

# map_id creates the aesthetic mapping to the state name column in your data
p <- ggplot(freq, aes(map_id = freq$Var1)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = freq$Freq), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()+
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

p

#####################################################################
        #####  ######   #######
        #      #    #   #      
        ###    #    #   #
        #      #    #   #
        #####  ######   #######
#####################################################################



