#1

#Team Name : Team 1 Time and Location Analysis

#Team Members : Eniola Webster-Esho,Alexius Fly & Ozioma Aguegboh


install.packages("ggthemes")
install.packages("leaflet")
install.packages("tidyverse")
install.packages("fiftystater")
install.packages("ggthemes")
install.packages("xts")

library(maps)
library(leaflet)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(xts)

#2
#Create a data frame with required columns
tweets_w_day_hour <- clean_tweets %>%
  mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  mutate(day = wday(created_at, label=TRUE)) %>%
  mutate(hour = as.POSIXlt(created_at)$hour)%>%
  mutate(date = as.Date(created_at))

#View tweets by day of the week
ggplot(data = tweets_w_day_hour, aes(x = day)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#3
#View tweets by date
ggplot(data = tweets_w_day_hour, aes(x = date)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Date") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#4
#View tweets by day and hour without boxplot
ggplot(tweets_w_day_hour)+geom_jitter(aes(x=day,y=hour))

#5
#View tweets by day and hour with boxplot
ggplot(tweets_w_day_hour)+geom_boxplot(aes(x=day,y=hour))+geom_jitter(aes(x=day,y=hour))

#6
#Plot a timeseries
ts <- xts(rep(1,times=nrow(tweets_w_day_hour)),tweets_w_day_hour$created_at)
ts_sum <- apply.daily(ts,sum) 
ts_sum_df <- data.frame(date=index(ts_sum), coredata(ts_sum))
colnames(ts_sum_df)=c('date','sum')
ggplot(ts_sum_df)+geom_line(aes(x=date,y=sum))


#7
nba_tweets_w_location <- clean_tweets %>% filter(!is.na(location))
View(nba_tweets_w_location)

#First create a data frame with the map data 
map.data <- map_data("state")

#Set latitue and longitude
tweets_w_day_hour$lat <- ifelse(is.na(tweets_w_day_hour$lat), tweets_w_day_hour$place_lat, tweets_w_day_hour$lat)
tweets_w_day_hour$lon <- ifelse(is.na(tweets_w_day_hour$lon), tweets_w_day_hour$place_lon, tweets_w_day_hour$lon)
tweets_w_day_hour <- tweets_w_day_hour[!is.na(tweets_w_day_hour$lat),]
states <- map.where("state", tweets_w_day_hour$lon, tweets_w_day_hour$lat)
head(sort(table(states), decreasing=TRUE))

#8
#Use ggplot2 to draw the map:
# 1) map base
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  # 2) limits for x and y axis
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  # 3) Add the dot for each tweet
  geom_point(data = tweets_w_day_hour, 
             aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "darkblue") +
  # 4) Remove unnecessary graph elements
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 


#9
#View tweet locations on a world map
install.packages("ggthemes")
library(ggthemes)

world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()+
  geom_point(data = tweets_w_day_hour, aes(x = lon, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = " NBA Playoffs Tweet Locations between 04/19 and 04/27 2022")
world_basemap



#10
#Create an interactive map of the data using leaflet
site_locations_base <- leaflet(tweets_w_day_hour) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~text,
                   radius = 3, stroke = FALSE)
site_locations_base
