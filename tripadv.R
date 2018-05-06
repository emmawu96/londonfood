library(tidyverse)
#install.packages("munsell")
library(munsell)
library(ggplot2)
library(dplyr)

data <- read_csv("tripadvisor_co_uk-travel_restaurant_reviews_sample.csv")
data1 <- select(data,c("restaurant_id","name","review_date","title","review_text","author","location","rating","food","value","service","visited_on"))

## now get the count for each column
count<- apply(data1, 2, n_distinct)

#change rating variables to numeric
data2 <- data1 %>% 
          filter(str_detect(location, 'London')) %>%
          separate(rating,into= c("score","other"), sep= "of")  %>% 
          separate(food,into= c("food","fother"), sep= "of")  %>% 
          separate(value,into= c("value","vother"), sep= "of") %>% 
          separate(service,into= c("service","sother"), sep= "of") 
data2 <- data2 %>% select(-c(other,fother,vother,sother)) 

data2$score <- as.numeric(data2$score)
data2$food <- as.numeric(data2$food)
data2$value <- as.numeric(data2$value)
data2$service <- as.numeric(data2$service)

#group by restaurants
data3<- as.tibble(data2)
by_res <- data3 %>% group_by(name)



summarise(by_res,mean=mean(score),sd=sd(score),na.rm=TRUE)

#t test on means of the scores
## overall score vs. food
t.test(by_res$score,by_res$food, na.action=na.omit)
## overall score vs. value
t.test(by_res$score,by_res$value, na.action=na.omit)
## overall score vs. service
t.test(by_res$score,by_res$service, na.action=na.omit)

#linear regression
linearMod <- lm(score ~ food + value + service , data=by_res)
summary(linearMod)
layout(matrix(c(1,2,3,4),2,2))
plot(linearMod)


#text analysis
library(stringr)
library(tidytext)

## analysis on review_text
data4 <-separate(data3,visited_on,c("month","year"))
scores <- data4 %>%
  select(name,score,year) %>%
  group_by(name,year) %>%
  summarise(score = mean(score))

original_texts <- data4 %>% select(c("name","year","review_text")) %>%
                  group_by(name,year)
data(stop_words)

tidy_reviews <- original_texts %>%
  unnest_tokens(word, review_text) %>% 
  ungroup() %>%
  anti_join(stop_words)

tidy_reviews %>%
  count(word, sort = TRUE)

###wordcloud
library(wordcloud)
library(reshape2)
tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

###bing sentiments
bing_pos <- get_sentiments("bing") %>%
  filter(sentiment=="positive")
bing_neg <- get_sentiments("bing") %>%
  filter(sentiment=="negative")

###sentiment analysis
review_sentiments <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(name, year, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  inner_join(scores)

### plots of sentiment on scores
review_plot <- ggplot(review_sentiments, aes(score, sentiment,color=year))
review_plot +
  geom_point(show.legend = FALSE) +
  coord_cartesian(ylim = c(-20, 180)) 
#### scatter with smooth line
review_plot +
  stat_smooth(se=F)

### regression analysis
linearMod2 <- lm(sentiment ~ score , data=review_sentiments)
summary(linearMod2)
#### regression line plot
review_plot +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm',formula=y~x,se=F)



## analysis on review title
original_titles <- data4 %>% select(c("name","year","title")) %>%
  group_by(name,year)
tidy_titles <- original_titles %>%
  unnest_tokens(word, title) %>% 
  ungroup()%>%
  anti_join(stop_words)

tidy_titles %>%
  count(word, sort = TRUE)

### wordcloud 
tidy_titles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


###sentiment analysis
title_sentiments <- tidy_titles %>%
  inner_join(get_sentiments("bing")) %>%
  count(name, year, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  inner_join(scores)

### plots of sentiment on scores
title_plot <- ggplot(title_sentiments, aes(score, sentiment,color=year))
title_plot +
  geom_point(show.legend = FALSE) +
  coord_cartesian(ylim = c(-10, 45)) 
#### scatter with smooth line
title_plot +
  stat_smooth(se=F)

## title vs. review
names(title_sentiments)[names(title_sentiments)=="sentiment"] <- "titleSentiment"
names(review_sentiments)[names(review_sentiments)=="sentiment"] <- "reviewSentiment"
title_review <- full_join(title_sentiments,review_sentiments)

title_review_plot <- ggplot(title_review, aes(x=score))
####ignored some high sentiment outliers
title_review_plot + 
  geom_point(data=title_review, aes(y=titleSentiment), color="blue", show.legend = FALSE) +
  geom_point(data=title_review, aes(y=reviewSentiment), color="pink", show.legend = FALSE) +
  stat_smooth(aes(y=titleSentiment), color="blue",se=F) +
  stat_smooth(aes(y=reviewSentiment), color="pink",se=F) +
  coord_cartesian(ylim = c(-20, 100)) 



# map of the restaurants
library(ggmap)
london <- geocode("london, england")
map <- get_map(london, zoom=11)    ##modify the map location to adjust the view


# data frame with name and avg score
avg_score <- scores %>% group_by(name) %>%
  summarise(avg_score=mean(score, na.rm = TRUE))

# high variable: 1 if avg_score higher than 4
avg_score$high<- as.numeric(avg_score$avg_score>=4)
avg_score$high <- cut(avg_score$high, breaks = c(0,0.1,1), labels = c(0,1)) 

# geocodes for the restaurants
# avg_score$geo <- geocode(paste(avg_score$name,"london, england", sep=", "))


# map with circle size depending on avg score
ggmap(map) +
  geom_point(
    aes(x = geo$lon, y = geo$lat, color = avg_score),
    data = avg_score,
    alpha=0.4, size=(avg_score$avg_score^2)*circle_scale_amt)+ 
  scale_size_continuous(range=range(avg_score$avg_score)) +
  scale_color_distiller(type = "seq", palette = "Spectral")

###############to do tmr##################
avg_score$geo[is.na(avg_score$geo.lon)] <- geocode(paste(avg_score$name,"london, uk", sep=", "))