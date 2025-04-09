library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
library(textdata)



#Data Cleaning

sentiment = sentimentdataset

glimpse(sentiment)


colnames(sentiment)
     
head(sentiment)


sentiment$`Unnamed: 0` <- NULL
sentiment$...1 <-  NULL
sentiment <-  subset(sentiment, select=-c(Year, Month, Day, Hour))

unique(sentiment$Country)

unique(sentiment$Platform)
head(unique(sentiment$Sentiment))
head(unique(sentiment$User))

summary(sentiment$Likes)
sd(sentiment$Likes)
summary(sentiment$Retweets)
sd(sentiment$Retweets)

head(unique(sentiment$Sentiment))

glimpse(sentiment)


sentiment %>% 
  group_by(Platform) %>% 
  summarize(avg_retweet = mean(Retweets)) %>% 
  select(Platform, avg_retweet)

sentiment %>% 
  group_by(Platform) %>% 
  summarize(avg_likes = mean(Likes)) %>% 
  select(Platform, avg_likes)


facebook <- sentiment[sentiment$Platform == "Facebook", ]
head(facebook$Sentiment, 30)

instagram <- sentiment[sentiment$Platform == "Instagram", ]
head(instagram$Sentiment, 30)

usa <- sentiment[sentiment$Country == "USA", ]
head(usa$Sentiment, 20)
head(usa$Text,20)

unique(sentiment$Country)

india <- sentiment[sentiment$Country == "India",]
head(india$Sentiment, 20)
head(india$Text,20)

japan <- sentiment[sentiment$Country == "Japan",]
head(japan$Sentiment, 20)
head(japan$Text, 20)



#Data Visualization


platform_freq = table(sentiment$Platform)
usa_platforms = table(usa$Platform)
india_platforms = table(india$Platform)
japan_platforms = table(japan$Platform)
barplot(platform_freq,
        main="Popular Social Media Platforms", 
        xlab="Platforms", 
        ylab="Frequency", col="green")
barplot(usa_platforms,
        main="USA Popular Social Media Platforms", 
        xlab="Platforms", 
        ylab="Frequency", col="purple")
barplot(india_platforms,
        main="India Popular Social Media Platforms", 
        xlab="Platforms", 
        ylab="Frequency", col="orange")
barplot(japan_platforms,
        main="Japan Popular Social Media Platforms", 
        xlab="Platforms", 
        ylab="Frequency", col="red")



head(sentiment$Platform, 3)
unique(sentiment$Country)


unique(sentiment$Country)

#USA: 33
# China: 7
# India: 14
# Japan: 18

 ### Platforms Encoding
# Twitter: 3
# Instagram: 2
# Facebook: 1


head(sentiment$Platform, 3)

afinn = get_sentiments(lexicon="afinn")

sentimentValues_df <- sentiment %>% 
  mutate(index=row_number()) %>% 
  unnest_tokens(word, Sentiment) %>% 
  inner_join(afinn) %>% 
  group_by(index) %>% 
  summarise(sentimentVal = sum(value)) %>% 
  left_join(
    sentiment%>% 
      mutate(index=row_number())
  )

textValues_df <- sentimentValues_df %>% 
  mutate(index=row_number()) %>% 
  unnest_tokens(word, Text) %>% 
  inner_join(afinn) %>% 
  group_by(index) %>% 
  summarise(textVal = sum(value))%>% 
  left_join(
    sentimentValues_df%>% 
      mutate(index=row_number())
  )

encoded <- textValues_df %>% 
  mutate(index=row_number()) %>% 
  unnest_tokens(word, Hashtags) %>% 
  inner_join(afinn) %>% 
  group_by(index) %>% 
  summarise(HashtagsVal = sum(value)) %>% 
  left_join(
    textValues_df%>% 
      mutate(index=row_number())
  )

encoded$sentimentVal
encoded$textVal
encoded$HashtagsVal
names(encoded)
cluster_df <- subset(encoded, select=
                       -c(index, 
                          Timestamp, User,Hashtags,
                          Text, Sentiment))

ggplot(data=cluster_df, mapping=aes(x=sentimentVal, y=Retweets))+
  geom_point(aes(color=Platform))+
  ggtitle(label="Sentiments On Various Social Media Platforms", 
          subtitle="(+)Positive and (-)Negative Scores ")+
  xlab("Sentiment Score")+
  ylab("Number Of Retweets")

ggplot(data=cluster_df, mapping=aes(x=Country, y=sentimentVal))+
  geom_col(aes(color=Platform))+
  ggtitle(label="Sentiments On Various Social Media Platforms", 
          subtitle="(+)Positive and (-)Negative Scores ")+
  xlab("Country")+
  ylab("SentimentScore")



# K-Means Clustering 

sentiment$Country <- unclass(factor(sentiment$Country))
sentiment$Platform <- unclass(factor(sentiment$Platform))


library(purrr)
set.seed(1234)
glimpse(sentiment)




## Elbow Method
names(cluster_df)


cluster_df$Platform <- as.numeric(as.factor(cluster_df$Platform))
cluster_df$Country <- as.numeric(as.factor(cluster_df$Country))

head(cluster_df$Platform, 3)

iss <- function(k) {
  kmeans(cluster_df, k,
         iter.max = 100, nstart=100, algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values, type="b", pch=18, frame=FALSE, 
     xlab="Number of Clusters k",
     ylab="Total Intra-Clusters Sum of Squares")

## Silhouette Method

library(cluster)
library(grid)
library(gridExtra)

k2 <- kmeans(cluster_df,
             2, iter.max = 100, nstart = 50, algorithm="Lloyd")
s2 <- plot(silhouette(
  k2$cluster, dist(cluster_df, "euclidean")))

k3 <- kmeans(cluster_df,
             3, iter.max = 100, nstart = 50, algorithm="Lloyd")
s3 <- plot(silhouette(
  k3$cluster, dist(cluster_df, "euclidean")))

k4 <- kmeans(cluster_df,
             4, iter.max = 100, nstart = 50, algorithm="Lloyd")
s4 <- plot(silhouette(
  k4$cluster, dist(cluster_df, "euclidean")))

k5 <- kmeans(cluster_df,
             5, iter.max = 100, nstart = 50, algorithm="Lloyd")
s5 <- plot(silhouette(
  k5$cluster, dist(cluster_df, "euclidean")))

k6 <- kmeans(cluster_df,
             6, iter.max = 100, nstart = 50, algorithm="Lloyd")
s6 <- plot(silhouette(
  k6$cluster, dist(cluster_df, "euclidean")))

k7 <- kmeans(cluster_df,
             7, iter.max = 100, nstart = 50, algorithm="Lloyd")
s7 <- plot(silhouette(
  k7$cluster, dist(cluster_df, "euclidean")))

k8 <- kmeans(cluster_df,
             8, iter.max = 100, nstart = 50, algorithm="Lloyd")
s8 <- plot(silhouette(
  k8$cluster, dist(cluster_df, "euclidean")))

k9 <- kmeans(cluster_df,
             9, iter.max = 100, nstart = 50, algorithm="Lloyd")
s9 <- plot(silhouette(
  k9$cluster, dist(cluster_df, "euclidean")))

k10 <- kmeans(cluster_df,
             10, iter.max = 100, nstart = 50, algorithm="Lloyd")
s10 <- plot(silhouette(
  k10$cluster, dist(cluster_df, "euclidean")))



library(ggplot2)
library(NbClust)
library(factoextra)
library(car)

fviz_nbclust(cluster_df, kmeans, method="wss")
fviz_nbclust(cluster_df, kmeans, method="silhouette")

## Gap Statistic Method

k2 <- kmeans(cluster_df,
             2, iter.max = 100, nstart = 50, algorithm="Lloyd")
s2 <- plot(silhouette(
  k2$cluster, dist(cluster_df, "euclidean")))

### Principle Cluster Analysis

pcclust <- prcomp(cluster_df)

summary(pcclust)

pcclust$rotation

names(cluster_df)



### Platforms Encoding
# Twitter: 3
# Instagram: 2
# Facebook: 1
names(cluster_df)
ggplot(cluster_df, aes(x=Country, y=sentimentVal)) +
  geom_point(stat='identity', aes(color=as.factor(k2$cluster))) +
  scale_color_discrete(name=' ',
                       breaks=c("1","2"),
                       labels=c("Cluster1", "Cluster2"))+
  ggtitle("Sentiments in Social Media", 
          subtitle="Using k-Means Clustering Technique")

#USA: 33
# China: 7
# India: 14
# Japan: 18


kcols = function(vec){
  cols=rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}

digCluster <- k2$cluster;

dignm <- as.character(digCluster)

plot(pcclust$x, col=kcols(digCluster), pch=19, xlab="K-Means", ylab="Classes")
legend("bottomleft", unique(dignm), fill=unique(kcols(digCluster)))



