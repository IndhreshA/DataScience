

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)

market <- MarketingAnalysis
str(market)

#Data Cleaning

names(market)

head(market)

summary(market)

summary(market$spending_score)

summary(market$income)

summary(market$last_purchase_amount)

mean(market$income)

mean(market$spending_score)

mean(market$membership_years)

mean(market$last_purchase_amount)

sd(market$income)

sd(market$spending_score)

sd(market$purchase_frequency)

gender_freq = table(market$age)

#Data Visualization

barplot(gender_freq, main = "Gender Frequency Plot", xlab="Gender", ylab="Frequency", col="Green")

boxplot(market$income, main="Income Statistics", col="Green")

product_freq = table(market$preferred_category)

barplot(height = product_freq, main="Frequency of Preffered Product Category", xlab="Category", ylab="Frequency", col="Blue")

hist(market$age, main="Distribution of Age", xlab="Age",ylab="Frequency", col="Red", labels=TRUE)


member <-  market
member %>% 
  group_by(preferred_category) %>% 
  mutate(mean_membershipYears = mean(membership_years)) %>% 
  select(preferred_category, mean_membershipYears, gender) %>% 
  ggplot(member, mapping= aes(fill=preferred_category,y=mean_membershipYears, x=preferred_category)) +
    geom_bar(position = 'dodge', stat='identity') +
    ggtitle("Preffered Category Based on membership Years")+
  xlab("Category")+
  ylab("Average Membership Years")

#K-Means Clustering

##Elbow Method

library(purrr)

cluster_df <- subset(market, select=-c(gender, preferred_category, id))

names(cluster_df)

set.seed(124)


iss <- function(k) { kmeans(cluster_df, k, iter.max=100, nstart=100, algorithm="Lloyd")$tot.withinss }

k.values <-  1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values, type="b", pch=18, frame=TRUE, xlab="Number Of Clusters K", 
     ylab="Total Intra-Clusters Sum Of Squares")



##Silhouwette Method

library(cluster)
library(gridExtra)
library(grid)

k2 <- kmeans(cluster_df, 2, iter.max=100,nstart=50,algorithm="Lloyd")
s2 <- plot(silhouette(k2$cluster, dist(cluster_df, "euclidean")))


k3 <- kmeans(cluster_df, 3, iter.max=100,nstart=50,algorithm="Lloyd")
s3 <- plot(silhouette(k3$cluster, dist(cluster_df, "euclidean")))

k4 <- kmeans(cluster_df, 4, iter.max=100,nstart=50,algorithm="Lloyd")
s4 <- plot(silhouette(k4$cluster, dist(cluster_df, "euclidean")))

k5 <- kmeans(cluster_df, 5, iter.max=100,nstart=50,algorithm="Lloyd")
s5 <- plot(silhouette(k5$cluster, dist(cluster_df, "euclidean")))

k6 <- kmeans(cluster_df, 6, iter.max=100,nstart=50,algorithm="Lloyd")
s6 <- plot(silhouette(k6$cluster, dist(cluster_df, "euclidean")))

k7 <- kmeans(cluster_df, 7, iter.max=100,nstart=50,algorithm="Lloyd")
s7 <- plot(silhouette(k7$cluster, dist(cluster_df, "euclidean")))

k8 <- kmeans(cluster_df, 8, iter.max=100,nstart=50,algorithm="Lloyd")
s8 <- plot(silhouette(k8$cluster, dist(cluster_df, "euclidean")))

k9 <- kmeans(cluster_df, 9, iter.max=100,nstart=50,algorithm="Lloyd")
s9 <- plot(silhouette(k9$cluster, dist(cluster_df, "euclidean")))

k10 <- kmeans(cluster_df, 10, iter.max=100,nstart=50,algorithm="Lloyd")
s10 <- plot(silhouette(k10$cluster, dist(cluster_df, "euclidean")), col='darkblue')


library(ggplot2)
library(NbClust)
library(factoextra)
library(car)

fviz_nbclust(cluster_df, kmeans, method = "wss")

fviz_nbclust(cluster_df, kmeans, method = "silhouette")

##Gap Statistic Method


k2 <- kmeans(cluster_df, 2, iter.max=100,nstart=50,algorithm="Lloyd")
s2 <- plot(silhouette(k2$cluster, dist(cluster_df, "euclidean")))

### Principle Cluster Analysis

pcclust <- prcomp(cluster_df)

summary(pcclust)

pcclust$rotation

set.seed(42)

### Plotting 

ggplot(cluster_df, aes(x='income', y='spending_score' )) +
  geom_point(stat='identity', aes(color=as.factor(k2$cluster))) +
  scale_color_discrete(name=' ',
                       breaks=c("1","2"),
                       labels=c('Cluster 1', 'Cluster 2')) +
  ggtitle("Segments of Customers", subtitle="Using K-Means Clustering Technique")

kcols = function(vec){cols=rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])}

digCluster <- k2$cluster; 

dignm <- as.character(digCluster) 

plot(pcclust$x, col=kcols(digCluster), pch=19, xlab="K-Means", ylab="Classes")
legend("bottomleft", unique(dignm), fill=unique(kcols(digCluster)))


