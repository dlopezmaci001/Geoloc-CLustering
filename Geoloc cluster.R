# Clustering

install.packages("ggmap")
library(ggmap)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(NbClust)
library(factoextra)
library(readxl)

# Load data 

df <- read_xlsx("C:/Users/Daniel/Desktop/Nimerya/TKDSalesRegion4.xlsx")


# Determine optimal number of clusters

set.seed(20)

test1 <- scale(na.omit(data.matrix(df)[-1])) # remove na's

wssplot <- function(test1,nc=20,seed=123){
  wss<- (nrow(test1)-1)*sum(apply(test1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(test1,centers=i)$withinss)}
  plot(1:nc,wss,type="b",xlab = "Number of clusters",
       ylab="within groups sum of squares")}


# original wss plot

wssplot(test1,nc=20) # less than 5 clusters are optimal.

# Optimisation plots for 3 methods based on NbClust package

fviz_nbclust(test1,kmeans,method="wss")+
  geom_vline(xintercept=4,linetype=2)+
  labs(subtitle = "withinss")

fviz_nbclust(test1,kmeans,nstart=20,method = "gap_stat",nboot=50)+
  labs(subtitle = "gap_stat") # you have to look at when it goes up and then down so maybe, 3,5 or 8

fviz_nbclust(test1,kmeans,method = "silhouette")+
  labs(subtitle = "silhouette") # Optimal is 2 followed by 2,3 and 4

# the one that appears most is nº4 for so let's take 4 (wss tends to be the best at evaluation)

##############################Performing k means cluster analysis ###################################################

# Perform kmeans analysis based on sales and transactions
clusters <- kmeans(df[,5:6],4)

# Append the new clusters data in a new column

df$clusters <- as.factor(clusters$cluster)

# check output

str(clusters)

########################## Graph and map the kmeans clisters

# combine ciudad and provincia into single column

locations_df <- paste(df$Ciudad,",",df$Provincia)
locations_df <- tibble(locations_df)

# Get geo data

locations_geo_df <- geocode(locations_df$locations_df,source="dsk")


# bind df

df.map_locations <- cbind(locations_df,locations_geo_df)

# Add new column for sales, transactions and cluster data

df.map_locations["Sales"] <- tibble(sales =c(df$Sales))
df.map_locations["trxs"] <- tibble(trxs =c(df$Transactions))
df.map_locations["Cluster"] <- tibble(cluster =c(df$clusters))

# Group by sales

df.map_locations["size1"]<- tibble(size1= c(ifelse(df.map_locations$Sales < 50000,1,2)))


# We write out data to prevent the code from messing up

write.xlsx(df.map_locations,"C:/Users/Daniel/Desktop/Nimerya/Sales region 50K.xlxs")

df_map <- read.xlsx("C:/Users/Daniel/Desktop/Nimerya/Sales region 50K.xlxs")

# Map the data in google maps

salesamt <- df.map_locations$size1

register_google(key='AIzaSyAHdza9HVhCwR7zSepGj3Spx4yeO9KTykc')


