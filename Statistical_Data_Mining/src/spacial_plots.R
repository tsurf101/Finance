# Plotting our spacial data 
# Kmeans is unsupervised while knn is more for classification 
# Each location on our dataset represents a crime 

# install.packages(c("cowplot", "googleway","corrplot","factoextra", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "sf"))
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(factoextra)
library(corrplot)

#setwd("~/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master/") #Tom S - PLEASE DO NOT DELETE

table = read.table("Raw_DataSet/raw_crime.csv", header=TRUE, sep=",") # takes 10 seconds
#Data Cleaning in this line below 
table2 <- table[, c("Long","Lat")] %>% na.omit(table) 
table2 <- subset(table2, table2$Lat>20)
attach(table2)

set.seed(1)
location_list <- sample_n(table2, 1500) # Randomly select 1500 otherwize R will crash 

# Viewing Data below only --------
# ---- Plotting Clusters  ---- using library(leafleft) ----- I like this method below 
#option 1 - displaying
map1 <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=location_list$Long, lat=location_list$Lat, popup="Crime", radius = 2)

map1 

# -- Plotting Clusters - This is pretty cool 
map2 <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=location_list$Long, lat=location_list$Lat, popup="Crime", radius = 2, clusterOptions = markerClusterOptions())

map2  # Print the map

# ----- Using Kmeans Clustering on leaflet ----- 
# To do - run through a loop here and do more as per lab 2 and lab 7 -helpful labs 

# Using the elbow method to determine the optimal number of clusters for location data 
fviz_nbclust(location_list, FUN = hcut, method = "wss") 
# Using the elbow method we see that the optimal number would be around 6. Anything after this the marginal difference does not make sense. 
graphical_kmeans_4 <- kmeans(as.matrix(location_list), centers=4, nstart=25)
graphical_kmeans_5 <- kmeans(as.matrix(location_list), centers=5, nstart=25)
graphical_kmeans_6 <- kmeans(as.matrix(location_list), centers=6, nstart=25)
graphical_kmeans_6$tot.withinss # lowest number compared to others 

# --------------------------------
#Data Cleaning in this line below 
#test <- table %>% na.omit(table) %>% select(-Location, -OCCURRED_ON_DATE) 
#test <- table %>% na.omit(table) %>% select(OFFENSE_DESCRIPTION, OFFENSE_CODE) 
#test_matrix <- sample_n(test, 100) # Due to mememory issues randomly select 1500
# Using the elbow method to determine the optimal number of clusters 
fviz_nbclust(table, FUN = hcut, method = "wss")
# ---------------------------------

# Another way to visualize cluster plot
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p4 <- fviz_cluster(graphical_kmeans_4, geom = "point",  data = location_list) + ggtitle("k = 4")
p5<- fviz_cluster(graphical_kmeans_5, geom = "point",  data = location_list) + ggtitle("k = 5")
p6 <- fviz_cluster(graphical_kmeans_6, geom = "point",  data = location_list) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(p4, p5, p6, nrow = 2) # this looks like a mess, I don't like it. 

# below looks better 
fviz_cluster(graphical_kmeans_6, data = location_list)

#creates another column k_clust in order data set 
# And we can extract the clusters and add to our initial data to do some descriptive statistics at the cluster level:
location_list$k_clust <- graphical_kmeans_6$cluster

test <- location_list %>%
  mutate(Cluster = graphical_kmeans_6$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")  # After doing the above, grouping by clusters you can do whatever you want (last line change)..... summarise_all .. mean

#corrplot(cor(test), type = 'upper', method = 'number', tl.cex = 0.9)
# http://www.datasciencemadesimple.com/get-the-summary-of-dataset-in-r-using-dplyr-summarise/

#this will tell us how much in each cluster
graphical_kmeans_6$size

# Coloring the model # 2 options
location_list$mcolor <- plyr::mapvalues(location_list$k_clust, from=c(1, 2, 3, 4, 5, 6), 
                             to=c('blue', 'orange', 'green', 'pink','red','black'))
# Creating new Markers 
icons <- awesomeIcons(markerColor = location_list$mcolor)

# Display version 1
leafMap_clustering_v1 <- location_list %>%
  leaflet() %>% 
  addTiles() %>%
  addAwesomeMarkers(~Long, ~Lat, icon=icons)

leafMap_clustering_v1

# Display version 2

leafMap_clustering_v2 <- location_list %>%
  leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(lng=location_list$Long,
                   lat=location_list$Lat, radius = 1, color= location_list$mcolor)
leafMap_clustering_v2

# Display version 3 - This one pops out a little more

leafMap_clustering_v3 <- location_list %>%
  leaflet() %>% 
  addTiles() %>%
  #below line allows it to "pop" 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng=location_list$Long,
                   lat=location_list$Lat, radius = 1, color= location_list$mcolor)
leafMap_clustering_v3


# test_matrix <- table %>% na.omit(table) %>% select(-OCCURRED_ON_DATE, -Location,-SHOOTING, -STREET)
# test_matrix <- sample_n(test_matrix, 100)
# revised_test_matrix <- model.matrix(~., test_matrix)[,-1]

# --------------------------------------------------------------------------------------
# spectral clustering  Attempt 
library(kernlab)
#redoing the data work below 
table2 <- table[, c("Long","Lat")] %>% na.omit(table) 
table2 <- subset(table2, table2$Lat>20)
attach(table2)

set.seed(1)
location_list <- sample_n(table3, 1500) # Randomly select 1500 otherwize R will crash 
test <- data.matrix(location_list, rownames.force = NA)

sc <- specc(test, centers = 4)

plot(location_list, col=sc, pch=4)            # estimated classes (x)
points(location_list, col=obj$classes, pch=5) # true classes (<>)

library(Spectrum)
Spectrum(test)
