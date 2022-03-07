library(clusterSim)
library(leaflet)
library(plyr)
library(dplyr)
library(osmar)
library(RColorBrewer)
#setwd("~/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master/") #Tom S - PLEASE DO NOT DELETE

data <- read.csv("Raw_DataSet/raw_crime.csv")  
# Data Cleaning
selectedData <- data.frame(data$Lon, data$Lat) %>% na.omit(data)
selectedData <- subset(selectedData, selectedData$data.Lat>20)

# Randomly select because otherwise your R will crash or take hours to load 
set.seed(1)
selectedData <- sample_n(selectedData, 1500) # Randomly select 1500

num_of_clusters <- 6
search_dist <- 100

km <- kmeans(selectedData, num_of_clusters)
clustered_data <- data.frame(selectedData,km$cluster) 

# below is for coloring  
pal <- colorFactor(palette= brewer.pal(10,"Set3"), domain = c(1:10)) # 10 is the max num of clusters

# generating the image using leaflet 
allDataMap <- leaflet(selectedData) %>% 
  fitBounds(min(clustered_data$data.Lon),min(clustered_data$data.Lat),
            max(clustered_data$data.Lon),max(clustered_data$data.Lat))  %>%
  addTiles(group = "OSM") %>%
  addCircleMarkers(~selectedData$data.Lon, ~selectedData$data.Lat,
                   radius = 7, color = "black",fillColor=  ~pal(km$cluster), 
                   fillOpacity=0.5,  group = "Data Points")

allDataMap                              

# ---- below creates fancy squares around the clusters on the map -------
# creating a table of the box values of each cluster
data_box_df <- data.frame() 

#adding the bbox of each cluster
for (i in 1:num_of_clusters){
  ci_data <- filter(clustered_data, km.cluster==i)
  min_lon <- min(ci_data$data.Lon)
  min_lat <- min(ci_data$data.Lat)
  max_lon <- max(ci_data$data.Lon)
  max_lat <- max(ci_data$data.Lat)
  data_box <- corner_bbox(left=min_lon, bottom=min_lat, right=max_lon, top=max_lat)
  
  data_box_df <- rbind(data_box_df,data_box)
  allDataMap <- allDataMap %>% 
    addPolylines(c(data_box["left"],  data_box["right"],data_box["right"],
                   data_box["left"],  data_box["left"]),
                 c(data_box["top"],   data_box["top"],   
                   data_box["bottom"],data_box["bottom"], data_box["top"]),
                 col=(pal(x = i)), opacity = 1)  
}

allDataMap

colnames(data_box_df) <- c("left", "bottom", "right","top")
