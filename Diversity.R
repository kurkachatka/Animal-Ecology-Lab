### Aim of this script: 
# Overlap point information with remote sensing data
# Calculate different Index around the observation points

## Download the libraries
library(tidyverse) ## needed to clean the data set
library(readxl) ## needed to open xlsx file
#library(raster) ## needed to import the .tif layer
library(terra)  ## raster handling (fast), prefer over raster package
library(sf) ## needed to convert data from degrees to meters
library(dplyr) ## needed to manipulate tables
library(ggplot2) ## needed to create graph

### Set working directories: Go to the tab Session > Set Working Directories > Choose Directory ... => look for the file in which you uploaded your Coordinates.xlsx file
setwd("~/Library/CloudStorage/OneDrive-UniversityofGdansk(forStudents)/PhD/Class/Teaching/Marta/Diversity_class")


##### First Part: Overlap point information with remote sensing data #####
### Import the files required for the manipulation:
## First, import the data set with information about the land cover:
Oliwa_landscape <- read.csv("Oliwa_landscape.csv",sep=",",dec=".",h=T)

## Second, import the dataset with coordinates:
Coordinates <- read_excel("Coordinates.xlsx")

### Select your own Group of points from the main Coordinates file:
Coordinates_Group <- Coordinates %>% filter(Group == XX) ## Replace XX by your group number => this will only keep the GPS coordinates Longitude and Latitude from your Group
## Example: If your Group number was 2:
# Coordinates_Group <- Coordinates %>% filter(Group == 2)


### As we are working with GPS information, we need to make sure that the data we are using is express in the same unit. 
### In remote sensing term, this unit is known as projection
### Here we want all our data to be in the projection stored in the following file
raster_crs <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGdansk(forStudents)/PhD/Class/Teaching/Marta/Diversity_class/raster_crs.RData")


## To apply raster_crs to our data.frame, we need to convert our dataset in spatial objects:
## Oliwa_landscape comes from a pre-process file that has the projection from raster_crs, in this case we know it already has the good format
crs(Oliwa_landscape, proj=T) ## We get the following error message: Error: unable to find an inherited method for function ‘crs’ for signature ‘x = "data.frame"’

## Convert into a sf object:
Coordinates_Group_sf <- st_as_sf(Coordinates_Group, coords = c("Longitude", "Latitude"),crs=4326)
Coordinates_Group_sf
## Convert into the unit stored in raster_crs
Coordinates_Group_laea <- st_transform(Coordinates_Group_sf, crs = st_crs(raster_crs))
Coordinates_Group_laea
crs(Coordinates_Group_laea, proj=T)

## We want to get a 20m buffer around our Coordinates_Group_laea points:
buffer_m <- 20
buffers_laea <- st_buffer(Coordinates_Group_laea, dist = buffer_m)
crs(buffers_laea,proj=T) 


## Visualize the three layers together: Oliwa_landscape, Coordinates_laea and buffers_laea
ggplot() +
  geom_raster(data = Oliwa_landscape, aes(x = x, y = y, fill = landcover)) +
  scale_fill_manual(name = "landcover", values = c(
    "Sealed"="#FF0000",
    "Forest"="#339900",
    "Bush"="#FFCC00",
    "Grass"="#99FF00",
    "Low vegetation"="#999999",
    "Water"="#3399FF")) +
  geom_sf(data = buffers_laea, fill = "#FFFFFF", color = "darkblue") +
  geom_sf(data = Coordinates_Group_laea, aes(color = as.factor(Group)), size = 1) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Oliwa Landscape with Buffers and Coordinates") +
  theme(legend.position = "right")
#######



###### Second part: Calculate different Index around the observation points ######
### Here, we need to connect the two data set together so we need to make sure they are in the same projection

### We need to convert Oliwa_landscape, otherwise the next part of the code will not work:
Oliwa_landscape_sf <- st_as_sf(Oliwa_landscape, coords = c("x", "y"), crs=raster_crs) 

## Create a unique code for each buffer using the group and the point number:
buffers_laea <- buffers_laea %>% mutate(buffer_id = paste(Group,Point, sep =""))
buffers_laea

## We need to clip landscape by each buffer, as for now the information are in separate files
intersection <- st_intersection(buffers_laea,Oliwa_landscape_sf) ## it tells us which data point of landscape falls in our buffer
unique(sort(intersection$buffer_id)) ## We have date for all the buffer

### Reduce the number of land cover categories we have, we only want: Sealed, Vegetation and Water:
head(intersection)
unique(intersection$landcover)
intersection_new <- intersection %>% mutate(Three_Categories = case_when(landcover == "Forest" | landcover == "Grass" | landcover == "Low vegetation" ~ "Vegetation",
                                                                         landcover == "Sealed" ~ "Sealed",
                                                                         landcover == "Water" ~ "Water"))
head(intersection_new)
unique(intersection_new$Three_Categories)

### Calculate the proportion of each habitat inside each the buffer, based on the data we got from intersection:
Percentage_landcover <- intersection_new %>% 
  group_by(buffer_id,Three_Categories) %>% 
  summarise(counts = n()) %>%
  mutate(per= prop.table(counts) * 100) %>%
  ungroup() %>%
  complete(buffer_id,Three_Categories, fill = list(counts = 0 ,per = 0))
Percentage_landcover 

### Extract the information for each categories:
Vegetation_Index <- Percentage_landcover %>% filter(Three_Categories == "Vegetation") %>% dplyr::select(buffer_id,per)
Vegetation_Index
### Change the column name:
colnames(Vegetation_Index) <- c('buffer_id','Vegetation_Index')

Water_Index <- Percentage_landcover %>% filter(Three_Categories == "Water") %>% dplyr::select(buffer_id,per)
Water_Index
### Change the column name:
colnames(Water_Index) <- c('buffer_id','Water_Index')

Sealed_Index <- Percentage_landcover %>% filter(Three_Categories == "Sealed") %>% dplyr::select(buffer_id,per)
Sealed_Index
### Change the column name:
colnames(Sealed_Index) <- c('buffer_id','Sealed_Index')

## Create the common column in my Coordinates data set as well:
Coordinates_Group_bufferID <- Coordinates_Group %>% mutate(buffer_id = paste(Group,Point, sep =""))


## Reorganize to have everything in one table:
FinalTable <- Coordinates_Group_bufferID  %>%
  full_join(Sealed_Index, by = "buffer_id") %>%
  full_join(Vegetation_Index, by = "buffer_id") %>%
  full_join(Water_Index, by = "buffer_id") 
head(FinalTable) 

#setwd("~/Library/CloudStorage/OneDrive-UniversityofGdansk(forStudents)/PhD/Class/Teaching/Marta/Diversity_class")
#write.csv(FinalTable, file="FinalTable.csv",row.names=F)

#######




