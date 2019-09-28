## Script to map satellite field verification sample locations
## KBG Sep-2019

library(tidyverse)
library(ggplot2)
library(sf)
library(ggsn) #scalebar() and north() functions

## Package sf tutorials
# https://datacarpentry.org/r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r/
# https://ryanpeek.github.io/2017-08-03-converting-XY-data-with-sf-package/

# Run script to source the data formating functions
source("Scripts/map_sample_functions.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Function to import XY data and transform to UTM and create pixel boundaries
# Lake San Antonio
lsa_data <- read_and_transform_data(centroids= "Data/LakeMaps/LakeSanAntonio_pixel.csv",
                                samples = "Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt",
                                utm_epsg = 32610, # UTM zone 10
                                buff_dist = 150)
# San Pablo Reservoir
spr_data <- read_and_transform_data(centroids= "Data/LakeMaps/SanPabloReservoir_20190804_pixel.csv",
                                    samples = "Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)



# Extract bounding box of sample locations
lsa_bbox <- st_bbox(lsa_data[["samples"]])
spr_bbox <- st_bbox(spr_data[["samples"]])


# Filter lakes shapefile for the lake of interest to get DFGWATERID value
ca_lakes_utm@data %>% 
  filter(str_detect(ca_lakes_utm@data$GNIS_NAME, "Pablo"))

# Run function to extract specific lake based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884)



#### PLOT THE MAPS ####
# LAKE SAN ANTONIO 
# Whole lake
ggplot() +
  geom_polygon(data= lsa_utm, aes(x= long, y= lat), fill= "skyblue", color= "black", alpha= 0.25) +
  geom_sf(data= lsa_data[["pixels"]], fill= "transparent") +
  geom_sf(data= lsa_data[["centroids"]], size= 0.5, color= "black") +
  geom_sf(data= lsa_data[["samples"]], size= 3, aes(color= pixel, fill= pixel)) +
  coord_sf(crs= 32610) +
  scalebar(data= lsa_utm, dist= 2000, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  labs(x= "Longitude", y= "Latitude") +
  theme_bw()


# Sample bounding box only
ggplot() +
  geom_polygon(data= lsa_utm, aes(x= long, y= lat), fill= "skyblue", color= "black", alpha= 0.25) +
  geom_sf(data= lsa_data[["pixels"]], fill= "transparent") +
  geom_sf(data= lsa_data[["centroids"]], size= 0.5, color= "black") +
  geom_sf(data= lsa_data[["samples"]], size= 1, aes(fill= pixel, color= pixel)) +
  coord_sf(xlim= c(lsa_bbox[1], lsa_bbox[3]), 
           ylim= c(lsa_bbox[2], lsa_bbox[4]),
           crs= 32610) +
  scalebar(data= lsa_data[["samples"]], dist= 150, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  labs(x= "Longitude", y= "Latitude") +
  theme_bw()


# SAN PABLO RESERVOIR

# Whole lake
ggplot() +
  geom_polygon(data= spr_utm, aes(x= long, y= lat), fill= "skyblue", color= "black", alpha= 0.25) +
  geom_sf(data= spr_data[["pixels"]], fill= "transparent") +
  geom_sf(data= spr_data[["centroids"]], size= 0.5, color= "black") +
  geom_sf(data= spr_data[["samples"]], size= 3, aes(color= pixel, fill= pixel)) +
  coord_sf(crs= 32610) +
  scalebar(data= spr_utm, dist= 1000, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  labs(x= "Longitude", y= "Latitude") +
  theme_bw()


# Sample bounding box only
ggplot() +
  geom_polygon(data= spr_utm, aes(x= long, y= lat), fill= "skyblue", color= "black", alpha= 0.25) +
  geom_sf(data= spr_data[["pixels"]], fill= "transparent") +
  geom_sf(data= spr_data[["centroids"]], size= 0.5, color= "black") +
  geom_sf(data= spr_data[["samples"]], size= 3, aes(color= pixel, shape= site)) +
  coord_sf(xlim= c(spr_bbox[1], spr_bbox[3]), 
           ylim= c(spr_bbox[2], spr_bbox[4]),
           crs= 32610) +
  scalebar(data= spr_data[["samples"]], dist= 150, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 3, 
           border.size = 0.5) +
  labs(x= "Longitude", y= "Latitude") +
  theme_bw()




# ggplot() +
#   geom_polygon(data= lsa_df, aes(x= long, y= lat), fill= "skyblue", color= "black", alpha= 0.25) +
#   geom_sf(data= pixels) +
#   geom_sf(data= cntrs_utm, size= 0.5, color= "black") +
#   geom_sf(data= samps_utm, size= 1, aes(fill= pixel, color= pixel)) +
#   #coord_sf(xlim= c(lsa_bbox[1], lsa_bbox[3]), 
#   #       ylim= c(lsa_bbox[2], lsa_bbox[4])) +
#   #scalebar(data= gps_utm, dist= 150, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
#   #         st.dist = 0.02,
#   #         st.size= 3, 
#   #         border.size = 0.5) +
#   theme_bw()
# 
# 
# cntrs_w <- st_as_sf(cntrs, coords= c("Lon", "Lat"), crs= 4326) 
# samps_w <- st_as_sf(samps, coords= c("long", "lat"), crs= 4326) 
# 
# pixels_w <- st_buffer(cntrs_w, dist= 0.001, endCapStyle = "SQUARE")
# pixels_t <- st_buffer(cntrs_t, dist= 150, endCapStyle = "SQUARE")


# 
# cntrs_utm <- st_transform(cntrs_w, crs= 32610)
# pixels_w2 <- st_buffer(cntrs_utm, dist= 150, endCapStyle = "SQUARE")
# 

