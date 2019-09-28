## Script to plot pixel locations of satellite flyovers on 
## sampling days, and identify the pixel number where the 
## samples were collected

## These numbers will then be used to extract the correct pixel data
## from the satellite file for plotting and analyses

## KBG Sep-2019
library(tidyverse)


# Run script to source the data formating functions
source("Scripts/map_functions.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Function to import XY data and transform to UTM and create pixel boundaries
# Lake San Antonio
lsa_data <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeSanAntonio_20190801.csv",
                                samples = "Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt",
                                utm_epsg = 32610, # UTM zone 10
                                buff_dist = 150)


# San Pablo Reservoir
spr_data <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-SanPabloReservoir_20190812.csv",
                                    samples = "Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)


# Extract bounding box of sample locations
lsa_bbox <- st_bbox(lsa_data[["samples"]])
spr_bbox <- st_bbox(spr_data[["samples"]])


# Filter lakes shapefile for the lake of interest to get DFGWATERID value
ca_lakes_utm@data %>% 
  filter(str_detect(ca_lakes_utm@data$GNIS_NAME, "Pablo"))

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884)


#### PLOT THE MAPS ####

# LAKE SAN ANTONIO 
lsa_bbox_map <- plot_bounding_box(data_list = lsa_data, shapefile_utm = lsa_utm, bbox= lsa_bbox, label= TRUE)


# SAN PABLO RESERVOIR
spr_bbox_map <- plot_bounding_box(data_list = spr_data, shapefile_utm = spr_utm, bbox= spr_bbox, label= TRUE)

