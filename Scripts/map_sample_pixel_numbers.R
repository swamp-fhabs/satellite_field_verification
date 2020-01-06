## Script to plot pixel locations of satellite flyovers on 
## sampling days, and identify the pixel number where the 
## samples were collected

## These numbers will then be used to extract the correct pixel data
## from the satellite file for plotting and analyses

## Original CIcyano files were sent to KBG by Randy Turner (SFEI) in September-2019. In December 2019 CI, CIcyano, and nonCIcyano CSV files were
## sent by Randy Turner. The pixel IDs did not match the original files from Sep-2019. This script was revised to re-identify the pixel ID numbers where sampling occurred
## The suffix OG, CI and CIcyano. Refer to the original (OG) CIcyano file from Sep-2019 and the new CI and CIcyano CSV files from Dec-2019.

## KBG Sep-2019
library(tidyverse)


# Run script to source the data formating functions
source("Scripts/map_functions.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Function to import XY data and transform to UTM and create pixel boundaries
# Lake San Antonio
# Origina CSV, CIcyano, and CI csvs all same pixels
lsa_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeSanAntonio_20190801.csv",
                                samples = "Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt",
                                utm_epsg = 32610, # UTM zone 10
                                buff_dist = 150)

lsa_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeSanAntonio_20190801.CI.csv",
                                    samples = "Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)

# Clear Lake 20190807
clr0807_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190807.csv",
                                    samples = "Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)
clr0807_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190807.CI.csv",
                                        samples = "Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)
clr0807_data_CIcyano <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190807.CIcyano.csv",
                                        samples = "Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)


# San Pablo Reservoir 20190812
spr_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-SanPabloReservoir_20190812.csv",
                                    samples = "Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)

spr_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-SanPabloReservoir_20190812.CI.csv",
                                           samples = "Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt",
                                           utm_epsg = 32610, # UTM zone 10
                                           buff_dist = 150)
spr_data_CIcyano <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-SanPabloReservoir_20190812.CIcyano.csv",
                                                samples = "Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt",
                                                utm_epsg = 32610, # UTM zone 10
                                                buff_dist = 150)

# Lake Almanor 20190815
alm_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeAlmanor_20190815.csv",
                                    samples = "Data/20190815_LakeAlmanor/LatLong_LakeALmanor_20190815.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)

alm_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeAlmanor_20190815.CI.csv",
                                    samples = "Data/20190815_LakeAlmanor/LatLong_LakeALmanor_20190815.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)

alm_data_CIcyano <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-LakeAlmanor_20190815.CIcyano.csv",
                                    samples = "Data/20190815_LakeAlmanor/LatLong_LakeALmanor_20190815.txt",
                                    utm_epsg = 32610, # UTM zone 10
                                    buff_dist = 150)

# Clear Lake 20190816
clr0816_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.csv",
                                        samples = "Data/20190816_ClearLake/LatLong_ClearLake_20190816.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)

clr0816_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.CI.csv",
                                        samples = "Data/20190816_ClearLake/LatLong_ClearLake_20190816.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)

clr0816_data_CIcyano <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.CIcyano.csv",
                                        samples = "Data/20190816_ClearLake/LatLong_ClearLake_20190816.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)

# Clear Lake 20191008
clr1008_data_OG <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20191008.csv",
                                        samples = "Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)

clr1008_data_CI <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20191008.CI.csv",
                                        samples = "Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt",
                                        utm_epsg = 32610, # UTM zone 10
                                        buff_dist = 150)


clr1008_data_CIcyano <- read_and_transform_data(centroids= "Data/Sentinel_flyover_data/sentinel-ClearLake_20191008.CIcyano.csv",
                                           samples = "Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt",
                                           utm_epsg = 32610, # UTM zone 10
                                           buff_dist = 150)





# Extract bounding box of sample locations
lsa_bbox_OG <- st_bbox(lsa_data_OG[["samples"]])
lsa_bbox_CI <- st_bbox(lsa_data_CI[["samples"]])

spr_bbox_OG <- st_bbox(spr_data_OG[["samples"]])
spr_bbox_CI <- st_bbox(spr_data_CI[["samples"]])
spr_bbox_CIcyano <- st_bbox(spr_data_CIcyano[["samples"]])

clr0807_bbox_OG <- st_bbox(clr0807_data_OG[["samples"]])
clr0807_bbox_CI <- st_bbox(clr0807_data_CI[["samples"]])
clr0807_bbox_CIcyano <- st_bbox(clr0807_data_CIcyano[["samples"]])


clr0816_bbox_OG <- st_bbox(clr0816_data_OG[["samples"]])
clr0816_bbox_CI <- st_bbox(clr0816_data_CI[["samples"]])
clr0816_bbox_CIcyano <- st_bbox(clr0816_data_CIcyano[["samples"]])


alm_bbox_OG <- st_bbox(alm_data_OG[["samples"]])
alm_bbox_CI <- st_bbox(alm_data_CI[["samples"]])
alm_bbox_CIcyano <- st_bbox(alm_data_CIcyano[["samples"]])


clr1008_bbox_OG <- st_bbox(clr1008_data_OG[["samples"]])
clr1008_bbox_CI <- st_bbox(clr1008_data_CI[["samples"]])
clr1008_bbox_CIcyano <- st_bbox(clr1008_data_CIcyano[["samples"]])

# Filter lakes shapefile for the lake of interest to get DFGWATERID value
#ca_lakes_utm@data %>% 
#  filter(str_detect(ca_lakes_utm@data$GNIS_NAME, "Almanor"))

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884)
clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075)
alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116)


#### PLOT THE MAPS ####

# LAKE SAN ANTONIO 
lsa_lake_map_OG <- plot_whole_lake(data_list = lsa_data_OG, shapefile_utm = lsa_utm, scalebar_dist = 1000)
lsa_lake_map_OG

ggsave(last_plot(), filename= "lsa_lake_map.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/20190801_LakeSanAntonio")

lsa_lake_map_CI <- plot_whole_lake(data_list = lsa_data_CI, shapefile_utm = lsa_utm, scalebar_dist = 1000)
lsa_lake_map_CI


lsa_bbox_map <- plot_bounding_box(data_list = lsa_data, shapefile_utm = lsa_utm, bbox= lsa_bbox, label= TRUE)

lsa_bbox_map_OG <- plot_bounding_box(data_list = lsa_data_OG, shapefile_utm = lsa_utm, bbox= lsa_bbox_OG, label= TRUE)
lsa_bbox_map_OG
# ggsave(last_plot(), filename= "lsa_lake_bbox_map.jpg", height= 6, width= 8, units= "in", dpi= 300,
#        path= "Data/20190801_LakeSanAntonio")

lsa_bbox_map_CI <- plot_bounding_box(data_list = lsa_data_CI, shapefile_utm = lsa_utm, bbox= lsa_bbox_CI, label= TRUE)
lsa_bbox_map_CI

# CLEAR LAKE 20190807
clr0807_lake_map <- plot_whole_lake(data_list = clr0807_data, shapefile_utm = clr_utm, scalebar_dist = 1000)
ggsave(last_plot(), filename= "clr0807_lake_map.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/20190807_ClearLake")

clr0807_bbox_map_OG <- plot_bounding_box(data_list = clr0807_data_OG, shapefile_utm = clr_utm, bbox= clr0807_bbox_OG, label= TRUE)
clr0807_bbox_map_OG

clr0807_bbox_map_CI <- plot_bounding_box(data_list = clr0807_data_CI, shapefile_utm = clr_utm, bbox= clr0807_bbox_CI, label= TRUE)
clr0807_bbox_map_CI

clr0807_bbox_map_CIcyano <- plot_bounding_box(data_list = clr0807_data_CIcyano, shapefile_utm = clr_utm, bbox= clr0807_bbox_CIcyano, label= TRUE)
clr0807_bbox_map_CIcyano

# SAN PABLO RESERVOIR
spr_lake_map_OG <- plot_whole_lake(data_list = spr_data_OG, shapefile_utm = spr_utm, scalebar_dist = 1000)
spr_lake_map_OG

spr_lake_map_CI <- plot_whole_lake(data_list = spr_data_CI, shapefile_utm = spr_utm, scalebar_dist = 1000)
spr_lake_map_CI

spr_bbox_map_OG <- plot_bounding_box(data_list = spr_data_OG, shapefile_utm = spr_utm, bbox= spr_bbox_OG, label= TRUE)
spr_bbox_map_OG

spr_bbox_map_CI <- plot_bounding_box(data_list = spr_data_CI, shapefile_utm = spr_utm, bbox= spr_bbox_CI, label= TRUE)
spr_bbox_map_CI

spr_bbox_map_CIcyano <- plot_bounding_box(data_list = spr_data_CIcyano, shapefile_utm = spr_utm, bbox= spr_bbox_CIcyano, label= TRUE)
spr_bbox_map_CIcyano



# CLEAR LAKE 20190816
clr0816_lake_map <- plot_whole_lake(data_list = clr0816_data, shapefile_utm = clr_utm, scalebar_dist = 1000)
clr0816_lake_map

clr0816_bbox_map_OG <- plot_bounding_box(data_list = clr0816_data_OG, shapefile_utm = clr_utm, bbox= clr0816_bbox_OG, label= TRUE)
clr0816_bbox_map_OG

clr0816_bbox_map_CI <- plot_bounding_box(data_list = clr0816_data_CI, shapefile_utm = clr_utm, bbox= clr0816_bbox_CI, label= TRUE)
clr0816_bbox_map_CI

clr0816_bbox_map_CIcyano <- plot_bounding_box(data_list = clr0816_data_CIcyano, shapefile_utm = clr_utm, bbox= clr0816_bbox_CIcyano, label= TRUE)
clr0816_bbox_map_CIcyano


# LAKE ALMANOR 20190815
alm_lake_map_OG <- plot_whole_lake(data_list = alm_data_OG, shapefile_utm = alm_utm, scalebar_dist = 1000)
alm_lake_map_OG

alm_lake_map_CI <- plot_whole_lake(data_list = alm_data_CI, shapefile_utm = alm_utm, scalebar_dist = 1000)
alm_lake_map_CI

alm_bbox_map_OG <- plot_bounding_box(data_list = alm_data_OG, shapefile_utm = alm_utm, bbox= alm_bbox_OG, label= TRUE)
alm_bbox_map_OG

alm_bbox_map_CI <- plot_bounding_box(data_list = alm_data_CI, shapefile_utm = alm_utm, bbox= alm_bbox_CI, label= TRUE)
alm_bbox_map_CI

alm_bbox_map_CIcyano <- plot_bounding_box(data_list = alm_data_CIcyano, shapefile_utm = alm_utm, bbox= alm_bbox_CIcyano, label= TRUE)
alm_bbox_map_CIcyano

# CLEAR LAKE 20191008
clr1008_lake_map <- plot_whole_lake(data_list = clr1008_data, shapefile_utm = clr_utm, scalebar_dist = 1000)
clr1008_lake_map

clr1008_bbox_map_OG <- plot_bounding_box(data_list = clr1008_data_OG, shapefile_utm = clr_utm, bbox= clr1008_bbox_OG, label= TRUE)
clr1008_bbox_map_OG

clr1008_bbox_map_CI <- plot_bounding_box(data_list = clr1008_data_CI, shapefile_utm = clr_utm, bbox= clr1008_bbox_CI, label= TRUE)
clr1008_bbox_map_CI

clr1008_bbox_map_CIcyano <- plot_bounding_box(data_list = clr1008_data_CIcyano, shapefile_utm = clr_utm, bbox= clr1008_bbox_CIcyano, label= TRUE)
clr1008_bbox_map_Cicyano


