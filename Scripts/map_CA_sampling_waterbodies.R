## Script to show satellite field verification waterbodies on 
## a map of California
## KBG Feb-2020

library(tidyverse)
library(broom)
library(sf)
library(ggplot2)
library(ggsn) #scalebar() and north() functions


# Run script to source the data formating functions
source("Scripts/map_functions.R")

# Open UTM lakes shapefile R object
#extract_satellite_waterbodies <- function(surface_acres){
  require(broom)
  ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")
  
  ca_lakes_extract <- ca_lakes_utm[ca_lakes_utm$sfc_acres > surface_acres, ]
  ca_lakes_data <- ca_lakes_extract@data %>% 
    as_tibble() %>% 
    mutate(id= rownames(ca_lakes_extract@data))
  
  ca_lakes_extract.df <- tidy(ca_lakes_extract)
  ca_lakes_extract_merged <- left_join(ca_lakes_extract.df, ca_lakes_data)
  rm(ca_lakes_utm)
  return(ca_lakes_extract_merged)
}
#CA_lakes <- extract_satellite_waterbodies(surface_acres = 384)

## Read in California state boundary
read_CA_boundary <- function(file_path, shape_layer){
  require(tidyverse)
  require(rgdal)
  require(broom)
  
  ## Read in CA boundary shapefiles
  CA_boundary <- readOGR(dsn= file_path, layer= shape_layer)
  CA_boundary.utm <- spTransform(CA_boundary, CRS("+proj=utm +zone=10 +datum=WGS84")) # reproject to match ggmap
  CA_tidy <- tidy(CA_boundary.utm)
  return(CA_tidy)
}
CA_bound.utm <- read_CA_boundary(file_path= "Data/Shapefiles/CA_State", shape_layer= "CA_State_TIGER2016")


## Get waterbody locations
## Read in lat/long files
samps_xy <- read_tsv("Data/map_waterbody_labels.tsv")

## Convert to spatial data set with defined CRS
samps_wgs84 <- st_as_sf(samps_xy, coords= c("long", "lat"), crs= 4326) 
samps_utm <- st_transform(samps_wgs84, crs= 32610) # UTM zone 10)

# Run function to extract specific lake shapefile based on DFGWATERID field
# lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342)
# spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884)
# clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075)
# alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116)



## Make Plot

CA_plot <- ggplot() +
  #geom_polygon(data= CA_lakes, aes(x= long, y= lat, group= group), color= "transparent", fill= "lightblue") +
  geom_polygon(data= CA_bound.utm, aes(x= long, y= lat, group= group), fill= "gray20", alpha= 0.2, size= 0.25) +
  # geom_polygon(data= lsa_utm, aes(x= long, y= lat, group= group), fill= "blue") +
  # geom_polygon(data= spr_utm, aes(x= long, y= lat, group= group), fill= "blue") +
  # geom_polygon(data= clr_utm, aes(x= long, y= lat, group= group), fill= "blue") +
  # geom_polygon(data= alm_utm, aes(x= long, y= lat, group= group), fill= "blue") +
  geom_sf(data= samps_utm, size= 1) +
  geom_sf_label(data= samps_utm, aes(label= waterbody), size= 3.5, nudge_y= 50000) +
  coord_sf(crs= 32610) +
  scalebar(data= CA_bound.utm, dist= 100, dist_unit = "km", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
           st.dist = 0.02,
           st.size= 2.5, 
           border.size = 0.5) +
  labs(x= "Longitude", y= "Latitude") +
  theme_bw()

CA_plot

ggsave(CA_plot, filename= "Map_waterbodies.png", width= 4, height= 4, units= "in", dpi= 320,
       path= "Data/Figures_output")




