## Functions to make maps of the satellite pixels locations on waterbodies in California
## Uses the sf package in R

## Package sf tutorials
# https://datacarpentry.org/r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r/
# https://ryanpeek.github.io/2017-08-03-converting-XY-data-with-sf-package/


## Function to read in lat/long data from satellite field verificaiton
## and output spatial data sets

## centroids= csv file of the lat/longs of the center of each pixel
## sample= tsv file of the lat/longs of each sample location
## utm_epsg= the UTM project to transform the geodata base
## buff_dist= half the width of the pixel (in units of UTM)

read_and_transform_data <- function(centroids, samples, utm_epsg, buff_dist= 150){
  require(tidyverse)
  require(sf)
  require(rgdal)
  
  
  ## Read in lat/long files
  cntrs_xy <- read_csv(centroids) %>% 
    mutate(pixel_num= seq(1, length(Lon)))
  samps_xy <- read_tsv(samples) 
  
  ## Convert to spatial data set with defined CRS
  cntrs_wgs84 <- st_as_sf(cntrs_xy, coords= c("Lon", "Lat"), crs= 4326) 
  cntrs_utm <- st_transform(cntrs_wgs84, crs= utm_epsg)
  
  samps_wgs84 <- st_as_sf(samps_xy, coords= c("long", "lat"), crs= 4326) 
  samps_utm <- st_transform(samps_wgs84, crs= utm_epsg)
  
  
  # Get bounding box of sampling sites
  #bbox <- st_bbox(samps_utm)
  
  ## Create square buffer around sample to identify size of pixels
  # st_buffer helpage: https://r-spatial.github.io/sf/reference/geos_unary.html
  pixels_utm <- st_buffer(cntrs_utm, dist= buff_dist, endCapStyle = "SQUARE")
  
  
  function_output <- list(cntrs_utm, pixels_utm, samps_utm)
  names(function_output) <- c("centroids", "pixels", "samples")
  return(function_output)
}



## Function to read in lakes shapefile and save as an .RDS object
read_lakes_shapefiles <- function(){
  require(tidyverse)
  require(rgdal)
  
  ## Read in lakes shapefiles
  lakes <- readOGR(dsn= "../LakeMaps/CA_Lakes", layer= "CA_Lakes")
  #lakes.wgs84<- spTransform(lakes, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
  lakes.utm <- spTransform(lakes, CRS("+proj=utm +zone=10 +datum=WGS84")) # reproject to match ggmap
  
  #lakes.wgs84 <- lakes.wgs84[!is.na(lakes.wgs84$NAME), ]
  lakes.utm <- lakes.utm[!is.na(lakes.utm$NAME), ]
  
  return(lakes.utm)
}
#ca_lakes_shapefile_utm <- read_lakes_shapefiles()
#saveRDS(ca_lakes_shapefile_utm, "Data/ca_lakes_shapefile_utm.rds")
#saveRDS(lakes.wgs84, "Data/ca_lakes_shapefile_wgs84.rds")


extract_lake_shapefile <- function(lakes_shapefile, DFGWATER_ID){
  require(broom)
  lake_extract <- lakes_shapefile[lakes_shapefile$DFGWATERID == DFGWATER_ID, ]
  lake_data <- lake_extract@data %>% 
    as_tibble() %>%
    mutate(id= rownames(lake_extract@data))
  lake.df <- tidy(lake_extract)
  lsa.df.merge <- left_join(lake.df, lake_data)
  return(lsa.df.merge)
}


#### PLOT MAPS ####
plot_whole_lake <- function(data_list, shapefile_utm, scalebar_dist, utm_epsg= 32610, label= FALSE){
  require(ggplot2)
  require(sf)
  require(ggsn) #scalebar() and north() functions
  
  if(label== FALSE){
    whole_lake_plot <-  ggplot() +
      geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
      geom_sf(data= data_list[["pixels"]], fill= "transparent") +
      geom_sf(data= data_list[["centroids"]], size= 0.5, color= "black") +
      geom_sf(data= data_list[["samples"]], size= 3, aes(color= pixel, fill= pixel)) +
      coord_sf(crs= utm_epsg) +
      scalebar(data= shapefile_utm, dist= scalebar_dist, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
               st.dist = 0.02,
               st.size= 3, 
               border.size = 0.5) +
      labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
      theme_bw()
  }
  
  if(label==TRUE){
    whole_lake_plot <-  ggplot() +
      geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
      geom_sf(data= data_list[["pixels"]], fill= "transparent") +
      #geom_sf(data= data_list[["centroids"]], size= 0.5, color= "black") +
      geom_sf_text(data= data_list[["centroids"]], aes(label= pixel_num), color= "black", alpha= 0.5) +
      geom_sf(data= data_list[["samples"]], size= 3, aes(color= pixel, fill= pixel)) +
      coord_sf(crs= utm_epsg) +
      scalebar(data= shapefile_utm, dist= scalebar_dist, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
               st.dist = 0.02,
               st.size= 3, 
               border.size = 0.5) +
      labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
      theme_bw()
  }
  return(whole_lake_plot)
}

plot_bounding_box <-  function(data_list, shapefile_utm, bbox, scalebar_dist= 150, utm_epsg= 32610, label= FALSE){
  require(ggplot2)
  require(sf)
  require(ggsn) #scalebar() and north() functions

  if(label== FALSE){
  bbox_plot <- ggplot() +
    geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
    geom_sf(data= data_list[["pixels"]], fill= "transparent") +
    geom_sf(data= data_list[["centroids"]], size= 0.5, color= "black") +
    geom_sf(data= data_list[["samples"]], size= 1, aes(fill= pixel, color= pixel)) +
    coord_sf(xlim= c(bbox[1], bbox[3]), 
             ylim= c(bbox[2], bbox[4]),
             crs= utm_epsg) +
    scalebar(data= data_list[["samples"]], dist= scalebar_dist, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
             st.dist = 0.02,
             st.size= 3, 
             border.size = 0.5) +
    labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
    theme_bw()
  }
  
  if(label== TRUE){
    bbox_plot <-  ggplot() +
      geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
      geom_sf(data= data_list[["pixels"]], fill= "transparent") +
      #geom_sf(data= data_list[["centroids"]], size= 0.5, color= "black") +
      geom_sf_text(data= data_list[["centroids"]], aes(label= pixel_num), color= "black", alpha= 0.5) +
      geom_sf(data= data_list[["samples"]], size= 1, aes(fill= pixel, color= pixel)) +
      coord_sf(xlim= c(bbox[1], bbox[3]), 
               ylim= c(bbox[2], bbox[4]),
               crs= utm_epsg) +
      scalebar(data= data_list[["samples"]], dist= scalebar_dist, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
               st.dist = 0.02,
               st.size= 3, 
               border.size = 0.5) +
      labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
      theme_bw()
  }
  return(bbox_plot)
}









