library(tidyverse)
#library(raster)
library(sf)
library(rgdal)
library(ggplot2)

read_noaa_tiff <- function(tif.file){
  require(stringr)
  #multiband raster R
  #https://www.neonscience.org/dc-multiband-rasters-r
  
  # Tiff band labels (tags) provided by Shelly Tomlinson of NOAA through email on 2-Jul-2020
  tiff.tags <- c("Rrs_400", "Rrs_412", "Rrs_442", "Rrs_490", "Rrs_510", "Rrs_560", "Rrs_620", "Rrs_665", "Rrs_674", "Rrs_681", "Rrs_709", "Rrs_754", "Rrs_761", "Rrs_764", "Rrs_768", "Rrs_779", "Rrs_865", "Rrs_885", "Rrs_900", "Rrs_940", "Rrs_1012", "rhos_400", "rhos_412", "rhos_442", "rhos_490", "rhos_510", "rhos_560", "rhos_620", "rhos_665", "rhos_674", "rhos_681", "rhos_709", "rhos_754", "rhos_761", "rhos_764", "rhos_768", "rhos_779", "rhos_865", "rhos_885", "rhos_900", "rhos_940", "rhos_1012", "chl_oc4", "cloud_albedo", "hab_l2_flags")
  #tiff.tags[c(28, 29, 31, 32)] # bandds for SS(665) and SS(681)

  ## Read in raster file
  tif <- raster::stack(tif.file,
                       bands= c(28, 29, 31, 32))
  #names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= ".")
  names(tif) <- tiff.tags[c(28, 29, 31, 32)]
  
  ## Transform multi-band raster stack into a matrix
  tif.mat <- raster::rasterToPoints(tif)
  return(tif.mat)  
}

tif.mat <- read_noaa_tiff(tif.file= "Data/ss665_data/sentinel-3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.tif")


find_sampling_pixels <- function(tif.matrix, field.lat.long){
  require(tidyverse)
  require(sf)
  
  ## FIELD DATA
  
  ## Read lat long of radiometer sites
  field.sf.utm <- read_tsv(field.lat.long) %>% 
    # Make spatial feature object
    st_as_sf(.,
             coords= c("long", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  ## SATELLITE DATA
  pix.center.sf <- as_tibble(tif.mat) %>% 
    filter(., 
           x > st_bbox(field.sf.utm)[1] - 300, 
           y > st_bbox(field.sf.utm)[2] - 300, 
           x < st_bbox(field.sf.utm)[3] + 300, 
           y < st_bbox(field.sf.utm)[4] + 300) %>% 
    st_as_sf(.,
             coords= c("x", "y"),
             crs= "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
    mutate(pixID= str_pad(as.character(seq(1:nrow(.))), width=2, pad= "0"))
  # Identify pixel boundaries (150 meter square buffers)
  pix.buffer.sf <- st_buffer(pix.center.sf, dist= 150, endCapStyle = "SQUARE")
  
  ## JOIN FIELD AND SATELLITE DATA
  data.combined <- st_join(field.sf.utm, pix.buffer.sf)
  return(data.combined)
  
}

test <- find_sampling_pixels(tif.matrix = tif.mat, 
                     field.lat.long = "Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt",
                     site.id = "LSA_20190801")



library(tidyverse)
library(sf)
utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

lakes <- st_read("Data/LakeMaps/CA_Lakes")
#6342

lake_extract <- filter(lakes, DFGWATERID == 6342) %>% 
  st_transform(., crs= utm.crs)

st_bbox(lake_extract)  
  

lake_extract <- lakes_shapefile[lakes_shapefile$DFGWATERID == DFGWATER_ID, ]
lake_data <- lake_extract@data %>% 
  as_tibble() %>%
  mutate(id= rownames(lake_extract@data))
lake.df <- tidy(lake_extract)
lsa.df.merge <- left_join(lake.df, lake_data)
return(lsa.df.merge)



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

























#multiband raster R
#https://www.neonscience.org/dc-multiband-rasters-r

# Tiff band labels (tags) provided by Shelly Tomlinson of NOAA through email on 2-Jul-2020
tiff.tags <- c("Rrs_400", "Rrs_412", "Rrs_442", "Rrs_490", "Rrs_510", "Rrs_560", "Rrs_620", "Rrs_665", "Rrs_674", "Rrs_681", "Rrs_709", "Rrs_754", "Rrs_761", "Rrs_764", "Rrs_768", "Rrs_779", "Rrs_865", "Rrs_885", "Rrs_900", "Rrs_940", "Rrs_1012", "rhos_400", "rhos_412", "rhos_442", "rhos_490", "rhos_510", "rhos_560", "rhos_620", "rhos_665", "rhos_674", "rhos_681", "rhos_709", "rhos_754", "rhos_761", "rhos_764", "rhos_768", "rhos_779", "rhos_865", "rhos_885", "rhos_900", "rhos_940", "rhos_1012", "chl_oc4", "cloud_albedo", "hab_l2_flags")
tiff.tags[c(28, 29, 31, 32)] # bandds for SS(665) and SS(681)




## Read in raster file
tif <- raster::stack("Data/ss665_data/sentinel-3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.tif",
                     bands= c(28, 29, 31, 32))
tif <- raster::stack("Data/ss665_data/sentinel-3a.2019219.0807.1845_1848C.L3.CAN.v930seadasv7521_1.tif",
                     bands= c(28, 29, 31, 32))
tif
noaa.and.field.files$NOAA.path[2]

#names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= ".")
names(tif) <- tiff.tags[c(28, 29, 31, 32)]
tif620.sp <- as(tif[[1]], "SpatialPixelsDataFrame")

tif.mat <- raster::rasterToPoints(tif)


as_tibble(tif.mat)

## Get lat long of radiometer sites
lat.long.df <- read_tsv("Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt")
#
lat.long.sf <- st_as_sf(lat.long.df,
                      coords= c("long", "lat"),
                      crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


utm.sf <- st_transform(lat.long.sf, crs= "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

tif.df <- filter(as_tibble(tif.mat), 
                        x > st_bbox(utm.sf)[1] - 300, 
                        y > st_bbox(utm.sf)[2] - 300, 
                        x < st_bbox(utm.sf)[3] + 300, 
                        y < st_bbox(utm.sf)[4] + 300)
tif.sf <- st_as_sf(tif.df,
                   coords= c("x", "y"),
                   crs= "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
pixels.sf <- st_buffer(tif.sf, dist= 150, endCapStyle = "SQUARE")


# create a grayscale color palette to use for the image.
grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent




ggplot() +
  #geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
  geom_sf(data= pixels.sf, fill= "transparent") +
  coord_sf() +
  #geom_sf(data= data_list[["centroids"]], size= 0.5, color= "black") +
  #geom_sf(data= data_list[["samples"]], size= 1, aes(fill= pixel, color= pixel)) +
  #coord_sf(xlim= c(bbox[1], bbox[3]), 
  #         ylim= c(bbox[2], bbox[4]),
  #         crs= utm_epsg) +
  #scalebar(data= data_list[["samples"]], dist= scalebar_dist, dist_unit = "m", location= "bottomleft", transform= FALSE, st.bottom= FALSE, 
  #         st.dist = 0.02,
  #         st.size= 3, 
  #         border.size = 0.5) +
  #labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
  theme_bw()
