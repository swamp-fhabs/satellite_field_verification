library(tidyverse)


#### FUNCTIONS ####

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

extract_lake_map_pixels <- function(tif.matrix, lake.id, utm){
  require(sf)
  require(tidyverse)
  
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  # Read CA lakes shapefile
  lake.extract <- st_read("Data/LakeMaps/CA_Lakes") %>% 
    # extract lake of interest
    filter(., DFGWATERID == lake.id) %>% 
    # Transform to utm  
    st_transform(., crs= utm.crs)
  
  
  
  ## EXTRACT LAKE FROM NOAA TIF MATRIX
  pix.center.sf <- as_tibble(tif.matrix) %>% 
    filter(., 
           x > st_bbox(lake.extract)[1] + 300, 
           y > st_bbox(lake.extract)[2] + 300, 
           x < st_bbox(lake.extract)[3] - 300, 
           y < st_bbox(lake.extract)[4] - 300) %>% 
    st_as_sf(.,
             coords= c("x", "y"),
             crs= utm.crs) %>% 
    mutate(pix_FID= str_pad(as.character(seq(0:(nrow(.)-1))), width=2, pad= "0"))
  
  pix.border.sf <- st_buffer(pix.center.sf, dist= 150, endCapStyle = "SQUARE")
  
  
  return(list(center= pix.center.sf, border= pix.border.sf))
}


###########
####POINT IN POLY FOR SF CLASS #######
lsa_utm_sf <- st_as_sf(lsa_utm, coords= c("long", "lat"), crs= 32610)
points <- noaa.extract.map.list[1][[1]][["center"]]

st_sample(points, lsa_utm_sf)
#####################

sampling_locations <- function(field.lat.long, utm){
  require(tidyverse)
  require(sf)
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  
  ## FIELD DATA
  
  ## Read lat long of radiometer sites
  field.sf.utm <- read_tsv(field.lat.long[1]) %>% 
    # Make spatial feature object
    st_as_sf(.,
             coords= c("long", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= utm.crs)
}

gpx_pix_site <- function(gpx_file, pix_site_file, utm= 10){
  require(tidyverse)
  require(tmaptools)
  require(sf)
  require(lubridate)
  
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  
  ## GPX time stamp at prime meridian UTC
  ## California is -8 UTC in winter and -7 UTC in summer during daylight savings time
  
  
  ## Read in GPX
  gpx_sf <- read_GPX(gpx_file)$track_points %>% 
    select(time) %>% 
    mutate(date_time_PST= with_tz(ymd_hms(str_replace(time, "\\+00", "")), tzone= "America/Los_Angeles")) %>% 
    mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins"))
  
  
  
  
  ## Transform into a tibble and format columns
  # use do.call to extract the lat/long geometry from class sf
  # gpx_df <- tibble(date_time_UTC= as.character(gpx_tp$time), 
  #                  lon= do.call(rbind, sf::st_geometry(gpx_tp$geometry))[, 1],
  #                  lat= do.call(rbind, sf::st_geometry(gpx_tp$geometry))[, 2]) %>% 
  #   mutate(date_time_PST= with_tz(ymd_hms(str_replace(.$date_time_UTC, "\\+00", "")), tzone= "America/Los_Angeles")) %>% 
  #   mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins"))
  
  radio <- read_tsv(pix_site_file) %>% 
    mutate(date_time_PST= ymd_hms(str_c("2020-07-08", time, sep=" "), tz= "America/Los_Angeles")) %>% 
    mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins")) %>% 
    rename(pix_site= pixel)
  
  
  gpx_radio_sf <- left_join(gpx_df, select(radio, pix_site, date_time_PST)) %>% 
    filter(!is.na(pix_site)) %>% 
    st_as_sf(.,
             coords= c("lon", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= utm.crs)
  
  return(gpx_radio_sf)
}


plot_lake <-  function(data_list, field_df, shapefile_utm, scalebar_dist= 150, utm_epsg= 32610){
  require(ggplot2)
  require(sf)
  
  bbox <- st_bbox(data_list[[1]][["border"]])
  
  bbox_plot <- ggplot() +
    geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
    geom_sf(data= data_list[[1]][["border"]], fill= "transparent") +
    #geom_sf(data= data_list[[1]][["center"]], size= 0.5, color= "black") +
    geom_sf_text(data= data_list[[1]][["center"]], aes(label= pix_FID), size= 3, color= "black") +
    geom_sf(data= field_df, size= 4, aes(fill= pix_site, color= pix_site)) +
    coord_sf(xlim= c(bbox[1]-300, bbox[3] + 300), 
             ylim= c(bbox[2]- 300, bbox[4] + 300),
             crs= utm_epsg) +
    labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
    theme_bw()
  
  return(bbox_plot)
}

#### FORMAT FILE PATHS #####

## NOAA files
noaa.tiffs <- list.files("Data/ss665_data", pattern= "7521_1.tif$") %>% 
  str_c("Data/ss665_data/", ., sep="")

noaa.df <- str_split(noaa.tiffs, pattern= "\\.", simplify= TRUE)[1:7]
names(noaa.df) <- c("Satellite", "Yr_JulianDay", "MoDay", "Unknown", "Level", "Unk", "Algorithm?")


## NOAA field df
## 4 columns: Lat/long text file, noaa.tiff object, waterbody ID from shapefile, UTM
noaa.field.info <- as_tibble(rbind(c("Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt", noaa.tiffs[3], 6342, 10),
                                   c("Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt", noaa.tiffs[4], 2075, 10),
                                   c("Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt", noaa.tiffs[6], 3884, 10),
                                   c("Data/20190815_LakeAlmanor/LatLong_LakeAlmanor_20190815.txt", noaa.tiffs[8], 1116, 10),
                                   c("Data/20190816_ClearLake/LatLong_ClearLake_20190816.txt", noaa.tiffs[9], 2075, 10),
                                   c("Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt", noaa.tiffs[12], 2075, 10)))
names(noaa.field.info) <- c("field.path", "NOAA.path", "DFGWATERID", "utm")



######################################################

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884)
clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075)
alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116)


# Import NOAA tif
noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)

# Extract pixels for each sampled water body
noaa.extract.map.list <- map2(noaa.tiff.list, noaa.field.info$DFGWATERID, function(x, y) extract_lake_map_pixels(tif.matrix = x, lake.id= y, utm= 10))



#### MAKE PLOTS ####


# Lake San Antonio
plot_lake(data_list= noaa.extract.map.list[1],
                  field_df= lsa_20190801_locations,
                  shapefile_utm= lsa_utm,
                  )
#ggsave(last_plot(), filename= "map_sampling_lsa20190801.png", width= 8, height= 6, units= "in", dpi= 300,
#       path= "Data/LakeMaps")



