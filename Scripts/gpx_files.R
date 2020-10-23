library(tidyverse)


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

#gpx_file "Data/20200708_ClearLake/2020-07-08 14.50.01.gpx"
# "Data/20200708_ClearLake/radiometer_ClearLake_20200708.txt"

test <- gpx_pix_site(gpx_file = "Data/20200708_ClearLake/2020-07-08 14.50.01.gpx",
             pix_site_file = "Data/20200708_ClearLake/radiometer_ClearLake_20200708.txt",
             utm= 10)


# ggplot(gpx_radio_sf) +
#   geom_sf(aes(fil
#l= pix_site), size= 3, shape= 21, color= "gray70") +
#   coord_sf() +
#   theme_bw()

# 
# ## Read in lat long from Aug-7
# cl0807 <- read_tsv('Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt')
# cl0807_sf <- st_as_sf(cl0807, 
#          coords= c("long", "lat"),
#          crs =  "+proj=longlat +datum=WGS84 +no_defs")
# 
# cl0807_sp_utm <- st_transform(cl0807_sf, 
#                               crs= "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# ## Read in the raster files (both UTM 11)  
# tif <- raster::stack("Data/ss665_data/sentinel-3a.2019281.1008.1838_1841C.L3.CAN.v930seadasv7521_1.tif",
#                      bands= c(28, 29, 31, 32))
# names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= "-")
# 
# ## Reproject GPX to UTM 10
# gpx_tp.utm <- st_transform(gpx_tp, crs= "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# 
# ## Crop the SS665 TIF to Clear lake
# cl.extent <- raster::extent(510984.8 - 500, 528084.8 + 500, 4312754 - 500, 4327245 + 500) # values from the raster header lsa.ci
# 
# tif.crop <- raster::crop(tif[[1]], y= cl.extent)
# 
# ## Convert to data frames
# tif.crop.df <- raster::as.data.frame(tif.crop, xy= TRUE)
# 
# 
# ## Plot
# 
# ggplot() +
#   geom_tile(data= tif.crop.df,
#            aes(x= x, y= y, fill= tif.crop.df[, 3]), color= "gray70", size= 0.1) +
#   # geom_raster(data= tif.crop.df,
#   #             aes(x= x, y= y, fill= tif.crop.df[, 3])) +
#   geom_sf(data= gpx_tp.utm, color= "white", size= 0.5) +
#   geom_sf(data= cl0807_sp_utm, color= "red", size= 0.5) +
#   scale_fill_continuous(guide= FALSE) +
#   coord_sf()
#   
# ggsave(last_plot(), filename= "ClearLake_20191008_SS665_GPX.pdf", height= 8, width= 8, units= "in",
#        path= "Data/ss665_data/", device= cairo_pdf)
# 
# gpx_tp.utm


