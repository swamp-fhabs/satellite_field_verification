
library(tidyverse)
source("Scripts/NOAA_tiff_functions.R")



## NOAA files
noaa.tiffs <- list.files("Data/ss665_data", pattern= "7521_1.tif$|20193_1.tif$") %>% 
  str_c("Data/ss665_data/", ., sep="")

noaa.df <- str_split(noaa.tiffs, pattern= "\\.", simplify= TRUE)[1:7]
names(noaa.df) <- c("Satellite", "Yr_JulianDay", "MoDay", "Unknown", "Level", "Unk", "Algorithm?")


## NOAA field df
## 4 columns: Lat/long text file, noaa.tiff object, waterbody ID from shapefile, UTM
noaa.field.info <- as_tibble(rbind(c("Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt", noaa.tiffs[3], 6342, 10, "LakeSanAntonio_20190801"),
                                   c("Data/20190807_ClearLake/LatLong_ClearLake_20190807.txt", noaa.tiffs[4], 2075, 10, "ClearLake_20190807"),
                                   c("Data/20190812_SanPabloReservoir/LatLong_SanPabloReservoir.txt", noaa.tiffs[6], 3884, 10, "SanPabloReservoir_20190812"),
                                   c("Data/20190815_LakeAlmanor/LatLong_LakeAlmanor_20190815.txt", noaa.tiffs[8], 1116, 10, "LakeAlmanor_20190815"),
                                   c("Data/20190816_ClearLake/LatLong_ClearLake_20190816.txt", noaa.tiffs[9], 2075, 10, "ClearLake_20190816"),
                                   c("Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt", noaa.tiffs[12], 2075, 10, "ClearLake_20191008"),
                                   c("Data/20200708_ClearLake/LatLong_ClearLake_20200708_2.txt", noaa.tiffs[13], 2075, 10, "ClearLake_20200708"),
                                   c("Data/20200724_ClearLake/GPS_avenza_map.txt", noaa.tiffs[14], 2075, 10, "ClearLake_20200724")),
                             .name_repair = "minimal")#,
# c("Data/20200724_ClearLake/LatLong_ClearLake_20200724.txt", noaa.tiffs[14], 2075, 10, "ClearLake_20200724")))
names(noaa.field.info) <- c("field.path", "NOAA.path", "DFGWATERID", "utm", "waterbody")



# Field sampling locations
lsa_20190801_locations <- sampling_locations(noaa.field.info$field.path[1], utm= 10)
clr_20190807_locations <- sampling_locations(noaa.field.info$field.path[2], utm= 10)
spr_20190812_locations <- sampling_locations(noaa.field.info$field.path[3], utm= 10)
alm_20190815_locations <- sampling_locations(noaa.field.info$field.path[4], utm= 10)
clr_20190816_locations <- sampling_locations(noaa.field.info$field.path[5], utm= 10)
clr_20191008_locations <- sampling_locations(noaa.field.info$field.path[6], utm= 10)
clr_20200708_locations <- gpx_pix_site(gpx_file = "Data/20200708_ClearLake/2020-07-08 14.50.01.gpx",
                                        pix_site_file = "Data/20200708_ClearLake/radiometer_ClearLake_20200708.txt",
                                        utm= 10) %>% 
  # Need to transfrom from UTM to Lat/Long and create Lat & Long columns
  st_transform(clr_20200708_locations, crs= 4326) %>% # WGS84 lat/long
  mutate(long= round(st_coordinates(.)[, 1], 4),
         lat= round(st_coordinates(.)[, 2], 4),
         waterbody= "ClearLake_20200708",
         date= as.character(as.Date(date_time_PST)))
#write_tsv(clr_20200708_locations, "Data/20200708_ClearLake/LatLong_ClearLake_20200708_2.txt")


clr_20200724_locations <- read_table2("Data/20200724_ClearLake/GPS_avenza_map.txt") %>% 
  st_as_sf(., coords= c("long", "lat"),
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  # Transform to from Lat/Long to UTM
  st_transform(., crs= "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")





