library(tidyverse)
source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")


mask <- raster::raster(("Data/ss665_data/sentinel-sentinel-3a.2020206.0724.1822C.L3.CAN.v950V20193_1_2.CI.tiff"))
utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


mask.mat <-raster::rasterToPoints(mask, fun=function(x){x > 250}) # import only masked pixels with values >250
colnames(mask.mat)[3] <-  "pix_val"

table(mask.sf$pix_val)
mask.sf <- as_tibble(mask.mat) %>% 
    st_as_sf(.,
           coords= c("x", "y"),
           crs= utm.crs)
clr_utm

st_bbox(clr_utm)

mask.sf.f <- st_crop(mask.sf, )

clr <- filter(pix.df, waterbody == "ClearLake_20200724")  %>% st_as_sf()

table(mask.sf.f$pix_val)
filter(pix.join, is.na(pix_val))

ggplot() +
  geom_sf(data= mask.sf.f, aes(color= as.factor(pix_val)), size= 3) +
  scale_color_manual(values= c("black", "red")) +
  #geom_sf(data= clr, color= "white", alpha= 0.5) +
theme_bw()

