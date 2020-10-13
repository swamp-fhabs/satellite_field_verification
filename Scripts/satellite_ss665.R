library(tidyverse)
#library(raster)
library(sf)
library(rgdal)
library(ggplot2)

#multiband raster R
#https://www.neonscience.org/dc-multiband-rasters-r

# Tiff band labels (tags) provided by Shelly Tomlinson of NOAA through email on 2-Jul-2020
tiff.tags <- c("Rrs_400", "Rrs_412", "Rrs_442", "Rrs_490", "Rrs_510", "Rrs_560", "Rrs_620", "Rrs_665", "Rrs_674", "Rrs_681", "Rrs_709", "Rrs_754", "Rrs_761", "Rrs_764", "Rrs_768", "Rrs_779", "Rrs_865", "Rrs_885", "Rrs_900", "Rrs_940", "Rrs_1012", "rhos_400", "rhos_412", "rhos_442", "rhos_490", "rhos_510", "rhos_560", "rhos_620", "rhos_665", "rhos_674", "rhos_681", "rhos_709", "rhos_754", "rhos_761", "rhos_764", "rhos_768", "rhos_779", "rhos_865", "rhos_885", "rhos_900", "rhos_940", "rhos_1012", "chl_oc4", "cloud_albedo", "hab_l2_flags")
tiff.tags[c(28, 29, 31, 32)] # bandds for SS(665) and SS(681)


# create a grayscale color palette to use for the image.
grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent



## Read in raster file
tif <- raster::stack("Data/ss665_data/sentinel-3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.tif",
                     bands= c(28, 29, 31, 32))
names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= "-")
#tif@layers
tif@layers
raster::getValues(tif, 2)

tif620.sp <- as(tif[[1]], "SpatialPixelsDataFrame")
tif620.sf <- st_as_sf(tif620.sp)
## Reproject to lat/long
#newcrs <- CRS("+proj=longlat +datum=WGS84")

#tif.ll <- raster::projectRaster(tif, crs= newcrs)

## Read in CIcyano data
dat <- read_tsv("Data/CI_field_sat.tsv")

dat.sf.ll <- st_as_sf(dat,
         coords= c("Lon", "Lat"),
         crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


dat.sf.utm <- st_transform(dat.sf.ll, crs= "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Convert CIcyano to UTM



## Extract raster points using CIcyano locations
?raster::extract

## Reproject LSA data
lsa_utm

lsa_utm.sf <- st_as_sf(lsa_utm,
                       coords = c("long", "lat"),
                       crs= "+proj=utm +zone=10 +datum=WGS84") %>% 
  st_transform(lsa_utm.sf, crs= "+proj=utm +zone=11 +datum=WGS84")

## Plot rasters
lsa.ci <- raster::raster("Data/20190801_LakeSanAntonio/OLCI_mosaicCyano_20190801_Lake_San_Antonio.tif")

lsa <- filter(dat.sf.utm, waterbody == "LakeSanAntonio_20190801")
lsa.bbox <- st_bbox(lsa)


lsa.extent <- raster::extent(134402.6, 149102.6, 3967704, 3979704)

tifsp.crop <- raster::crop(tif[[1]], y= lsa.extent)
tifsp.crop.df <- raster::as.data.frame(tifsp.crop, xy= TRUE)
head(tifsp.crop.df)

lsa.ci.df<- raster::as.data.frame(lsa.ci, xy= TRUE)

?crop
?extent

bobox <- st_bbox(st_as_sf(filter(dat.sf.utm, waterbody == "LakeSanAntonio_20190801")))

ggplot() +
  geom_raster(data= tifsp.crop.df,
              aes(x= x, y= y, fill= rhos_620.sentinel.3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.28)) +
  scale_fill_continuous(guide= FALSE) +
  geom_polygon(data= lsa_utm, aes(x= long, y= lat, group= id), color= "white", alpha= 0.25) +
  geom_sf(data= lsa, color = "red") +
  geom_tile(data= tifsp.crop.df,
            aes(x= x, y= y), color= "white", fill= NA)


ggplot() +
  geom_raster(data= lsa.ci.df,
              aes(x= x, y= y, fill= OLCI_mosaicCyano_20190801_Lake_San_Antonio)) +
  geom_tile(data= lsa.ci.df, aes(x= x, y= y), fill= NA, color= "white", size= 0.2)+
  scale_fill_continuous(guide= FALSE) 
  geom_polygon(data= lsa_utm.sf, aes(x= long, y= lat, group= id), color= "white", alpha= 0.25)
  geom_sf(data= lsa, color = "red")
  geom_tile(data= tifsp.crop.df,
            aes(x= x, y= y), color= "white", fill= NA)


  ggplot() +
    # geom_raster(data= lsa.ci.df,
    #             aes(x= x, y= y, fill= OLCI_mosaicCyano_20190801_Lake_San_Antonio)) 
  geom_tile(data= lsa.ci.df,
              aes(x= x, y= y), color= "gray50", fill= NA, size= 0.1, alpha= 0) +
    #geom_raster(data= tifsp.crop.df,
    #            aes(x= x, y= y, fill= rhos_620.sentinel.3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.28), alpha= 0.5) 
    #geom_sf(data= lsa_utm.sf, size= 0.25) 
  geom_tile(data= tifsp.crop.df,
              aes(x= x, y= y), fill= NA, color= "white", size= 0.1) 
  
    
  
  #fill= OLCI_mosaicCyano_20190801_Lake_San_Antonio
  
  
## Read in the raster files (both UTM 11)  
  lsa.ci <- raster::raster("Data/20190801_LakeSanAntonio/OLCI_mosaicCyano_20190801_Lake_San_Antonio.tif")
  tif <- raster::stack("Data/ss665_data/sentinel-3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.tif",
                       bands= c(28, 29, 31, 32))
  names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= "-")
  
## Crop the SS665 TIF to Lake San Antonio
  lsa.extent <- raster::extent(134402.6, 149102.6, 3967704, 3979704) # values from the raster header lsa.ci
  
  tifsp.crop <- raster::crop(tif[[1]], y= lsa.extent)
  
## Convert to data frames
  tifsp.crop.df <- raster::as.data.frame(tifsp.crop, xy= TRUE)
  lsa.ci.df<- raster::as.data.frame(lsa.ci, xy= TRUE)
  
## Plot
  
  ggplot() +
    geom_tile(data= lsa.ci.df,
              aes(x= x, y= y), color= "gray50", fill= NA, size= 0.1, alpha= 0) +
    geom_tile(data= tifsp.crop.df,
              aes(x= x, y= y), fill= NA, color= "lightblue", size= 0.1) +
    geom_sf(data= lsa, color = "red") +
    #coord_equal() +
    coord_sf()
  
  
  ggplot() +
    geom_tile(data= lsa.ci.df,
              aes(x= x, y= y, fill= OLCI_mosaicCyano_20190801_Lake_San_Antonio), color= "gray50", size= 0.1) +
    scale_fill_continuous(guide= FALSE) +
    coord_quickmap() +
    coord_equal()
    
  ggplot() +
  geom_tile(data= tifsp.crop.df,
               aes(x= x, y= y, fill= rhos_620.sentinel.3a.2019213.0801.1803C.L3.CAS.v930seadasv7521_1.28), color= "lightblue", size= 0.1) +
  scale_fill_continuous(guide= FALSE) +
    coord_quickmap()
  
    