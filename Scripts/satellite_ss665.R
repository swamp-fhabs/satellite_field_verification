library(tidyverse)
#library(raster)
library(sf)
library(rgdal)

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
tif <- raster::stack("Data/ss665_data/sentinel-3a.2019219.0807.1845_1848C.L3.CAN.v930seadasv7521_1.tif",
                     bands= c(28, 29, 31, 32))
names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= "-")
#tif@layers

raster::getValues(tif, 2)

## Reproject to lat/long
#newcrs <- CRS("+proj=longlat +datum=WGS84")

#tif.ll <- raster::projectRaster(tif, crs= newcrs)

## Read in CIcyano data

## Convert CIcyano to UTM

## Extract raster points using CIcyano locations
?raster::extract


