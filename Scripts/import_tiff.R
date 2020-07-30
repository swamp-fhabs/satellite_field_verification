

import_tiff <- function(tiff){
  library(tidyverse)
  ## https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/
  bloom <- raster::raster(tiff)
  
  
  # Reproject to lat long
  crs.ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  bloom.ll <- raster::projectRaster(bloom, crs= crs.ll, method= "ngb")

  # Make data frame
  bloom.df <- raster::as.data.frame(bloom.ll, xy = TRUE) %>%
    as_tibble() %>% 
    rename(CIcyano_PixVal= starts_with("OLCI"))  %>% 
    mutate(CIcyano_PixVal= ifelse(CIcyano_PixVal > 250, NA, CIcyano_PixVal)) %>% 
    filter(complete.cases(.))
}


CL_tiff <- "Data/20200708_ClearLake/OLCI_mosaicCyano_20200708_Clear_Lake.tif"

cl_data <- import_tiff(CL_tiff)
