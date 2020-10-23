
library(tidyverse)

#### FUNCTIONS #############################################################

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

extract_lake_pixels <- function(tif.matrix, lake.id, utm){
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
           x > st_bbox(lake.extract)[1] - 300, 
           y > st_bbox(lake.extract)[2] - 300, 
           x < st_bbox(lake.extract)[3] + 300, 
           y < st_bbox(lake.extract)[4] + 300) %>% 
    st_as_sf(.,
             coords= c("x", "y"),
             crs= utm.crs) %>% 
    mutate(pix_FID= str_pad(as.character(seq(0:(nrow(.)-1))), width=2, pad= "0"))
  
  return(pix.center.sf)
}

find_sampling_pixels <- function(pix.center.sf, field.lat.long, utm){
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
  
  
  ## SATELLITE DATA
  # pix.center.sf <- as_tibble(tif.matrix) %>% 
  #   filter(., 
  #          x > st_bbox(field.sf.utm)[1] - 300, 
  #          y > st_bbox(field.sf.utm)[2] - 300, 
  #          x < st_bbox(field.sf.utm)[3] + 300, 
  #          y < st_bbox(field.sf.utm)[4] + 300) %>% 
  #   st_as_sf(.,
  #            coords= c("x", "y"),
  #            crs= utm.crs) %>% 
  #   mutate(pixID= str_pad(as.character(seq(1:nrow(.))), width=2, pad= "0"))
  # Identify pixel boundaries (150 meter square buffers)
  pix.buffer.sf <- st_buffer(pix.center.sf, dist= 150, endCapStyle = "SQUARE")
  
  ## JOIN FIELD AND SATELLITE DATA
  data.combined <- st_join(field.sf.utm, pix.buffer.sf)
  
  
  ## Extract distinct rows to identify the unique satellite pixels that were sampled
  data.combined.distinct <- as_tibble(data.combined) %>% 
    select(date, waterbody, pix_site, pix_FID, starts_with("rhos")) %>% 
    rename(site= pix_site) %>% 
    distinct() %>% 
    arrange(site)
  
  return(data.combined.distinct)
  
}

calc_CI <- function(df){
  
  # Wynne et al. 2008 equation #1
  CI_sat <-  with(df,
              -1*(rhos_681 - rhos_665 - (rhos_709 - rhos_665) * ((681-665) / (709-665)))
  )
  
  # From Lunetta et al. 2015
  ss665_sat <-  with(df,
                 rhos_665 - rhos_620 + (rhos_620 - rhos_681) * ((665-620) / (681-620)) # I prefer this equation because it uses a + like in Wynne et al. 2008
                 #rhos_665 - rhos_620 - (rhos_681 - rhos_620) * ((665-620) / (681-620))
  )
  
  # From Lunetta et al. 2015
  CIcyano_sat <- ifelse(ss665_sat > 0 & CI_sat > 0, CI_sat, 0)
  
  ## Add parameters to data frame
  df <- mutate(df,
               ss665_sat= ss665_sat,
               CI_sat= CI_sat,
               CIcyano_sat= CIcyano_sat)
    
  
  ## The variations on the formula below generate the result as the ss665 equation above
  ## Across the publications it is common to change the sign of the second operator between + and -
  ## and then reverse the order of the bands in the parenthese. If you do this, the result is the same.
  # ss665_2 <-  with(df.wide,
  #                  rhos_665 - b07_620 - (rhos_681 - b07_620) * (665-620) / (681-620)
  # )
  
  
  return(df)
}


############################################################################


## NOAA files
noaa.tiffs <- list.files("Data/ss665_data", pattern= "7521_1.tif$") %>% 
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
                                        c("Data/20191008_ClearLake/LatLong_ClearLake_20191008.txt", noaa.tiffs[12], 2075, 10, "ClearLake_20191008")))
names(noaa.field.info) <- c("field.path", "NOAA.path", "DFGWATERID", "utm", "waterbody")

#### RUN FUNCTIONS ####

# Import NOAA tif
noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)

# Extract pixels for each sampled water body
noaa.extract.list <- map2(noaa.tiff.list, noaa.field.info$DFGWATERID, function(x, y) extract_lake_pixels(tif.matrix = x, lake.id= y, utm= 10))

# Identify the satellite pixels that were sampled in the field
sampled.pixels.list <- map2(noaa.extract.list, noaa.field.info$field.path, function(x, y) find_sampling_pixels(pix.center.sf = x, field.lat.long = y, utm= 10))

# Calculate CI values
ci_sat_df <- map(sampled.pixels.list, calc_CI) %>% 
  bind_rows()

#### MERGE WITH FIELD DATA
CI_field_sat <- left_join(ci_sat_df, field_CI_values, by= c("waterbody", "site"))
#write_tsv(CI_field_sat, path= "Data/CI_field_sat_NOAA_tiff.tsv")


# library(ggplot2)
# ggplot(CI_field_sat) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   geom_abline(intercept= 0, slope= 1) +
#   geom_point(aes(x= CI_sat, y= CI_field, color= waterbody), size= 2) +
#   theme_bw()


