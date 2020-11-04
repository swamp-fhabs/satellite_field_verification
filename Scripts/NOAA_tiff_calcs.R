library(tidyverse)
source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")



#### RUN FUNCTIONS ####

# Import NOAA tif
#noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)
#saveRDS(noaa.tiff.list, "Data/ss665_data/NOAA_tiffs.RDS")
noaa.tiff.list <- readRDS("Data/ss665_data/NOAA_tiffs.RDS")

# Extract pixels for each sampled water body
noaa.extract.list <- map2(noaa.tiff.list, noaa.field.info$DFGWATERID, function(x, y) extract_lake_pixels(tif.matrix = x, lake.id= y, utm= 10))

# Identify the satellite pixels that were sampled in the field

sampled.pixels.list <-  map2(noaa.extract.list, noaa.field.info$field_path, function(x, y) find_sampling_pixels(pix.center.sf = x, field.lat.long = y, utm= 10))



# Calculate CI values
ci_sat_df <- map(sampled.pixels.list, calc_CI) %>% 
  bind_rows()

## Average across sites that transversed multiple pixels
## I think I need to commit to this averaging, it's better than inflating my df
## Alternatively, I could try to figure out exactly which site replicate occurred in each pixel
## But I think this is no good, because then you couldn't average over the whole site, and you'd be 
## Estimating a whole pixel with a single Radiometer reading
## Unfortunately, you lose the pix_FID column down the road
ci_sat_df2 <- ci_sat_df %>% 
  group_by(waterbody, site) %>% 
  summarise(across("rhos_620":"CIcyano_sat", mean), .groups= "drop")


#### MERGE WITH FIELD DATA

## CALC CI VALUS FROM RADIOMETER RRS DATA
#field_CI_values <- calc_CI_values(in_dir = "Data/rrs_data", out_path = "Data") 
#write_tsv(field_CI_values, "Data/CI_field.tsv")
field_CI_values <- read_tsv("Data/CI_field.tsv")

CI_field_sat <- left_join(ci_sat_df, field_CI_values, by= c("waterbody", "site"))
CI_field_sat2 <- left_join(ci_sat_df2, field_CI_values, by= c("waterbody", "site"))
#write_tsv(CI_field_sat, path= "Data/CI_field_sat_NOAA_tiff.tsv")

ci_fs2 <- CI_field_sat2
# library(ggplot2)
# ggplot(CI_field_sat) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   geom_abline(intercept= 0, slope= 1) +
#   geom_point(aes(x= CI_sat, y= CI_field, color= waterbody), size= 2) +
#   theme_bw()


