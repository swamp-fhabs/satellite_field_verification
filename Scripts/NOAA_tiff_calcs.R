library(tidyverse)
source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")



#### RUN FUNCTIONS ####

# Import NOAA tif
#noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)
#saveRDS(noaa.tiff.list, "Data/ss665_data/NOAA_tiffs.RDS")
noaa.tiff.list <- readRDS("Data/ss665_data/NOAA_tiffs.RDS")

# Extract pixels for each sampled water body
noaa.extract.list <- map2(noaa.tiff.list[-8], noaa.field.info$DFGWATERID, function(x, y) extract_lake_pixels(tif.matrix = x, lake.id= y, utm= 10))

# Identify the satellite pixels that were sampled in the field
sampled.pixels.list <- map2(noaa.extract.list[-8], noaa.field.info$field.path, function(x, y) find_sampling_pixels(pix.center.sf = x, field.lat.long = y, utm= 10))

clr07 <- sampled.pixels.list[[7]]

# Calculate CI values
ci_sat_df <- map(sampled.pixels.list, calc_CI) %>% 
  bind_rows()

#### MERGE WITH FIELD DATA

## CALC CI VALUS FROM RADIOMETER RRS DATA
source("Scripts/calc_CI_functions.R") # load functions
field_CI_values <- calc_CI_values(in_dir = "Data/rrs_data", out_path = "Data") 

CI_field_sat <- left_join(ci_sat_df, field_CI_values, by= c("waterbody", "site"))
#write_tsv(CI_field_sat, path= "Data/CI_field_sat_NOAA_tiff.tsv")


# library(ggplot2)
# ggplot(CI_field_sat) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   geom_abline(intercept= 0, slope= 1) +
#   geom_point(aes(x= CI_sat, y= CI_field, color= waterbody), size= 2) +
#   theme_bw()


