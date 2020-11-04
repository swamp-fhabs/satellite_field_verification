library(tidyverse)
library(ggplot2)
library(ggplot2)
library(lemon)
library(cowplot)
library(extrafont)

source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342, utm= 10)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884, utm= 10)
clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075, utm= 10)
alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116, utm= 10)

# Import NOAA tiffs
#noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)
noaa.tiff.list <- readRDS("Data/ss665_data/NOAA_tiffs.RDS")


# Extract pixels for each sampled water body
noaa.extract.map.list <- map2(noaa.tiff.list, noaa.field.info$DFGWATERID, function(x, y) extract_lake_map_pixels(tif.matrix = x, lake.id= y, utm= 10))

# Prepare list of arguments for lakewide_pixels() function
# pixel centers, lake boundary polygon, DFGWATERID
args_lakewide_pixels <- list(list(noaa.extract.map.list[1][[1]][["center"]], lsa_utm, 6342),
                             list(noaa.extract.map.list[2][[1]][["center"]], clr_utm, 2075),
                             list(noaa.extract.map.list[3][[1]][["center"]], spr_utm, 3884),
                             list(noaa.extract.map.list[4][[1]][["center"]], alm_utm, 1116),
                             list(noaa.extract.map.list[5][[1]][["center"]], clr_utm, 2075),
                             list(noaa.extract.map.list[6][[1]][["center"]], clr_utm, 2075),
                             list(noaa.extract.map.list[7][[1]][["center"]], clr_utm, 2075),
                             list(noaa.extract.map.list[8][[1]][["center"]], clr_utm, 2075))
names(args_lakewide_pixels) <-  c("LakeSanAntonio_20190801", "ClearLake_20190807", "SanPabloReservoir_20190812", "LakeAlmanor_20190815", 
                                  "ClearLake_20190816", "ClearLake_20191008", "ClearLake_20200708", "ClearLake_20200724")


## Extract lakewide pixels
lakewide.pixels <- map(args_lakewide_pixels, function(x) lakewide_pixels(points.sf= x[[1]],
                                                                         lake.polygon.sf = x[[2]],
                                                                         lake_ID= x[[3]],
                                                                         dist_m= 175))

## Calculate CI values
pix.df <- map(lakewide.pixels, calc_CI) %>% 
  bind_rows(., .id= "waterbody") %>% 
  select(waterbody, pix_FID, ss665_sat, CI_sat, CIcyano_sat, starts_with("rhos"), geometry)


#### GGPLOT THEMES ############################
theme_sat <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= "transparent", color= "black", linetype= "solid", size= 0.5),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black", size= 12),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   #axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "right")


waterbody_labeller <- c("ClearLake_20190807" = "Clear Lake\n2019-08-07",
                        "ClearLake_20190816" = "Clear Lake\n2019-08-16",
                        "ClearLake_20191008" = "Clear Lake\n2019-10-08",
                        "ClearLake_20200708" = "Clear Lake\n2020-07-08",
                        "ClearLake_20200724" = "Clear Lake\n2020-07-24",
                        "LakeAlmanor_20190815" ="Lake Almanor\n2019-08-15",
                        "LakeSanAntonio_20190801" = "L. San Antonio\n2019-08-01",
                        "SanPabloReservoir_20190812" = "San Pablo Res.\n2019-08-12")

#### MAKE PLOTS #######################################


ggplot(pix.df, aes(x= ss665_sat)) +
  geom_histogram(binwidth= 0.0001, boundary= 1, fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "SS(665) satellite", y= "Count") +
  scale_y_continuous(expand= c(0, 0)) +
  facet_rep_wrap(~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat

ggplot(pix.df, aes(x= CI_sat)) +
  geom_histogram(binwidth= 0.0001, boundary= 1, fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "CI satellite", y= "Count") +
  scale_y_continuous(expand= c(0, 0)) +
  facet_rep_wrap(~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat


ggplot(pix.df, aes(x= ss665_sat, y= CI_sat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color= waterbody)) +
  labs(x= "SS(665) satellite", y= "CI satellite") +
  scale_color_discrete(guide= FALSE) +
  facet_rep_wrap(~waterbody, ncol= 2, labeller= as_labeller(waterbody_labeller)) +
  theme_sat

ggplot(pix.df, aes(x= ss665_sat, y= CI_sat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color= waterbody)) +
  labs(x= "SS(665) satellite", y= "CI satellite") +
  scale_color_discrete(guide= FALSE) +
  facet_rep_wrap(~waterbody, ncol= 2, labeller= as_labeller(waterbody_labeller)) +
  theme_sat



