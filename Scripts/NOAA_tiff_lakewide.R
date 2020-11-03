library(tidyverse)
library(ggplot2)
source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342, utm= 10)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884, utm= 10)
clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075, utm= 10)
alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116, utm= 10)

# Import NOAA tif
#noaa.tiff.list <- map(noaa.field.info$NOAA.path, read_noaa_tiff)
noaa.tiff.list <- readRDS("Data/ss665_data/NOAA_tiffs.RDS")


# Extract pixels for each sampled water body
noaa.extract.map.list <- map2(noaa.tiff.list, noaa.field.info$DFGWATERID, function(x, y) extract_lake_map_pixels(tif.matrix = x, lake.id= y, utm= 10))

#### CALCULATE CI ###############################################

# LSA PIXELS 
lsa_20190801_points <- noaa.extract.map.list[1][[1]][["center"]]
lsa_20190801_pixels <- lakewide_pixels(points.sf= lsa_20190801_points,
                              lake.polygon.sf= lsa_utm, 
                              lake_ID= 6342)
lsa_20190801_CI <- calc_CI(lsa_20190801_pixels)

# CLR 2019-08-07
clr_20190807_points <- noaa.extract.map.list[2][[1]][["center"]]
clr_20190807_pixels <- lakewide_pixels(points.sf= clr_20190807_points,
                                       lake.polygon.sf= clr_utm, 
                                       lake_ID= 2075)
clr_20190807_CI <- calc_CI(clr_20190807_pixels)

# SPR 2019-08-07
spr_20190812_points <- noaa.extract.map.list[3][[1]][["center"]]
spr_20190812_pixels <- lakewide_pixels(points.sf= spr_20190812_points,
                                       lake.polygon.sf= spr_utm, 
                                       lake_ID= 3884)
spr_20190812_CI <- calc_CI(spr_20190812_pixels)

# AlM 2019-08-15
alm_20190815_points <- noaa.extract.map.list[4][[1]][["center"]]
alm_20190815_pixels <- lakewide_pixels(points.sf= alm_20190815_points,
                                       lake.polygon.sf= alm_utm, 
                                       lake_ID= 1116)
alm_20190815_CI <- calc_CI(alm_20190815_pixels)

# CLR 2019-08-16
clr_20190816_points <- noaa.extract.map.list[5][[1]][["center"]]
clr_20190816_pixels <- lakewide_pixels(points.sf= clr_20190816_points,
                                       lake.polygon.sf= clr_utm, 
                                       lake_ID= 2075)
clr_20190816_CI <- calc_CI(clr_20190816_pixels)


# CLR 2019-10-08
clr_20191008_points <- noaa.extract.map.list[6][[1]][["center"]]
clr_20191008_pixels <- lakewide_pixels(points.sf= clr_20191008_points,
                                       lake.polygon.sf= clr_utm, 
                                       lake_ID= 2075)
clr_20191008_CI <- calc_CI(clr_20191008_pixels)



# CLEAR LAKE 2020-07-08 PIXELS
clr_20200708_points <- noaa.extract.map.list[7][[1]][["center"]]
clr_20200708_pixels <- lakewide_pixels(points.sf= clr_20200708_points,
                              lake.polygon.sf= clr_utm, 
                              lake_ID= 2075)
clr_20200708_CI <- calc_CI(clr_20200708_pixels)


# CLEAR LAKE 2020-07-24 PIXELS
clr_20200724_points <- noaa.extract.map.list[8][[1]][["center"]]
clr_20200724_pixels <- lakewide_pixels(points.sf= clr_20200724_points,
                                       lake.polygon.sf= clr_utm, 
                                       lake_ID= 2075)
clr_20200724_CI <- calc_CI(clr_20200724_pixels)

###############################################




ci.list <- list(lsa_20190801_CI, clr_20190807_CI, spr_20190812_CI, alm_20190815_CI, clr_20190816_CI, clr_20191008_CI, clr_20200708_CI, clr_20200724_CI)


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
                        "LakeAlmanor_20190815" ="Lake Almanor\n2019-08-15",
                        "LakeSanAntonio_20190801" = "L. San Antonio\n2019-08-01",
                        "SanPabloReservoir_20190812" = "San Pablo Res.\n2019-08-12")

#### MAKE PLOTS ####

ggplot(lsa.ci, aes(x= ss665_sat)) +
  geom_histogram()

ggplot(lsa.ci, aes(x= ss665_sat, y= CI_sat)) +
  geom_point()


ggplot(clr.ci, aes(x= ss665_sat)) +
  geom_histogram() +
  theme_sat

ggplot(clr.ci, aes(x= ss665_sat, y= CI_sat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_sat



