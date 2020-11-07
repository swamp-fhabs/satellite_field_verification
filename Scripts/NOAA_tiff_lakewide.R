library(tidyverse)
library(ggplot2)
library(lemon)
library(cowplot)
library(extrafont)
library(scales)

source("Scripts/NOAA_tiff_functions.R")
source("Scripts/NOAA_tiff_format.R")
source("Scripts/ggplot_themes.R")

# Open UTM lakes shapefile R object
ca_lakes_utm <- readRDS("Data/Shapefiles/ca_lakes_shapefile_utm.rds")

# Run function to extract specific lake shapefile based on DFGWATERID field
lsa_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 6342, utm= 10)
spr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 3884, utm= 10)
clr_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 2075, utm= 10)
alm_utm <- extract_lake_shapefile(ca_lakes_utm, DFGWATER_ID= 1116, utm= 10)

# Import NOAA tiffs
#noaa.tiff.list <- map(noaa.field.info$NOAA_path, read_noaa_tiff)
#saveRDS(noaa.tiff.list, "Data/ss665_data/NOAA_tiffs.RDS")
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
  select(waterbody, pix_FID, ss665_sat, mci_sat, CI_sat, CIcyano_sat, starts_with("rhos"), geometry) %>% 
  mutate(CI_sat.POS= ifelse(CI_sat < 0, 0, CI_sat),
         false_neg= ifelse(CIcyano_sat == 0 & CI_sat > 0, "Y", "N"))


pix.df %>% 
  group_by(waterbody) %>% 
  #count(false_neg)
  mutate(prop= prop.table())


false.negs <- pix.df %>% 
  count(waterbody, false_neg) %>% 
  group_by(waterbody) %>% 
  mutate(prop= prop.table(n)) %>% 
  st_set_geometry(NULL)


pix.df2 <- st_set_geometry(pix.df, NULL) %>% 
  select(waterbody, CI_sat, CIcyano_sat, ss665_sat) %>% 
  mutate(CI_sat_r= round(CI_sat, 4),
         ss665_sat_b= ifelse(ss665_sat > 0, 1, 0))

pix.counts <- pix.df2 %>% 
  count(CI_sat_r, ss665_sat > 0) %>% 
  rename(ss665_binary= `ss665_sat > 0`)

ggplot(pix.df2, aes(x= CI_sat, y= ss665_sat_b)) +
  geom_point() +
  geom_smooth(method= "gam")


fit.1 <- glm(ss665_sat_b ~ CI_sat, 
             family= "binomial",
             data= pix.df2)
summary(fit.1)


#### MAKE PLOTS #######################################


ggplot(pix.df, aes(x= ss665_sat)) +
  geom_histogram(binwidth= 0.0001, boundary= 1, fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "SS(665) satellite", y= "Count") +
  scale_y_continuous(expand= c(0, 0)) +
  facet_rep_wrap(~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat

ggplot(pix.df, aes(x= mci_sat)) +
  geom_histogram(binwidth= 0.001, boundary= 1, fill= "black") +
  #geom_histogram(boundary= 1, fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "Satellite MCI", y= "Count") +
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
  labs(x= "Satellite SS(665)", y= "Satellite CI") +
  scale_color_discrete(guide= FALSE) +
  facet_rep_wrap(~waterbody, ncol= 2, labeller= as_labeller(waterbody_labeller)) +
  theme_sat
ggsave(last_plot(), filename= "Lakewide_CI_SS665.png", height= 7, width= 7, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")

ggplot(pix.df, aes(x= mci_sat, y= CI_sat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color= waterbody)) +
  labs(x= "Satellite MCI", y= "Satellite CI") +
  scale_color_discrete(guide= FALSE) +
  facet_rep_wrap(~waterbody, ncol= 2, labeller= as_labeller(waterbody_labeller)) +
  theme_sat
ggsave(last_plot(), filename= "Lakewide_CI_MCI.png", height= 7, width= 7, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


ggplot(pix.df, aes(x= mci_sat, y= ss665_sat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color= waterbody)) +
  labs(x= "Satellite MCI", y= "Satellite SS(665)") +
  scale_color_discrete(guide= FALSE) +
  facet_rep_wrap(~waterbody, ncol= 2, labeller= as_labeller(waterbody_labeller)) +
  theme_sat
ggsave(last_plot(), filename= "Lakewide_ss665_MCI.png", height= 7, width= 7, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")






pixel_plots <- function(df, wb_name, lk_shp, out_name){

  df_buffer <- df %>% 
  filter(waterbody == wb_name) %>% 
  st_buffer(., dist= 150, endCapStyle= "SQUARE") 


ci.plot <- ggplot() +
  geom_sf(data= lk_shp, fill= "lightsteelblue1") +
  geom_sf(data= df_buffer, aes(fill= CI_sat.POS), color= "transparent") +
  scale_fill_gradientn(colors= c("gray60", viridis_pal(begin= 0.4, option= "magma")(50)), 
                       limits= c(0, 0.06), name= "CI") +
  coord_sf() +
  theme_sat
message("Saving CI plot")
ggsave(ci.plot, filename= str_c(out_name, "_CI.png", sep= ""), height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


cicyano.plot <- ggplot() +
  geom_sf(data= lk_shp, fill= "lightsteelblue1") +
  geom_sf(data= df_buffer, aes(fill= CIcyano_sat), color= "transparent") +
  scale_fill_gradientn(colors= c("gray60", viridis_pal(begin= 0.4, option= "magma")(50)), 
                       limits= c(0, 0.06), name= "CI_cyano") +
  coord_sf() +
  theme_sat
message("Saving CI_cyano plot")
ggsave(cicyano.plot, filename= str_c(out_name, "_CIcyano.png", sep= ""), height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")

fn.hist <- ggplot(df_buffer, aes(x= CI_sat.POS)) +
  geom_histogram(binwidth= 0.0001, boundary=1, color= "gray50") +
  facet_wrap(~false_neg, nrow= 2, labeller= as_labeller(c("Y" = "Potential CI_cyano false negative",
                                                          "N" = "CI and CI_cyano agree"))) +
  labs(x= "Satellite CI", y= "Count") +
  scale_x_continuous(expand= c(0, 0)) +
  scale_y_continuous(expand= c(0, 0)) +
  theme_sat
message("Saving histogram")
ggsave(fn.hist, filename= str_c(out_name, "_FalseNegs_hist.png", sep= ""), height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


}


pixel_plots(df= pix.df, wb_name= "LakeSanAntonio_20190801", lk_shp= lsa_utm, out_name= "LakeSanAntonio_20190801")
pixel_plots(df= pix.df, wb_name= "ClearLake_20190807", lk_shp= clr_utm, out_name= "ClearLake_20190807")
pixel_plots(df= pix.df, wb_name= "ClearLake_20190816", lk_shp= clr_utm, out_name= "ClearLake_20190816")
pixel_plots(df= pix.df, wb_name= "ClearLake_20191008", lk_shp= clr_utm, out_name= "ClearLake20191008")
pixel_plots(df= pix.df, wb_name= "ClearLake_20200708", lk_shp= clr_utm, out_name= "ClearLake_20200708")
pixel_plots(df= pix.df, wb_name= "ClearLake_20200724", lk_shp= clr_utm, out_name= "ClearLake_20200724")
pixel_plots(df= pix.df, wb_name= "LakeAlmanor_20190815", lk_shp= alm_utm, out_name= "LakeAlmanor_20190815_MCI")
pixel_plots(df= pix.df, wb_name= "SanPabloReservoir_20190812", lk_shp= spr_utm, out_name= "SanPabloReservoir_20190812")


