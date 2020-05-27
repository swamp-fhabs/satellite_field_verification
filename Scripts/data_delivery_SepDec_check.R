
## Original CIcyano files were sent to KBG by Randy Turner (SFEI) in September-2019. In December 2019 CI, CIcyano, and nonCIcyano CSV files were
## sent by Randy Turner. The pixel IDs did not match the original files from Sep-2019. This script was revised to re-identify the pixel ID numbers where sampling occurred
## The suffix OG, CI and CIcyano. Refer to the original (OG) CIcyano file from Sep-2019 and the new CI and CIcyano CSV files from Dec-2019.




library(tidyverse)
library(ggplot2)
source("Scripts/map_functions.R")


df <- read_tsv("Data/CI_field_sat_h2o_data.tsv")

dfw <- df %>% 
  dplyr::select(waterbody, pixel, data_delivery, pix_num, pix_CIcyano_sat) %>% 
  distinct() %>% 
  pivot_wider(names_from = data_delivery, values_from = c(pix_num, pix_CIcyano_sat)) %>% 
  distinct()



test <- ci_fs %>% 
  filter(waterbody == "ClearLake_20190816" & data_delivery == "dec2019")


ggplot(data= dfw) +
  geom_abline(slope= 1, intercept= 0) +
  geom_point(aes(x= pix_CIcyano_sat_sep2019, y= pix_CIcyano_sat_dec2019), size= 3) +
  facet_wrap(~waterbody, nrow= 3)
ggsave(last_plot(), filename= "SepDec_Sat_CIcyano.png", height= 6, width= 6, units= "in",
       path= "../SepDec_SatData")

ggplot(data= dfw) +
  geom_point(aes(x= pix_CIcyano_sat_sep2019, y= pix_CIcyano_sat_dec2019))

ggplot(data= dfw) +
  geom_point(aes(x= "", y= pix_CIcyano_sat_dec2019))

ggplot(data= dfw) +
  geom_point(aes(x= "", y= pix_CIcyano_sat_sep2019))



## Download mosaic from fhabs.sfei.org
## Clear Lake 16-Aug-2-2019
library(raster)
library(sf)
#library(rgdal)
class(bloom)

## https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/
bloom <- raster("Data/Sentinel_flyover_data/OLCI_mosaicCyano_20190816_Clear_Lake.tif")
bloom.df <- as.data.frame(bloom, xy = TRUE) %>% 
  rename(CIcyano= OLCI_mosaicCyano_20190816_Clear_Lake) %>% 
   as_tibble()


bloom.sf.utm <- st_as_sf(bloom.df, 
         coords= c("x", "y"), 
         crs= as.character(crs(bloom)))

bloom.sf.ll <- st_transform(bloom.sf.utm, "+proj=longlat +datum=WGS84 +ellps=WGS84")

bloom.sf.ll %>% 
  filter(CIcyano == 124)



###### Look at Lat Long relationships
## Confirm that pixels are located in the same location up to 5 decimal places (~1 meter distance)
## Pixels that are not in the September and December datasets are on the shoreline and related to the
## 1 pixel shift up/down that occurred

## ClearLake_20190816	 Pixel CL03C

dec <- read_csv("Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.CIcyano.csv") %>% 
  rename(cicy= `Pixel Value`) %>% 
  mutate(llid= str_c(Lat, Lon, sep= " , "),
         mon= "Dec") %>% 
  mutate(llid1= str_c(round(Lat, 1), round(Lon, 1), sep= " , ")) %>%
  mutate(llid2= str_c(round(Lat, 2), round(Lon, 2), sep= " , ")) %>%
  mutate(llid3= str_c(round(Lat, 3), round(Lon, 3), sep= " , ")) %>% 
  mutate(llid4= str_c(round(Lat, 4), round(Lon, 4), sep= " , ")) %>% 
  mutate(llid5= str_c(round(Lat, 5), round(Lon, 5), sep= " , ")) %>% 
  mutate(llid6= str_c(round(Lat, 6), round(Lon, 6), sep= " , ")) %>% 
  mutate(llid7= str_c(round(Lat, 7), round(Lon, 7), sep= " , ")) %>% 
  mutate(llid8= str_c(round(Lat, 8), round(Lon, 8), sep= " , ")) 

sep <- read_csv("Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.csv") %>% 
  rename(cicy= `Pixel Value`) %>% 
  mutate(llid= str_c(Lat, Lon, sep= " , "),
         mon= "Sep") %>% 
  mutate(llid1= str_c(round(Lat, 1), round(Lon, 1), sep= " , ")) %>%
  mutate(llid2= str_c(round(Lat, 2), round(Lon, 2), sep= " , ")) %>%
  mutate(llid3= str_c(round(Lat, 3), round(Lon, 3), sep= " , ")) %>% 
  mutate(llid4= str_c(round(Lat, 4), round(Lon, 4), sep= " , ")) %>% 
  mutate(llid5= str_c(round(Lat, 5), round(Lon, 5), sep= " , ")) %>% 
  mutate(llid6= str_c(round(Lat, 6), round(Lon, 6), sep= " , ")) %>% 
  mutate(llid7= str_c(round(Lat, 7), round(Lon, 7), sep= " , ")) %>% 
  mutate(llid8= str_c(round(Lat, 8), round(Lon, 8), sep= " , ")) 

table(dec$llid %in% sep$llid)
table(dec$llid2 %in% sep$llid2)
table(dec$llid3 %in% sep$llid3)
table(dec$llid4 %in% sep$llid4)
table(dec$llid5 %in% sep$llid5)
table(dec$llid6 %in% sep$llid6)
table(dec$llid7 %in% sep$llid7)
table(dec$llid8 %in% sep$llid8)



dec[dec$llid2 %in% sep$llid2, ]

both <- full_join(dec, sep)
anti4d <- anti_join(dec, sep, by= "llid4")
anti4s <- anti_join(sep, dec, by= "llid4")
write_tsv(anti4d, path= "../SepDec_SatData/Dec_mismatch_4DD.tsv")

anti5d <- anti_join(dec, sep, by= "llid5")
anti5s <- anti_join(sep, dec, by= "llid5")






both.w <- both %>% 
  dplyr::select(mon, llid5) %>% 
  arrange(llid5)
  pivot_wider(names_from = mon, values_from = "llid5", values_fn = list(llid5 = length))


ggplot(data= anti4d) +
  geom_point(aes(x= Lon, y= Lat))
  

ggplot(filter(bloom.sf.ll, CIcyano != 252)) +
  geom_sf()+
  geom_point(data= anti5d, aes(x= Lon, y= Lat), color= "red") +
  geom_point(data= anti5s, aes(x= Lon, y= Lat), color= "blue")


#### See if pixels have the same value
## The pixel right below the one specified in my data is 124, so I think that the Dec data 
## has the same results, but is just shifted 1 down
# CL03C


sep124 <- sep %>% 
  filter(cicy == 124 & Lat == 38.9641253581)


left_join(sep124, dplyr::select(dec, cicy, mon, llid5))

dec124 <- dec %>% 
  filter(llid2 == sep124$llid2 & str_detect(llid3, ".67"))



ggplot(filter(bloom.sf.ll, CIcyano != 252)) +
  geom_sf()+
  geom_point(data= dec124, aes(x= Lon, y= Lat, color= as.factor(cicy)), size= 3) +
  geom_point(aes(x= -122.67919, y= 38.96326), color= "green", shape= 3) +
  coord_sf(x= c(-122.6, -122.7), y= c(38.95, 38.97)) +
  ggtitle("ClearLake_20190807 P3 CIcyano=124")
ggsave(last_plot(), filename= "ClearLake_20190816_CL03C_124.png", height= 6, width= 6, units= "in",
       path= "../SepDec_SatData")


### Clear Lake Aug-07
dec07 <- read_csv("Data/Sentinel_flyover_data/sentinel-ClearLake_20190807.CIcyano.csv") %>% 
  rename(cicy= `Pixel Value`) %>% 
  mutate(llid= str_c(Lat, Lon, sep= " , "),
         mon= "Dec") %>% 
  mutate(llid1= str_c(round(Lat, 1), round(Lon, 1), sep= " , ")) %>%
  mutate(llid2= str_c(round(Lat, 2), round(Lon, 2), sep= " , ")) %>%
  mutate(llid3= str_c(round(Lat, 3), round(Lon, 3), sep= " , ")) %>% 
  mutate(llid4= str_c(round(Lat, 4), round(Lon, 4), sep= " , ")) %>% 
  mutate(llid5= str_c(round(Lat, 5), round(Lon, 5), sep= " , ")) %>% 
  mutate(llid6= str_c(round(Lat, 6), round(Lon, 6), sep= " , ")) %>% 
  mutate(llid7= str_c(round(Lat, 7), round(Lon, 7), sep= " , ")) %>% 
  mutate(llid8= str_c(round(Lat, 8), round(Lon, 8), sep= " , ")) 

sep07 <- read_csv("Data/Sentinel_flyover_data/sentinel-ClearLake_20190807.csv") %>% 
  rename(cicy= `Pixel Value`) %>% 
  mutate(llid= str_c(Lat, Lon, sep= " , "),
         mon= "Sep") %>% 
  mutate(llid1= str_c(round(Lat, 1), round(Lon, 1), sep= " , ")) %>%
  mutate(llid2= str_c(round(Lat, 2), round(Lon, 2), sep= " , ")) %>%
  mutate(llid3= str_c(round(Lat, 3), round(Lon, 3), sep= " , ")) %>% 
  mutate(llid4= str_c(round(Lat, 4), round(Lon, 4), sep= " , ")) %>% 
  mutate(llid5= str_c(round(Lat, 5), round(Lon, 5), sep= " , ")) %>% 
  mutate(llid6= str_c(round(Lat, 6), round(Lon, 6), sep= " , ")) %>% 
  mutate(llid7= str_c(round(Lat, 7), round(Lon, 7), sep= " , ")) %>% 
  mutate(llid8= str_c(round(Lat, 8), round(Lon, 8), sep= " , ")) 


## ClearLake_20190807	 Pixel P3

dec07_94 <- dec07 %>% 
  filter(cicy == 94)

sep07_94 <- sep07 %>% 
  filter(cicy == 94)


ggplot(filter(bloom.sf.ll, CIcyano != 252)) +
  geom_sf() +
  geom_point(data= dec07_94, aes(x= Lon, y= Lat, color= mon), size= 3) +
  geom_point(data= sep07_94, aes(x= Lon, y= Lat, color= mon), size= 3) +
  geom_point(aes(x= -122.71622, y= 38.98348), color= "green", shape= 3) +
  geom_point(aes(x= -122.71656, y= 38.98281), color= "green", shape= 3) +
  coord_sf(x= c(-122.71, -122.72), y= c(38.97, 38.99)) +
  ggtitle("ClearLake_20190807 P3 CIcyano=94")
ggsave(last_plot(), filename= "ClearLake_20190807_P3_94.png", height= 6, width= 6, units= "in",
       path= "../SepDec_SatData")



## ClearLake_20190807	 Pixel P1

dec07_117 <- dec07 %>% 
  filter(cicy == 117)

sep07_117 <- sep07 %>% 
  filter(cicy == 117)


ggplot(filter(bloom.sf.ll, CIcyano != 252)) +
  geom_sf() +
  geom_point(data= dec07_117, aes(x= Lon, y= Lat, color= mon), size= 3) +
  geom_point(data= sep07_117, aes(x= Lon, y= Lat, color= mon), size= 3) +
  geom_point(aes(x= -122.70942, y= 38.97833), color= "green", shape= 3) +
  geom_point(aes(x= -122.70921, y= 38.97916), color= "green", shape= 3) +
  coord_sf(x= c(-122.7, -122.715), y= c(38.97, 38.98)) +
  ggtitle("ClearLake_20190807 P1 CIcyano=117")
ggsave(last_plot(), filename= "ClearLake_20190807_P1_117.png", height= 6, width= 6, units= "in",
       path= "../SepDec_SatData")

38.97916	-122.70921


  

