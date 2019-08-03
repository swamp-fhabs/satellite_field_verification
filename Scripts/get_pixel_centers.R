library(tidyverse)
library(rgdal)
library(raster)
library(broom)
library(sf)

slr_pix <- read_csv("Data/SanLuisReservoir_pixel.csv")
lsa_pix <- read_csv("Data/LakeSanAntonio_pixel.csv")

ggplot(data= pix) +
  geom_point(aes(x= Lon, y= Lat), size= 1)


lakes <- readOGR(dsn= "/Users/kbg/Google Drive/Maps/GIS_files/CA_Lakes", layer= "CA_Lakes")
lakes.reproj <- spTransform(lakes, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
lakes.reproj <- lakes.reproj[!is.na(lakes.reproj$NAME), ]
lakes.data <- lakes.reproj@data %>% 
  as_tibble() %>%
  mutate(id= rownames(lakes.reproj@data))


lakes.reproj$NAME[str_detect(lakes.reproj$NAME, "Antonio")]


slr <- lakes.reproj[lakes.reproj$NAME == "San Luis Reservoir", ]
slr.data <- slr@data %>% 
  as_tibble() %>%
  mutate(id= rownames(slr@data))
slr.df <- tidy(slr)

slr.df2 <- left_join(slr.df, slr.data)#, by= c("id" = "DFGWATERID"))

ggplot() +
  geom_path(data= slr.df2, aes(x= long, y= lat), fill= "skyblue", color= "black") +
  geom_point(data= slr_pix, aes(x= Lon, y= Lat), size= 1) +
  coord_sf()



lsa <- lakes.reproj[lakes.reproj$DFGWATERID == 6342, ]
lsa.data <- lsa@data %>% 
  as_tibble() %>%
  mutate(id= rownames(lsa@data))
lsa.df <- tidy(lsa)
lsa.df2 <- left_join(lsa.df, lsa.data)#, by= c("id" = "DFGWATERID"))


ggplot() +
  geom_polygon(data= lsa.df2, aes(x= long, y= lat), fill= "skyblue", color= "black") +
  #geom_point(data= lsa_pix, aes(x= Lon, y= Lat, fill= `Pixel Value`), size= 5, pch= 22) +
  geom_point(data= lsa_pix, aes(x= Lon, y= Lat), size= 1) +
  coord_sf() +
  theme_bw()

st_sfc(lsa_pix)

