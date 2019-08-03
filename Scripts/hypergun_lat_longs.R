## Script to extract time stamps from HyperGun data files from Calaveras Reservoir 2018
library(tidyverse)
library(ggmap)
library(ggplot2)
library(rgdal)
library(broom)
library(raster)
library(sf)
## LIST SAMPLE FOLDERS
sample_folders <- list.files("Data/Calaveras_radiometer_data_2018", pattern= "Calaveras*", full.names = TRUE)

#folder= sample_folders[1]
#filename= hg_data[1]

## LOOP OVER ALL THE SAMPLE FOLDERS TO EXTRACT TIMING DATA
lat_longs <- map(sample_folders, function(folder){
  
  ## LIST HG FILES FOR PLATE, WATER, AND SKY
  hg_data <- list.files(folder, pattern = "^(water|sky|plate).*txt") 
  
  ## FUNCTION TO EXTRACT TIME STAMP FROM SPECIFIC HG FILES
  lat_long_list <- map(hg_data,
                      function(filename){
                        suppressMessages(read_delim(file.path(folder, filename),
                                                    delim= "\t",
                                                    col_names = FALSE,
                                                    skip= 4,
                                                    n_max = 1)) %>% 
                          select(X4, X6) %>% 
                          rename(lat= X4, long= X6) %>% 
                          mutate(long= -1*long) 
                      }
  )
  lat_long_df <- do.call(rbind, lat_long_list)
  
  ## GET TYPE OF MEASUREMENT: PLATE, WATER, SKY
  type <- str_replace(hg_data, "_.*$", "")
  
  ## COMBINE INTO AN OUTPUT DATA FRAME
  hg_lat_longs <-  tibble(sample= str_replace(folder, "Data/Calaveras_radiometer_data_2018/", ""),
                          type) %>% 
    cbind(., lat_long_df["lat"], lat_long_df["long"])
  
  return(hg_lat_longs)
})

lat_longs_df <- do.call(rbind, lat_longs) %>% 
  as_tibble() %>% 
  mutate(lat2= lat/100,
         long2= long/100) %>% 
  mutate(pixel= ifelse(str_detect(sample, "P1"), "P1",
                       ifelse(str_detect(sample, "P2"), "P2", "P3")),
         site= ifelse(str_detect(sample, "S1"), "S1",
                       ifelse(str_detect(sample, "S2"), "S2", "S3")),
         rep= ifelse(str_detect(sample, "Rep1"), "Rep1",
               ifelse(str_detect(sample, "Rep2"), "Rep2", "Rep3")),
         PS= str_c(pixel, site, by= ""))


lat_longs_df2 <- lat_longs_df %>% 
  #filter(complete.cases(.)) %>% 
  st_as_sf(., coords= c("long2", "lat2"))

ll2 <- st_set_crs(lat_longs_df2, 4326)

class(ll2)


ggplot() +
  geom_sf(data= ll2, aes(color= PS), pch=1)

?st_set_crs
st_is_longlat(lat_longs_df)

ggplot(data= filter(lat_longs_df, sample == "Calaveras_P1S1_Rep1")) +
  geom_point(aes(x= long, y= lat, color= type))

ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2, color= PS), size= 3) +
  labs(title= "Calaveras 2018") +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_locations_1.pdf", height= 6, width= 8, units= "in", path= "Figures", device= cairo_pdf)

ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2, color= pixel, shape= site), size= 3) +
  labs(title= "Calaveras 2018") +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_locations_2.pdf", height= 6, width= 8, units= "in", path= "Figures", device= cairo_pdf)


ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2, color= site), size= 3) +
  labs(title= "Calaveras 2018") +
  facet_grid(pixel~.) +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_locations_3.pdf", height= 6, width= 8, units= "in", path= "Figures", device= cairo_pdf)


ggplot(data= filter(lat_longs_df, type == "water")) +
  geom_point(aes(x= long2, y= lat2, color= site), size= 3) +
  labs(title= "Calaveras 2018") +
  facet_grid(pixel~site) +
  labs(title= "Calaveras 2018: water only") +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_locations_4.pdf", height= 6, width= 8, units= "in", path= "Figures", device= cairo_pdf)





?ggsave
ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2, color= PS), size= 3) +
  #facet_grid(.~pixel) +
  theme_bw()


ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2), size= 3) +
  facet_grid(site~pixel) +
  #labs(title = lat_longs_df$sample) +
  theme_bw()


ggplot(data= lat_longs_df) +
  geom_point(aes(x= long2, y= lat2), size= 3) +
  facet_grid(PS~rep) +
  theme_bw()


class(cala@polygons)

cala@polygons[[1]]@ID






lakes <- readOGR(dsn= "/Users/kbg/Google Drive/Maps/GIS_files/CA_Lakes", layer= "CA_Lakes")
lakes.reproj <- spTransform(lakes, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
lakes.df <- tidy(lakes.reproj)
lakes.df2 <- lakes.reproj@data


str(lakes.reproj)



l2 <- lakes.reproj[!is.na(lakes.reproj$NAME), ]

cala <- l2[l2$NAME == "Calaveras Reservoir" & l2$COUNTY == "Santa Clara", ]

str(droplevels(cala@data))
cala.data <- cala@data %>% 
  mutate(DFGWATERID= as.character(DFGWATERID)) %>% 
  as_tibble()

str(lakes.reproj@data)
head(lakes.reproj$NAME)

str(cala@polygons[1])
filter(lakes.df2, NAME=="Calaveras Reservoir")

lnames <- as.character(unique(lakes.df2$NAME))

sort(lnames)

IDS <- getSpPPolygonsIDSlots(lakes.reproj)
  

cala.df <- tidy(cala)

left_join(cala.df, cala.data, by= c("id" = "DFGWATERID"))

?left_join
str(cala)

ggplot() +
  geom_polygon(data= cala.df, aes(x= long, y= lat), fill= "skyblue", color= "black")  +
  #geom_point(data= lat_longs_df, aes(x= long2, y= lat2, color= type)
  geom_sf(data= ll2, aes(color= PS), pch=1)

ggplot() +
  geom_sf(data= ll2, aes(color= PS), pch=1)

