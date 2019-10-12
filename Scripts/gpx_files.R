library(tidyverse)
library(tidyverse)
library(tmaptools)


## GPX time stamp at prime meridian UTC
## California is -8 UTC in winter and -7 UTC in summer during daylight savings time


## Read in GPX
gpx_sf <- read_GPX("Data/20191008_ClearLake/2019-10-08 13.09.10.gpx")
gpx_tp <- gpx_sf$track_points


## Transform into a tibble and format columns
# use do.call to extract the lat/long geometry from class sf
gpx_df <- tibble(time= as.character(gpx_tp$time), 
                 lon= do.call(rbind, st_geometry(gpx_tp$geometry))[, 1],
                 lat= do.call(rbind, st_geometry(gpx_tp$geometry))[, 2]) %>% 
  mutate(date_time= ymd_hms(str_replace(gpx_df$time, "\\+00", "")))


