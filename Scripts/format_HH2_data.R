

## SEPTEMBER-2019: OLD SCRIPT REPLACED BY radiometer_noaa_format.R
## KEEPING FOR A BIT IN CASE I NEED TO REFER TO ANY OF THESE FUNCTIONS

#### LIBRARIES
library(tidyverse)
library(lubridate)
library(ggplot2)

## READ IN METADATA
format_metadata <- function(file_path){
  # Read in metadata file
  meta <- suppressMessages(read_tsv(file_path)) %>% 
    mutate(file_start_num= as.numeric(file_start),
           file_end_num= as.numeric(file_end),
           Date= mdy(Date))
  # Make long to get start and end file numbers in same column
  meta.l <- meta %>% 
    select(-file_start, -file_end) %>% 
    gather(key= start_end, value= file_num, file_start_num:file_end_num) %>% 
    arrange(file_num)
  
  # Fill out the vector between start and end numbers
  fill_df <- data.frame(file_num= full_seq(meta.l$file_num, 1)) %>% as_tibble()
  
  # Join complete vector with metadata and fill in remaining columns
  meta_fill <- left_join(fill_df, meta.l, by= "file_num") %>% 
    select(-start_end) %>% 
    fill(everything()) %>% 
    mutate(spec_file= str_c(basename, 
                       str_pad(.$file_num, pad= "0", 5),
                       ".asd.rad"),
           spectra_id= str_c("P", pixel, pixel_rep, "_", type, type_rep, "_", spectra_rep)) %>% 
    select(-basename) %>% 
    select(spec_file, everything())
  return(meta_fill)
}
meta <- format_metadata(file_path= "./Data/20190705_135521/20190705_metadata.txt")



## READ IN DATA
# note Windows file encoding
hh2 <- read.delim("./Data/20190705_135521/20190705_processed.dat", 
                   sep= "\t", 
                   header= FALSE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "UCS-2LE") %>%
  select(-V2) %>% 
  rename(spec_file= V1, nm= V3, radiance= V4) %>% 
  as_tibble()



## FILTER WAVELENGTHS TO MATCH OLCI BANDS: 620, 665, 681, AND 709
source("Scripts/olci_bands.R")
hh2.filt <- extract_olci_bands(hh2)

#hh2.filt2 <- hh2 %>% 
#  filter(nm == 665 | nm == 681 | nm == 709)

## MERGE HH2 WITH METADATA
# hh2.meta <- left_join(hh2.filt, meta_fill, by= "file") %>% 
#   filter(type != "ERROR")

hh2.meta <- left_join(hh2, meta, by= "spec_file") %>% 
  filter(type != "ERROR")

#### PLOT SPECTRA
ggplot(data= hh2.meta, aes(x= nm, y= radiance)) +
  geom_line(aes(color= spec_file)) + 
  facet_wrap(~spectra_id, ncol= 6, scales= "free_y") +
  scale_color_discrete(guide=FALSE) +
  theme_bw()
  

ggplot(data= filter(hh2.meta, spectra_id == "P1A_SA1_1"), aes(x= nm, y= radiance)) +
  geom_line(aes(color= spec_file)) + 
  facet_wrap(~spectra_id, ncol= 6, scales= "free_y") +
  scale_color_discrete(guide=FALSE) +
  theme_bw()


hh2.meta %>% 
  group_by(spectra_id) %>% 
  mutate(reading_num= )



#### ASSESS QUALITY/VARIANCE OF SPECTRA ####
ggplot(data= hh2.meta) +
  geom_point(aes(x= nm, y= radiance, color= factor(type_rep))) +
  facet_grid(type~spectra_rep, scales= "free_y") +
  theme_bw()

hh2.statistics <- hh2.meta %>% 
  group_by(fov, type, spectra_rep, nm) %>% 
  summarize(
    n= length(radiance),
    mean_rad= mean(radiance, na.rm= TRUE),
    sd_rad= sd(radiance, na.rm= TRUE),
    cv_rad= sd_rad/mean_rad
  )


## AVERAGE OVER ALL THE DIFFFERENT SPECTRA
hh2.means <- hh2.meta %>% 
  group_by(fov, type, nm) %>% 
  summarize(
    n= length(radiance),
    mean_rad= mean(radiance, na.rm= TRUE),
    sd_rad= sd(radiance, na.rm= TRUE),
    cv_rad= sd_rad/mean_rad
  )


ggplot(data=hh2.means) +
  geom_point(aes(x= nm, y= mean_rad)) +
  geom_errorbar(aes(x= nm, ymin= mean_rad - sd_rad, ymax= mean_rad + sd_rad), width= 0.2) +
  facet_grid(type~fov, scales= "free_y") +
  theme_bw()

## CALCULATE THE CYANOBACTERIAL INDEX
hh2.means.w <- hh2.means %>% 
  select(-sd_rad, -cv_rad, -n) %>% 
  spread(nm, mean_rad) %>% 
  rename(nm665= `665`, nm681= `681`, nm709= `709`)

# From Wynne et al. 2008
calc_CI <- function(df){
  CI= with(df, -1*(nm681 - nm665 - (nm709 - nm665)*((681-665)/(709-665))) )
}

ci_value <- calc_CI(hh2.means.w)


