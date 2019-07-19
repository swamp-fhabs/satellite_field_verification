## Script to extract radiance data from HyperGun data files from Calaveras Reservoir 2018
## Then calculates CI index and makes plots
library(tidyverse)
library(ggplot2)

## LIST SAMPLE FOLDERS
sample_folders <- list.files("Data/Calaveras_radiometer_data_2018", pattern= "Calaveras*", full.names = TRUE)

## Function to get radiance data from files
get_rad_data <- function(folder){
  ## File with Hypergun radiance matched to OLCI bands and sample name
  olci_file <- list.files(folder, pattern = "seabass_OLCI.*txt") 
  sample_name <-  str_replace(folder, "Data/Calaveras_radiometer_data_2018/", "")
  
    ## Read in data
    rad_data <- read_delim(file.path(folder, olci_file), 
                           delim= ",",
                           skip= 31,
                           col_names = FALSE) %>% 
      rename(band= X1, rad= X2)
    
    ## Transform data into wide format
    rad_data <- tibble(sample_name,
                     b7_620= pull(filter(rad_data, band == "b7")["rad"]),
                     b8_665= pull(filter(rad_data, band == "b8")["rad"]),
                     b10_681=  pull(filter(rad_data, band == "b10")["rad"]),
                     b11_709=  pull(filter(rad_data, band == "b11")["rad"]))
    return(rad_data)
  }

## Use SAFELY function to handle errors
get_rad_data_safely <- safely(get_rad_data)  

## Run function
rad_list <- map(sample_folders, get_rad_data_safely) %>% 
  map("result") 

## Make into data frame
rad_df <- do.call(rbind, rad_list)
rad_df_long <- gather(rad_df, key= band, value= rad, contains("b")) %>% 
  mutate(nm= as.numeric(str_replace_all(band, "^.*_", "")))

ggplot() +
  geom_line(data= filter(rad_df_long, nm == 665 | nm == 709), aes(x= nm, y= rad), linetype= "dashed", size= 0.25) + 
  geom_point(data= filter(rad_df_long, nm != 620), aes(x= nm, y= rad), size= 2) +
  facet_wrap(~sample_name, scales= "free_y") +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_2018.pdf", height= 7, width= 10, units= "in", device= cairo_pdf)


## Calc CI index 
## CI from Wynne et al. 2008
## CIcyano from Lunetta et al. 2015

CI_df <- rad_df %>% 
  mutate(CI= -1*(b10_681 - b8_665 - (b11_709 - b8_665) * ((681 - 665)/(709 - 665))),
         CIcyano= -1*(b8_665 - b7_620 - (b10_681 - b7_620) * ((665 - 620)/(681 - 620)))) %>% 
  mutate(site= str_replace(sample_name, "_Rep.*$", ""),
         rep= str_replace(sample_name, "^.*_", ""))
write_tsv(CI_df, path= "calaveras_2018_ci.tsv")


ggplot(data= CI_df) +
  geom_point(aes(x= rep, y= CI, color= rep), size= 2) +
  scale_color_discrete(guide= FALSE) +
  facet_wrap(~site, scales= "free_y") +
  theme_bw()
ggsave(last_plot(), filename= "calaveras_2018_CI.pdf", height= 7, width= 7, units= "in", device= cairo_pdf)













