
## Original CIcyano files were sent to KBG by Randy Turner (SFEI) in September-2019. In December 2019 CI, CIcyano, and nonCIcyano CSV files were
## sent by Randy Turner. The pixel IDs did not match the original files from Sep-2019. This script was revised to re-identify the pixel ID numbers where sampling occurred
## The suffix OG, CI and CIcyano. Refer to the original (OG) CIcyano file from Sep-2019 and the new CI and CIcyano CSV files from Dec-2019.




library(tidyverse)

df <- read_tsv("Data/CI_field_sat_h2o_data.tsv")

df %>% 
  select(uniqueID, data_delivery, pix_num, contains("sat")) %>% 
  pivot_wider(names_from = data_delivery, values_from = c(pix_CIcyano_sat))


?pivot_wider
