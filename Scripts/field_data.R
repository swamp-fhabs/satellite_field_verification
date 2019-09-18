library(tidyverse)
library(ggplot2)


fd <- read_tsv("Data/field_Data_satellite_2019.txt") %>% 
  mutate(secchi_avg= (secchi_disappear_m + secchi_reappear_m)/2)


## CHLA
ggplot(data= fd, aes(x= waterbody, y= chla_ugL)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()

## SECCHI X NTU
ggplot(data= fd, aes(x= turb_ntu, y= secchi_avg)) +
  geom_point(aes(color= waterbody)) +
  theme_bw()

## SECCHI X CHLA
ggplot(data= fd, aes(x= chla_ugL, y= secchi_avg)) +
  geom_point(aes(color= waterbody)) +
  theme_bw()

## CHLA X NTU
ggplot(data= fd, aes(x= chla_ugL, y= turb_ntu)) +
  geom_point(aes(color= waterbody)) +
  theme_bw()


## TEMP
ggplot(data= fd, aes(x= waterbody, y= temp_C)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()

## pH
ggplot(data= fd, aes(x= waterbody, y= pH)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()



