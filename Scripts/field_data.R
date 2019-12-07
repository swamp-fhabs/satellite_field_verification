library(tidyverse)
library(ggplot2)


fd <- read_tsv("Data/field_Data_satellite_2019.txt") %>% 
  mutate(secchi_avg= (secchi_disappear_m + secchi_reappear_m)/2)

#### GGPLOT THEMES ############################
theme_sat <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= "transparent", color= "black", linetype= "solid", size= 0.5),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(vjust = -0.75),
                   axis.title.y = element_text(vjust = 1.5),
                   legend.background = element_rect(size=0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   #axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "right")





## CHLA
ggplot(data= fd, aes(x= waterbody, y= chla_ugL)) +
  geom_boxplot(fill= "gray80") +
  geom_point() +
  labs(x= "Waterbody", y=expression(paste("Chlorophyll-a ("~mu,"g/L)"))) +
  theme_sat
ggsave(last_plot(), filename= "chla.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")

## SECCHI
ggplot(data= fd, aes(x= waterbody, y= secchi_avg)) +
  geom_boxplot(fill= "gray80") +
  geom_point() +
  labs(x= "Waterbody", y= "Secchi depth (m)") +
  theme_sat
ggsave(last_plot(), filename= "secchi.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")

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



