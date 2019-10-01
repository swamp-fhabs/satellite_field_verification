## Script to plot the rrs and CI values
## OLCI bands (nanometers, nm) 620, 665, 681, and 709

## TXT files generated in calc_CI_from_rrs.R script. Run this script first

#### Libraries
library(tidyverse)
library(ggplot2)

## Read in CI and rrs data
ci_fs <- read_tsv("Data/CI_values_field_sat.tsv") 
rrs_bands <- read_tsv("Data/rrs_OLCI_band_values.tsv") %>% 
  mutate(nm= ifelse(band == "b07_620", 620,
                    ifelse(band == "b08_665", 665,
                           ifelse(band == "b10_681", 681, 709))))


make_olci_band_plots <- function(df, sampID, out_dir){
  
  # Filter data by band type
  point_df <- filter(df, df$uniqueID == sampID, band != "b07_620")
  line_df <- filter(df, uniqueID == sampID, band == "b08_665" | band == "b11_709")
  
  plot1 <- ggplot() +
    geom_line(data= line_df,
              aes(x= nm, y= rrs, group= sampID), linetype= "dashed", size= 0.25) +
    geom_point(data= point_df, 
               aes(x= nm, y= rrs), size= 3) +
    labs(x= "OLCI bands (nm)", y= "Remote sensed reflectance (rrs)") +
    scale_x_continuous(breaks= c(665, 681, 709)) +
    scale_y_continuous(limits= c(min(df$rrs), max(df$rrs))) +
    ggtitle(sampID) +
    theme_bw(base_size= 16)
  
  ggsave(plot1, filename= str_c("olci_bands-", sampID, ".jpg"), path=out_dir, dpi= 300, height= 5.5, width= 5, units= "in")
  
  
}
#map(rrs_bands$uniqueID, function(x) make_olci_band_plots(df= rrs_bands, sampID= x, out_dir= "Data/olci_band_plots"))


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



## CI PIXELS FIELD X SAT
cifs_lims <- c(0, 150)
cifs_brks <- seq(0, 150, by= 25)

ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
 # geom_boxplot(aes(x= sat_pix_val, y= pix_val, color= waterbody)) +
  geom_point(aes(x= sat_pix_val, y= pix_val, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite pixel value", y= "Field pixel value") +
  #scale_shape_manual(values= c(21)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cifs_lims, breaks= cifs_brks, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cifs_lims, breaks= cifs_brks, expand= c(0.02, 0)) +
  #facet_grid(.~waterbody, scales= "free_x") +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "ci_pixel_fs.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")


# ## Plot satellite data agains field data
# ggplot(data= ci_fs) +
#   geom_boxplot(aes(x= sat_pix_val, y= pix_val)) +
#   #geom_point(aes(x= sat_pix_val, y= pix_val)) +
#   facet_grid(.~waterbody, scales= "free_x") +
#   theme_sat




ggplot(data= ci_val) +
  geom_boxplot(aes(x= waterbody, y= ci, color= site))
  theme_bw()
  

ggplot(data= ci_val) +
  geom_hline(yintercept = 0, size= 0.5) +
  geom_boxplot(aes(x= waterbody, y= ci_mod, color= site)) +
  theme_bw()

ggplot(data= ci_val) +
  geom_hline(yintercept = 0, size= 0.5) +
  geom_boxplot(aes(x= waterbody, y= pix_val, color= site)) +
  theme_bw()





# ci_df <- tibble(uniqueID= names(ci_list), ci= do.call(rbind, ci_list)[, 1]) %>% 
#   mutate(waterbody= do.call(rbind, str_split(uniqueID, "-"))[, 1], 
#          sample= do.call(rbind, str_split(uniqueID, "-"))[, 2]) %>% 
#   mutate(pix_site= do.call(rbind, str_split(sample, "_"))[, 1],
#          rep= do.call(rbind, str_split(sample, "_"))[, 2]) %>% 
#   mutate(pixel= str_sub(pix_site, start= 1, end= 2),
#          site= str_sub(pix_site, start= 3, end= 4)) %>% 
#   select(uniqueID, waterbody, sample, pix_site, pixel,site, rep, ci)


# sent_cl_20190816 <- read_csv("Data/Sentinel_flyover_data/sentinel-ClearLake_20190816.csv") %>% 
#   rename(pix_val= `Pixel Value`)
# 
# 
# sent_lsa <- sent_list[[4]] %>% 
#   rename(long= Lon, lat= Lat)
# samp_lsa <- read_tsv("Data/20190801_LakeSanAntonio/LatLong_LakeSanAntonio.txt")
# 
# 
# sum(round(samp_lsa$long[1], 2) %in% round(sent_lsa$long, 2))
# 
# val <- 1000
# lon_true <- (trunc(sent_lsa$long*val)/val) %in% (trunc(samp_lsa$long[1]*val)/val)
# 
# lat_true <- (trunc(sent_lsa$lat*val)/val) %in% (trunc(samp_lsa$lat[1]*val)/val)
# 
# which(lat_true & lon_true == TRUE)


