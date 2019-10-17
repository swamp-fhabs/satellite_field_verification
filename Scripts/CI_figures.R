## Script to plot the rrs and CI values
## OLCI bands (nanometers, nm) 620, 665, 681, and 709

## TXT files generated in calc_CI_from_rrs.R script. Run this script first

#### Libraries
library(tidyverse)
library(ggplot2)

## Read in CI and rrs data
ci_fs <- read_tsv("Data/CI_field_sat_h2o_data.tsv") 
ci_fs_mod <- ci_fs %>% 
  mutate(ci_mod = ifelse(ci_mod < 0, 1, ci_mod),
         ci_mod_sat = ifelse(ci_mod < 1, 1, ci_mod_sat))





# rrs_bands <- read_tsv("Data/rrs_OLCI_band_values.tsv") %>% 
#   mutate(nm= ifelse(band == "b07_620", 620,
#                     ifelse(band == "b08_665", 665,
#                            ifelse(band == "b10_681", 681, 709))))


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


## CI-mod X WATERBODY

ggplot(data= ci_fs, aes(x= pixel, y= ci_mod)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(fill= "gray75") +
  #geom_point(size= 2, color= "black", alpha= 0.4) +
  labs(x= "Measurement location", y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  #facet_grid(.~waterbody, scales= "free_x") +
  facet_wrap(~waterbody, ncol= 3, scales= "free_x") +
  theme_sat +
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

ggsave(last_plot(), filename= "ci_mod_wbd.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")




## CI PIXELS FIELD X SAT
cifs_lims <- c(1, 200)
#cifs_brks <- seq(1, 150, by= 25)

ggplot(data= ci_fs_mod) +
  annotation_logticks() +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(x= pix_val_sat+1, y= pix_val+1, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite pixel value", y= "Field pixel value") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_x_continuous(limits= cifs_lims, breaks= cifs_brks, expand= c(0.02, 0)) +
  #scale_y_continuous(limits= cifs_lims, breaks= cifs_brks, expand= c(0.02, 0)) +
  scale_x_log10(limits= cifs_lims, expand= c(0.01, 0)) +
  scale_y_log10(limits= cifs_lims, expand= c(0.01, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "ci_pixel_fs.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")

## CI FIELD X SAT
ci_lims <- c(-0.001, 0.004)
ci_brks <- seq(-0.001, 0.004, by= 0.001)

ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(x= ci_sat, y= ci, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite CI value", y= "Field CI value") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= c(0, 0.004), breaks= ci_brks, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "ci_fs.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")


## CI-mod FIELD X SAT
cimod_lims <- c(0.9, 60)
cimod_brks <- c(1, seq(10, 60, by= 10))

ggplot(data= ci_fs_mod) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point(aes(x= ci_mod_sat, y= ci_mod, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite CI-mod", y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cimod_lims, breaks= cimod_brks, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cimod_lims, breaks= cimod_brks, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "ci_mod_fs.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")



## CI PIXELS X WATERBODY

ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= pix_site, y= pix_val), size= 3, shape= 21, fill= "gray75") +
  labs(x= "Measurement location", y= "Field pixel value") +
  scale_fill_discrete(name= "Waterbody") +
  #facet_grid(.~waterbody, scales= "free_x") +
  facet_wrap(~waterbody, ncol= 3, scales= "free_x") +
  theme_sat#+
  #theme(axis.text.x= element_text(angle= 90))

ggsave(last_plot(), filename= "ci_pix_wbd.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")


## SS(665) X WATERBODY

ggplot(data= ci_fs, aes(x= pixel, y= ss665)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(fill= "gray75") +
  labs(x= "Measurement location", y= "SS(665 nm)") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  # facet_grid(.~waterbody, scales= "free_x") +
  facet_wrap(~waterbody, ncol= 3, scales= "free_x") +
  theme_sat +
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

ggsave(last_plot(), filename= "ss665_wbd.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")





## CI X WATERBODY

ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= pix_site, y= ci), size= 3, shape= 21, fill= "gray75") +
  labs(x= "Measurement location", y= "Cyanobacterial Index (CI)") +
  scale_fill_discrete(name= "Waterbody") +
  scale_y_continuous(limits= c(-0.001, 0.004), expand= c(0.02, 0)) +
  facet_grid(.~waterbody, scales= "free_x") +
  theme_sat +
  theme(axis.text.x= element_text(angle= 90))

ggsave(last_plot(), filename= "ci_wbd.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")



## CI-mod X CHLA
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= chla_ugL, y= ci_mod, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Chlorophyll-a (ug/L)", y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(expand= c(0.02, 0)) +
  scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  theme_sat

ggsave(last_plot(), filename= "ci_mod_chla.jpg", height= 6, width= 8, units= "in", dpi= 300,
       path= "Data/Figures_output")

