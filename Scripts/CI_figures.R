## Script to plot the rrs and CI values
## OLCI bands (nanometers, nm) 620, 665, 681, and 709

## TXT files generated in calc_CI_and_format.R script. Run this script first

#### Libraries
library(tidyverse)
library(ggplot2)


## Read in CI and rrs data
ci_fs <- read_tsv("Data/CI_field_sat_h2o_data.tsv") 
ci_fs_mod <- ci_fs %>% 
  mutate(ci_mod = ifelse(ci_mod < 0, 1, ci_mod),
         ci_mod_sat = ifelse(ci_mod < 1, 1, ci_mod_sat),
         ci_mod_final= ifelse(ss665_threshold == "Absent", 0, ci_mod))


## CIcyano results
ci_fs_mod %>% 
  count(ss665_threshold)
summary(ci_fs$ss665)



# rrs_bands <- read_tsv("Data/rrs_OLCI_band_values.tsv") %>%
#   mutate(nm= ifelse(band == "b07_620", 620,
#                     ifelse(band == "b08_665", 665,
#                            ifelse(band == "b10_681", 681, 709))))


make_spectral_shape_plots <- function(df, sampID, out_dir){
  
  ## SS(681) CI
  
  # Filter data by band type
  point_df_681 <- filter(df, df$uniqueID == sampID, band != "b07_620")
  line_df_681 <- filter(df, uniqueID == sampID, band == "b08_665" | band == "b11_709")
  
  plot681 <- ggplot() +
    geom_line(data= line_df_681,
              aes(x= nm, y= rrs, group= sampID), linetype= "dashed", size= 0.25) +
    geom_point(data= point_df_681, 
               aes(x= nm, y= rrs), size= 3) +
    labs(x= "OLCI bands (nm)", y= "Remote sensed reflectance (rrs)") +
    scale_x_continuous(breaks= c(665, 681, 709)) +
    scale_y_continuous(limits= c(min(df$rrs), max(df$rrs))) +
    ggtitle(sampID, subtitle= "SS(681)") +
    theme_bw(base_size= 16)
  
  ggsave(plot681, filename= str_c(sampID, "_ss681", ".jpg"), path=out_dir, dpi= 300, height= 5.5, width= 5, units= "in")
  
  
  
  ## SS(665) CIcyano
  point_df_665 <- filter(df, df$uniqueID == sampID, band != "b11_709" )
  line_df_665 <- filter(df, uniqueID == sampID, band == "b07_620" | band == "b10_681")
  
  
  plot665 <- ggplot() +
    geom_line(data= line_df_665,
              aes(x= nm, y= rrs, group= sampID), linetype= "dashed", size= 0.25) +
    geom_point(data= point_df_665, 
               aes(x= nm, y= rrs), size= 3) +
    labs(x= "OLCI bands (nm)", y= "Remote sensed reflectance (rrs)") +
    scale_x_continuous(breaks= c(620, 665, 681)) +
    scale_y_continuous(limits= c(min(df$rrs), max(df$rrs))) +
    ggtitle(sampID,  subtitle= "SS(665)") +
    theme_bw(base_size= 16)
  
  ggsave(plot665, filename= str_c(sampID, "_ss665", ".jpg"), path=out_dir, dpi= 300, height= 5.5, width= 5, units= "in")
  
}
#map(rrs_bands$uniqueID, function(x) make_spectral_shape_plots(df= rrs_bands, sampID= x, out_dir= "Data/spectral_shape_plots"))

#### SUMMARY STATISTICS ####

coef_variation_site <- ci_fs %>% 
  group_by(waterbody, pix_site) %>% 
  summarize(n= length(ci),
            mean_ci= mean(ci, na.rm= TRUE),
            sd_ci= sd(ci, na.rm= TRUE),
            cv_ci= sd_ci / mean_ci) %>% 
  ungroup()

coef_variation_pixel <- ci_fs %>% 
  group_by(waterbody, pixel) %>% 
  summarize(n= length(ci),
            mean_ci= mean(ci, na.rm= TRUE),
            sd_ci= sd(ci, na.rm= TRUE),
            cv_ci= sd_ci / mean_ci) %>% 
  ungroup()


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


waterbody_labeller <- c("ClearLake_20190807" = "Clear Lake\n2019-08-07",
                        "ClearLake_20190816" = "Clear Lake\n2019-08-16",
                        "ClearLake_20191008" = "Clear Lake\n2019-10-08",
                        "LakeAlmanor_20190815" ="Lake Almanor\n2019-08-15",
                        "LakeSanAntonio_20190801" = "L. San Antonio\n2019-08-01",
                        "SanPabloReservoir_20190812" = "San Pablo Res.\n2019-08-12")

#### SEPTEMBER 2019 DATA DELIVERY ####################################################

## CIcyano Histogram
ggplot(data= ci_fs, aes(x= ss665)) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black",
                 color= "gray50") +
  labs(x= "SS(665) values", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  scale_x_continuous(limits= c(-0.003, 0),
                     breaks= seq(-0.003, 0, by= 0.0005),
                     labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
                     expand= c(0, 0)) +
  theme_sat
ggsave(last_plot(), filename= "ss665_hist.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")

## CV Histogram
ggplot(data= coef_variation_site, aes(x= cv_ci)) +
  geom_histogram() +
  # geom_histogram(binwidth= 0.0001, 
  #                boundary= 1,
  #                fill= "black",
  #                color= "gray50") +
  # labs(x= "SS(665) values", y= "Count") +
  # scale_y_continuous(expand= c(0,0)) +
  # scale_x_continuous(limits= c(-0.003, 0),
  #                    breaks= seq(-0.003, 0, by= 0.0005),
  #                    labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
  #                    expand= c(0, 0)) +
  theme_sat
# ggsave(last_plot(), filename= "ss665_hist.png", , units= "in", dpi= 300,
#        path= "Data/Figures_output")

ggplot(data= coef_variation_pixel, aes(x= abs(cv_ci))) +
  geom_histogram(binwidth = 0.05,
                 boundary= 1,
                 fill= "black") +
  # geom_histogram(binwidth= 0.0001, 
  #                boundary= 1,
  #                fill= "black",
  #                color= "gray50") +
  # labs(x= "SS(665) values", y= "Count") +
   scale_y_continuous(expand= c(0,0)) +
  scale_x_continuous(expand= c(0,0)) +
  # scale_x_continuous(limits= c(-0.003, 0),
  #                    breaks= seq(-0.003, 0, by= 0.0005),
  #                    labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
  #                    expand= c(0, 0)) +
  theme_sat
# ggsave(last_plot(), filename= "ss665_hist.png", , units= "in", dpi= 300,
#        path= "Data/Figures_output")





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

ggsave(last_plot(), filename= "ci_mod_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

ggsave(last_plot(), filename= "ci_pixel_fs.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

ggsave(last_plot(), filename= "ci_fs.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

ggsave(last_plot(), filename= "ci_mod_fs.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

ggsave(last_plot(), filename= "ci_pix_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## SS(665) X WATERBODY

ggplot(data= ci_fs, aes(x= pixel, y= ss665)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(fill= "gray75") +
  labs(x= "Measurement location", y= "SS(665 nm)") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  # facet_grid(.~waterbody, scales= "free_x") +
  facet_wrap(~waterbody, ncol= 3, scales= "free_x", labeller= as_labeller(waterbody_labeller)) +
  theme_sat +
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

ggsave(last_plot(), filename= "ss665_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## SS(665) X CI-MOD
ggplot(data= ci_fs_mod) +
  geom_point(aes(x= ss665, y= ci_mod, fill= waterbody), size= 3, shape= 21) +
  labs(x= "SS(665)", y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  theme_sat

ggsave(last_plot(), filename= "ss665_CImod.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")




## CI X WATERBODY
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= pix_site, y= ci), size= 3, shape= 21, fill= "gray75") +
  labs(x= "Measurement location", y= "Field cyano. index (CI)") +
  scale_fill_discrete(name= "Waterbody") +
  scale_y_continuous(limits= c(-0.001, 0.0041), expand= c(0.02, 0)) +
  facet_wrap(.~waterbody, ncol= 6, scales= "free_x", labeller= as_labeller(waterbody_labeller)) +
  #facet_grid(.~waterbody, scales= "free_x") +
  theme_sat +
  theme(axis.text.x= element_text(angle= 90, size= 8, vjust= 0.5),
        strip.text = element_text(size= 8))

ggsave(last_plot(), filename= "ci_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

ggsave(last_plot(), filename= "ci_mod_chla.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")

## CI X CHLA
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= chla_ugL, y= ci, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(paste("Chlorophyll-a (",mu,"g/L)")), y= "Field CI") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(expand= c(0.02, 0)) +
 # scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  theme_sat

ggsave(last_plot(), filename= "ci_chla.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")



#### DECEMBER 2019 DATA DELIVERY #########################

## Field CI X SAT CI
ci_lims <- c(-.0005, 0.005)
ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")

ggplot(data= filter(ci_fs, data_delivery == "dec2019")) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CI_sat, y= ci, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite CI value", y= "Field CI value") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CI_fs_dec2019.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## Field CI X SAT CIcyano

ggplot(data= filter(ci_fs, data_delivery == "dec2019")) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CIcyano_sat, y= ci, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[cyano]~value), y= "Field CI value") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CI_CIcyanoSat_dec2019.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")

## Field CI-mod X SAT CI-mod
cimod_lims <- c(-10, 80)
cimod_brks <- c(-10, 1, seq(10, 80, by= 10))
cimod_labels <- c("-10", "DL", "10", "20", "30", "40", "50", "60", "70", "80")

ggplot(data= filter(ci_fs, data_delivery == "dec2019")) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point(aes(x= CI_mod_sat, y= ci_mod, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[mod]~value), y= expression(Field~CI[mod]~value)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CImod_fs_dec2019.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")

## Field CI-mod X SAT CI-mod

ggplot(data= filter(ci_fs, data_delivery == "dec2019")) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point(aes(x= CIcyano_mod_sat, y= ci_mod, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[cyano-mod]~value), y= expression(Field~CI[mod]~value)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CImod_CIcyanoSat_dec2019.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")



## Sat CI-mod X SAT CIcyano-mod
ci_lims <- c(6.309573e-05, 0.005)
ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")

ggplot(data= filter(ci_fs, data_delivery == "dec2019")) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05, size= 0.5) +
  geom_vline(xintercept = 6.309573e-05, size= 0.5) +
  geom_point(aes(x= CI_sat, y= CIcyano_sat, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI~value), y= expression(Satellite~CI[cyano]~value)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CISat_CIcyanoSat_dec2019.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")




ggplot(data= ci_fs) +
  geom_point(aes(x= waterbody, y= pix_CIcyano_sat, fill= data_delivery), size= 3, shape= 21, position= "jitter") +
  #labs(x= expression(Satellite~CI~value), y= expression(Satellite~CI[cyano]~value)) +
  theme_sat

