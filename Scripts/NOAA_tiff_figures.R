## Script to plot the rrs and CI values
## OLCI bands (nanometers, nm) 620, 665, 681, and 709

## TXT files generated in calc_CI_and_format.R script. Run this script first

#### Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(extrafont)
library(lemon)
library(smatr)
source("Scripts/ggplot_themes.R")
source("Scripts/NOAA_tiff_functions.R")


## Read in CI and rrs data
#ci_fs <- read_tsv("Data/CI_field_sat_NOAA_tiff.tsv") 
ci_fs.avg <- read_tsv("Data/CI_field_sat_avg_NOAA_tiff.tsv") 

## Summarize over pixels
ci_fs_pix.sum <- ci_fs.avg %>% 
  group_by(waterbody, pixel) %>% 
  summarize(n= length(pixel),
            across(c(contains("field")), list(mean= mean, sd= sd, se= ~sd(.x)/sqrt(n)), .names= "{col}.{fn}"),
            .groups= "keep") %>% 
  #ungroup() %>% 
  left_join(., select(ci_fs.avg, waterbody, pixel, contains("sat")), by= c("waterbody", "pixel")) %>% 
  distinct() %>% 
  mutate(CI_field.mean.POS= ifelse(CI_field.mean < 0, 0, CI_field.mean),
         CI_sat.POS= ifelse(CI_sat < 0, 0, CI_sat))


#map(rrs_bands$uniqueID, function(x) make_spectral_shape_plots(df= rrs_bands, sampID= x, out_dir= "Data/spectral_shape_plots"))





#### MAKE FIGURES ####


## ss(665) Histogram
ggplot(data= ci_fs.avg, aes(x= ss665_field)) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "Field SS(665)", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  # scale_x_continuous(limits= c(-0.003, 0),
  #                    breaks= seq(-0.003, 0, by= 0.0005),
  #                    labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
  #                    expand= c(0, 0)) +
  facet_rep_wrap(~waterbody, ncol= 4, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat

ggsave(last_plot(), filename= "ss665_field_hist.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")

ggplot(data= ci_fs.avg, aes(x= ss665_sat)) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black") +
  geom_vline(xintercept = 0, color= "gray70") +
  labs(x= "Satellite SS(665)", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  #scale_x_continuous(limits= c(-0.003, 0),
  #                   breaks= seq(-0.003, 0, by= 0.0005),
  #                   labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
  #                   expand= c(0, 0)) +
  facet_rep_wrap(~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat
ggsave(last_plot(), filename= "ss665_sat_hist.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


## Field ss665 X Sat ss665
ggplot(data= ci_fs.avg) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(x= ss665_sat, y= ss665_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite SS(665)", y= "Field SS(665)") +
  #  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  #  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(guide= FALSE) +
  coord_equal() +
  facet_rep_wrap(~waterbody, ncol= 4, labeller= as_labeller(waterbody_labeller)) +
  theme_sat
ggsave(last_plot(), filename= "ss665_field_sat.png", height= 5, width= 12, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


## CI X WATERBODY
ggplot(data= filter(ci_fs.avg, waterbody == "ClearLake_20200708" | waterbody == "ClearLake_20200724")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.004, linetype= "dashed") +
  geom_boxplot(aes(x= site, y= CI_field), fill= "gray75") +
  labs(x= "Measurement location", y= "Field CI") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_y_continuous(limits= c(-0.001, 0.0041), expand= c(0.02, 0)) +
  facet_wrap(.~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
  theme_sat +
  theme(axis.text.x= element_text(angle= 90, size= 14, vjust= 0.5),
        strip.text = element_text(size= 14))

ggsave(last_plot(), filename= "CIF_wbd_2020.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")





## Field CI X SAT CI



CIF_CIS.plot <- ggplot(data= ci_fs.avg) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(x= CI_sat, y= CI_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite CI", y= "Field CI") +
#  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
#  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat
CIF_CIS.plot

ggsave(CIF_CIS.plot, filename= "CIF_CIS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")


clr_20200724 <- filter(ci_fs2, waterbody == "ClearLake_20200724")
#ggplot(data= filter(clr_2020, pixel == "P3"))+
  ggplot(data= clr_20200724)+
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CI_sat, y= CI_field, fill= as.character(site)), size= 3, shape= 21) +
  labs(x= "Satellite CI", y= "Field CI") +
  #  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  #  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat
  
  
  ci_lims <- c(-.0005, 0.005)
  ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
  ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")
  
  ## CELLS PER ML PLOT
  ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_point(aes(x= CI_sat.POS*10^8, y= CI_field.mean.POS*10^8, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CI_sat.POS*10^8, 
                      ymin= (CI_field.mean.POS - 1*CI_field.se)*10^8, 
                      ymax= (CI_field.mean.POS + 1*CI_field.se)*10^8)) +
    labs(x= "Satellite CI cells/mL", y= "Field CI cells/mL (\u00B1 SE)") +
    scale_fill_discrete(name= "Waterbody") +
    #scale_y_log10() +
    #scale_x_log10() +
    #annotation_logticks() +
    #scale_color_discrete(name= "Waterbody_Date") +
    scale_color_discrete(guide=FALSE) +
    coord_equal() +
    theme_sat
  
  
  ggsave(last_plot(), filename= "CIF_CIS_cellsmL.png", height= 6.5, width= 8, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  ## CELLS PER Ml DENSITY
  ggplot(data= ci_fs_pix.sum) +
    geom_vline(xintercept = 0) +
    geom_density(aes(x= CI_sat.POS*10^8), color= "red", size= 1.5)+
    geom_density(aes(x= CI_field.mean.POS*10^8), color= "blue", size= 1.5)+
    annotate("text", x= 300000, y= 2e-06, label= "Satellite CI", color= "red", hjust= 0) +
    annotate("text", x= 300000, y= 1.8e-06, label= "Field CI", color= "blue", hjust= 0) +
    labs(x= "CI cells / mL", y= "Density") +
    #scale_fill_discrete(name= "Waterbody") +
    scale_y_continuous(expand= c(0, 0), labels= NULL) +
    scale_x_continuous(limits= c(-500000, 3250000), expand= c(0, 0)) +
    theme_sat
  
  
  ggsave(last_plot(), filename= "cells_mL_density.png", height= 4, width= 6, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  
  
  
  
  ## Standardized major axis regression library(smatr)
  ## And OLS regression
  sma.fit1 <- sma(CI_field.mean ~ CI_sat, ci_fs_pix.sum)
  sma.fit2 <- sma(CI_field.mean.POS ~ CI_sat.POS, ci_fs_pix.sum)
  #summary(sma.fit1)
  
  lm.fit1 <- lm(CI_field.mean ~ CI_sat, ci_fs_pix.sum)
  #summary(lm.fit1)
  
  lm.fit2 <- lm(CI_sat ~ CI_field.mean, ci_fs_pix.sum)
  #summary(lm.fit2)
  
  lm.fit3 <- lm(CI_sat.POS ~ CI_field.mean.POS, ci_fs_pix.sum)
  #summary(lm.fit2)
  
  
  sma_annotation <- "Standardized major axis regression\ny = 1.18*x - 0.0007\nr2= 0.63"
  
  CIF_CIS_sma.plot <- ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
        geom_point(aes(x= CI_sat, y= CI_field.mean, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CI_sat, 
                      ymin= CI_field.mean - 1*CI_field.se, 
                      ymax= CI_field.mean + 1*CI_field.se)) +
    geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
                slope= sma.fit1$coef[[1]][2, 1],
                size= 1) +
    #annotate("text", x= 0.0001, y= 0.0045, label= sma_annotation, hjust= 0, size= 3) +
    # geom_abline(intercept= lm.fit1$coefficients[1],
    #             slope= lm.fit1$coefficients[2],
    #             size= 1, color= "red") +
    labs(x= "Satellite CI", y= "Field CI (\u00B1 SE)") +
    scale_fill_discrete(name= "Waterbody") +
    scale_x_continuous(limits= c(-0.006, 0.018), breaks= seq(-0.005, 0.015, by= 0.005), expand= c(0, 0)) +
    scale_y_continuous(limits= c(-0.002, 0.033), breaks= seq(0, 0.03, by= 0.005), expand= c(0, 0)) +
    scale_color_discrete(name= "Waterbody_Date") +
    coord_equal() +
    theme_sat
  CIF_CIS_sma.plot
  
  ggsave(CIF_CIS_sma.plot, filename= "CIF_CIS_sma.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  
CIF_CIS.sum.plot <-   ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
    #             slope= sma.fit1$coef[[1]][2, 1],
    #             size= 1) +
    geom_point(aes(x= CI_sat, y= CI_field.mean, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CI_sat, 
                      ymin= CI_field.mean - 1.96*CI_field.se, 
                      ymax= CI_field.mean + 1.96*CI_field.se)) +
    # annotate("text", x= 0.0001, y= 0.0045, label= sma_annotation, hjust= 0, size= 3) +
    # geom_abline(intercept= lm.fit1$coefficients[1],
    #             slope= lm.fit1$coefficients[2],
    #             size= 1, color= "red") +
    labs(x= "Satellite CI", y= "Field CI (\u00B1 1.96*SE)") +
    scale_fill_discrete(name= "Waterbody") +
    #scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
    #scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
    scale_color_discrete(name= "Waterbody_Date") +
    coord_equal() +
    theme_sat
  CIF_CIS.sum.plot
  
  ggsave(CIF_CIS.sum.plot, filename= "CIF_CIS_sum.png", height= 6, width= 7, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  CIF_CIS.sum.facet.plot <-   ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
    #             slope= sma.fit1$coef[[1]][2, 1],
    #             size= 1) +
    geom_point(aes(x= CI_sat, y= CI_field.mean, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CI_sat, 
                      ymin= CI_field.mean - 1.96*CI_field.se, 
                      ymax= CI_field.mean + 1.96*CI_field.se)) +
    # annotate("text", x= 0.0001, y= 0.0045, label= sma_annotation, hjust= 0, size= 3) +
    # geom_abline(intercept= lm.fit1$coefficients[1],
    #             slope= lm.fit1$coefficients[2],
    #             size= 1, color= "red") +
    labs(x= "Satellite CI", y= "Field CI (\u00B1 1.96*SE)") +
    scale_fill_discrete(name= "Waterbody") +
    #scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
    #scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
    scale_color_discrete(name= "Waterbody_Date") +
    facet_wrap(~waterbody, ncol= 2, scales= "free_y", labeller= as_labeller(waterbody_labeller)) +
    #coord_equal() +
    theme_sat
  CIF_CIS.sum.facet.plot
  ggsave(CIF_CIS.sum.facet.plot, filename= "CIF_CIS_sum_facet.png", height= 6, width= 8, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  
  
  CIcyanoF_CIcyanoS.sum.plot <-  ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
    #             slope= sma.fit1$coef[[1]][2, 1],
    #             size= 1) +
    geom_point(aes(x= CIcyano_sat, y= CIcyano_field.mean, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CIcyano_sat, 
                      ymin= CI_field.mean - CI_field.se, 
                      ymax= CI_field.mean + CI_field.se)) +
    labs(x= "Satellite CIcyano", y= "Field CIcyano (\u00B1 SE)") +
    scale_x_continuous(limits= c(-0.006, 0.018), breaks= seq(-0.005, 0.015, by= 0.005), expand= c(0, 0)) +
    scale_y_continuous(limits= c(-0.002, 0.033), breaks= seq(0, 0.03, by= 0.005), expand= c(0, 0)) +
    scale_fill_discrete(name= "Waterbody") +
    scale_color_discrete(name= "Waterbody_Date") +
    coord_equal() +
    theme_sat
  CIcyanoF_CIcyanoS.sum.plot
  
  ggsave(CIcyanoF_CIcyanoS.sum.plot, filename= "CIcyanoF_CIcyanoS_sum.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  
  CIF_CIcyanoS.sum.plot <-  ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
    #             slope= sma.fit1$coef[[1]][2, 1],
    #             size= 1) +
    geom_point(aes(x= CIcyano_sat, y= CI_field.mean, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CIcyano_sat, 
                      ymin= CI_field.mean - CI_field.se, 
                      ymax= CI_field.mean + CI_field.se)) +
    labs(x= "Satellite CIcyano", y= "Field CI (\u00B1 SE)") +
    scale_x_continuous(limits= c(-0.006, 0.018), breaks= seq(-0.005, 0.015, by= 0.005), expand= c(0, 0)) +
    scale_y_continuous(limits= c(-0.002, 0.033), breaks= seq(0, 0.03, by= 0.005), expand= c(0, 0)) +
    scale_fill_discrete(name= "Waterbody") +
    scale_color_discrete(name= "Waterbody_Date") +
    coord_equal() +
    theme_sat
CIF_CIcyanoS.sum.plot

ggsave(CIcyanoF_CIcyanoS.sum.plot, filename= "CIF_CIcyanoS_sum.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")

  
  ## Root mean square error
  rmse.fit1 <- Metrics::rmse(ci_fs_pix.sum$CI_field.mean, predict(lm.fit1))
  rmse.fit1/mean(ci_fs_pix.sum$CI_field.mean) # return % estimate error.
  
  rmse.fit2 <- Metrics::rmse(ci_fs_pix.sum$CI_sat, predict(lm.fit2))
  rmse.fit2/mean(ci_fs_pix.sum$CI_sat) # return % estimate error.
  

  rmse.fit3 <- Metrics::rmse(ci_fs_pix.sum$CI_sat.POS, predict(lm.fit3))
  rmse.fit3/mean(ci_fs_pix.sum$CI_sat.POS) # return % estimate error.
  


