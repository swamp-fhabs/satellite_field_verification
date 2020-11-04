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


## Read in CI and rrs data
ci_fs <- read_tsv("Data/CI_field_sat_NOAA_tiff.tsv") 

## Summarize over pixels
ci_fs_pix.sum <- ci_fs %>% 
  group_by(waterbody, pixel) %>% 
  summarize(n= length(pixel),
            across(c(contains("field")), list(mean= mean, sd= sd, se= ~sd(.x)/sqrt(n)), .names= "{col}.{fn}"),
            .groups= "keep") %>% 
  #ungroup() %>% 
  left_join(., select(ci_fs, waterbody, pixel, contains("sat")), by= c("waterbody", "pixel")) %>% 
  distinct() %>% 
  mutate(CI_field.mean.POS= ifelse(CI_field.mean < 0, 0, CI_field.mean),
         CI_sat.POS= ifelse(CI_sat < 0, 0, CI_sat))







#### GGPLOT THEMES ############################
theme_sat <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= "transparent", color= "black", linetype= "solid", size= 0.5),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black", size= 12),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   #axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "right")


waterbody_labeller <- c("ClearLake_20190807" = "Clear Lake\n2019-08-07",
                        "ClearLake_20190816" = "Clear Lake\n2019-08-16",
                        "ClearLake_20191008" = "Clear Lake\n2019-10-08",
                        "ClearLake_20200708" = "Clear Lake\n2020-07-08",
                        "ClearLake_20200724" = "Clear Lake\n2020-07-24",
                        "LakeAlmanor_20190815" ="Lake Almanor\n2019-08-15",
                        "LakeSanAntonio_20190801" = "L. San Antonio\n2019-08-01",
                        "SanPabloReservoir_20190812" = "San Pablo Res.\n2019-08-12")

#### MAKE FIGURES ####


## ss(665) Histogram
ggplot(data= ci_fs, aes(x= ss665_field)) +
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

ggplot(data= ci_fs, aes(x= ss665_sat)) +
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
ggplot(data= ci_fs) +
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





## Field CI X SAT CI

ci_lims <- c(-.0005, 0.005)
ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")


CIF_CIS.plot <- ggplot(data= ci_fs2) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CI_sat, y= CI_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= "Satellite CI", y= "Field CI") +
#  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
#  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat

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
    labs(x= "Satellite CI", y= "Field CI (\u00B1 SE)") +
    scale_fill_discrete(name= "Waterbody") +
    #scale_y_log10() +
    #scale_x_log10() +
    #annotation_logticks() +
    scale_color_discrete(name= "Waterbody_Date") +
    coord_equal() +
    theme_sat
  
  
  ggsave(last_plot(), filename= "CIF_CIS_cellsmL.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
         path= "Data/Figures_output_NOAA_tiff")
  
  
  
  
  ## Standardized major axis regression library(smatr)
  ## And OLS regression
  sma.fit1 <- sma(CI_field.mean ~ CI_sat, ci_fs_pix.sum)
  #summary(sma.fit1)
  
  lm.fit1 <- lm(CI_field.mean ~ CI_sat, ci_fs_pix.sum)
  #summary(lm.fit1)
  
  lm.fit2 <- lm(CI_sat ~ CI_field.mean, ci_fs_pix.sum)
  #summary(lm.fit2)
  
  sma_annotation <- "Standardized major axis regression\ny = 1.18*x - 0.0007\nr2= 0.63"
  
  CIF_CIS_sma.plot <- ggplot(data= ci_fs_pix.sum) +
    geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
        geom_point(aes(x= CI_sat*10^8, y= CI_field.mean*10^8, color= waterbody), size= 3) +
    geom_errorbar(aes(x= CI_sat*10^8, 
                      ymin= (CI_field.mean - 1*CI_field.se)*10^8, 
                      ymax= (CI_field.mean + 1*CI_field.se)*10^8)) +
    geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
                slope= sma.fit1$coef[[1]][2, 1],
                size= 1) +
    #annotate("text", x= 0.0001, y= 0.0045, label= sma_annotation, hjust= 0, size= 3) +
    # geom_abline(intercept= lm.fit1$coefficients[1],
    #             slope= lm.fit1$coefficients[2],
    #             size= 1, color= "red") +
    labs(x= "Satellite CI", y= "Field CI (\u00B1 SE)") +
    scale_fill_discrete(name= "Waterbody") +
    #scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
    #scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
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
  
  
  
  
  ## Root mean square error
  rmse.fit1 <- Metrics::rmse(ci_fs_pix.sum$CI_field.mean, predict(lm.fit1))
  rmse.fit1/mean(ci_fs_pix.sum$CI_field.mean) # return % estimate error.
  
  rmse.fit2 <- Metrics::rmse(ci_fs_pix.sum$CI_sat, predict(lm.fit2))
  rmse.fit2/mean(ci_fs_pix.sum$CI_sat) # return % estimate error.
  



