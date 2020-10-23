## Script to plot the rrs and CI values
## OLCI bands (nanometers, nm) 620, 665, 681, and 709

## TXT files generated in calc_CI_and_format.R script. Run this script first

#### Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(extrafont)
library(smatr)


## Read in CI and rrs data
ci_fs <- read_tsv("Data/CI_field_sat_NOAA_tiff.tsv") 

## Summarize over pixels
ci_fs_pix.sum <- ci_fs %>% 
  group_by(waterbody, pixel) %>% 
  summarize(n= length(pixel),
            across(c(contains("field"), chla_ugL), list(mean= mean, sd= sd, se= ~sd(.x)/sqrt(n)), .names= "{col}.{fn}")) %>% 
  ungroup() %>% 
  left_join(., select(ci_fs, waterbody, pixel, contains("sat")), by= c("waterbody", "pixel")) %>% 
  distinct()


ci_fs_pixsite.sum <- ci_fs %>% 
  group_by(waterbody, pix_site) %>% 
  summarize(n= length(pix_site),
            across(contains("field"), list(mean= mean, sd= sd, se= ~sd(.x)/sqrt(n)), .names= "{col}.{fn}")) %>% 
  ungroup() %>% 
  left_join(., select(ci_fs, waterbody, pix_site, chla_ugL, secchi_avg, contains("sat")), by= c("waterbody", "pix_site")) %>% 
  distinct()

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
                        "LakeAlmanor_20190815" ="Lake Almanor\n2019-08-15",
                        "LakeSanAntonio_20190801" = "L. San Antonio\n2019-08-01",
                        "SanPabloReservoir_20190812" = "San Pablo Res.\n2019-08-12")

#### MAKE FIGURES ####

## ss(665) Histogram
ggplot(data= ci_fs, aes(x= ss665_field)) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black",
                 color= "gray50") +
  labs(x= "Field SS(665)", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  scale_x_continuous(limits= c(-0.003, 0),
                     breaks= seq(-0.003, 0, by= 0.0005),
                     labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
                     expand= c(0, 0)) +
  theme_sat
ggsave(last_plot(), filename= "ss665_field_hist.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output_NOAA_tiff")

ggplot(data= ci_fs, aes(x= ss665_sat)) +
  geom_vline(xintercept= 0, size= 1) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black",
                 color= "gray50") +
  labs(x= "Satellite SS(665)", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  #scale_x_continuous(limits= c(-0.003, 0),
  #                   breaks= seq(-0.003, 0, by= 0.0005),
  #                   labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
  #                   expand= c(0, 0)) +
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
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat





## Field CI X SAT CI

ci_lims <- c(-.0005, 0.005)
ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")


CIF_CIS.plot <- ggplot(data= ci_fs) +
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



