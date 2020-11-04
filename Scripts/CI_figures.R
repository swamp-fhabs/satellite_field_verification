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
ci_fs <- read_tsv("Data/CI_field_sat_h2o_data.tsv") 

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


## CIcyano results
# ci_fs_mod %>% 
#   count(ss665_threshold)
# summary(ci_fs$ss665)



#make_spectral_shape_plots <- function(df, sampID, out_dir){
  
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
  summarize(n= length(CI_field),
            mean_CI_field= mean(CI_field, na.rm= TRUE),
            sd_CI_field= sd(CI_field, na.rm= TRUE),
            cv_CI_field= sd_CI_field / mean_CI_field) %>% 
  ungroup()

coef_variation_pixel <- ci_fs %>% 
  group_by(waterbody, pixel) %>% 
  summarize(n= length(CI_field),
            mean_CI_field= mean(CI_field, na.rm= TRUE),
            sd_CI_field= sd(CI_field, na.rm= TRUE),
            cv_CI_field= sd_CI_field / mean_CI_field) %>% 
  ungroup()


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

#### MAKE FIGURES ####################################################

## ss(665) Histogram
ggplot(data= ci_fs, aes(x= ss665_field)) +
  geom_histogram(binwidth= 0.0001, 
                 boundary= 1,
                 fill= "black",
                 color= "gray50") +
  labs(x= "SS(665)", y= "Count") +
  scale_y_continuous(expand= c(0,0)) +
  scale_x_continuous(limits= c(-0.003, 0),
                     breaks= seq(-0.003, 0, by= 0.0005),
                     labels= c("-0.003", "", "-0.002", "", "-0.001", "", "0"),
                     expand= c(0, 0)) +
  facet_rep_wrap(~waterbody) + 
  theme_sat
ggsave(last_plot(), filename= "ss665_hist.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## SS(665) X WATERBODY
ggplot(data= ci_fs, aes(x= pixel, y= ss665_field)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(fill= "gray75") +
  labs(x= "Measurement location", y= "SS(665)") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  # facet_grid(.~waterbody, scales= "free_x") +
  facet_wrap(~waterbody, ncol= 3, scales= "free_x", labeller= as_labeller(waterbody_labeller)) +
  theme_sat +
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

ggsave(last_plot(), filename= "ss665F_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## CI X WATERBODY
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x= pix_site, y= CI_field), fill= "gray75") +
  labs(x= "Measurement location", y= "Field CI") +
  scale_fill_discrete(name= "Waterbody") +
  scale_y_continuous(limits= c(-0.001, 0.0041), expand= c(0.02, 0)) +
  facet_wrap(.~waterbody, ncol= 6, scales= "free_x", labeller= as_labeller(waterbody_labeller)) +
  theme_sat +
  theme(axis.text.x= element_text(angle= 90, size= 8, vjust= 0.5),
        strip.text = element_text(size= 8))

ggsave(last_plot(), filename= "CIF_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## CI X CHLA
#summary(lm(CI_field.mean ~ chla_ugL, data= ci_fs_pixsite.sum))
ggplot(data= ci_fs_pixsite.sum) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(x= chla_ugL, 
                    ymin= CI_field.mean - 1.96*CI_field.se, 
                    ymax= CI_field.mean + 1.96*CI_field.se)) +
  geom_point(aes(x= chla_ugL, y= CI_field.mean, fill= waterbody), size= 2, shape= 21) +
  geom_abline(intercept= lm(CI_field.mean ~ chla_ugL, data= ci_fs_pixsite.sum)$coefficients[1], 
              slope= lm(CI_field.mean ~ chla_ugL, data= ci_fs_pixsite.sum)$coefficients[2], 
              size= 1) +
  labs(x= expression(paste("Chlorophyll-a (",mu,"g/L)")), y= "Field CI (\u00B1 1.96*SE) ") +
  scale_x_continuous(limits= c(0, 50), expand= c(0.01, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  theme_sat

ggsave(last_plot(), filename= "CIF_chla.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


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
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CIF_CIS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")

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
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_abline(intercept= sma.fit1$coef[[1]][1, 1],
              slope= sma.fit1$coef[[1]][2, 1],
              size= 1) +
  geom_point(aes(x= CI_sat, y= CI_field.mean, color= waterbody), size= 3) +
  geom_errorbar(aes(x= CI_sat, 
                    ymin= CI_field.mean - 1.96*CI_field.se, 
                    ymax= CI_field.mean + 1.96*CI_field.se)) +
  annotate("text", x= 0.0001, y= 0.0045, label= sma_annotation, hjust= 0, size= 3) +
  # geom_abline(intercept= lm.fit1$coefficients[1],
  #             slope= lm.fit1$coefficients[2],
  #             size= 1, color= "red") +
  labs(x= "Satellite CI", y= "Field CI (\u00B1 1.96*SE)") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_color_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat
CIF_CIS_sma.plot

ggsave(CIF_CIS_sma.plot, filename= "CIF_CIS_sma.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


CIF_CIS.plot <- ggplot(data= ci_fs_pix.sum) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
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
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_color_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat
CIF_CIS.plot

ggsave(CIF_CIS.plot, filename= "CIF_CIS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")




## Root mean square error
rmse.fit1 <- Metrics::rmse(ci_fs_pix.sum$CI_field.mean, predict(lm.fit1))
rmse.fit1/mean(ci_fs_pix.sum$CI_field.mean) # return % estimate error.

rmse.fit2 <- Metrics::rmse(ci_fs_pix.sum$CI_sat, predict(lm.fit2))
rmse.fit2/mean(ci_fs_pix.sum$CI_sat) # return % estimate error.


## Field CI X SAT CIcyano
CIF_CIcyanoS.plot <- ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CIcyano_sat, y= CI_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[cyano]), y= "Field CI") +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CIF_CIcyanoS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## Field CIcyano X SAT CIcyano
ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 6.309573e-05) +
  geom_vline(xintercept = 6.309573e-05) +
  geom_point(aes(x= CIcyano_sat, y= CIcyano_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[cyano]), y= expression(Field~CI[cyano])) +
  scale_x_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.01, 0)) +
  scale_y_continuous(limits= ci_lims, breaks= ci_brks, labels= ci_labels, expand= c(0.02, 0)) +
  scale_fill_discrete(name= "Waterbody_Date") +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CIcyanoF_CIcyanoS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


#### COMBINED PLOTS
waterbody_date_legend <- get_legend(CIF_CIS.plot +guides(fill = guide_legend(ncol=2)) + theme(legend.background= element_rect(color= "transparent")))

CIF_CIS_combined <- plot_grid(CIF_CIcyanoS.plot + theme(legend.position= "none"), 
                         CIF_CIS.plot + theme(legend.position= "none"), 
                         CIF_CIS_sma.plot+ theme(legend.position= "none"),
                         ncol= 2,
                         labels= c("A", "B", "C"),
                         rel_widths = c(1, 1))
CIF_CIS_combined_legend <- plot_grid(CIF_CIS_combined, waterbody_date_legend,
                                     nrow= 2,
                                     rel_heights = c(1, 0.5))

CIF_CIS_combined_legend
ggsave(CIF_CIS_combined, filename= "CIF_CIS_combined.png", height= 6.5, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


library(ggplot2)
theme_set(theme_half_open())

p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line()
plot.mpg <- ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + geom_point(size=2.5)
# Note that these cannot be aligned vertically due to the legend in the plot.mpg
ggdraw(plot_grid(p1, plot.mpg, ncol=1, align='v'))

legend <- get_legend(plot.mpg)
plot.mpg <- plot.mpg + theme(legend.position='none')
# Now plots are aligned vertically with the legend to the right
ggdraw(plot_grid(plot_grid(p1, plot.mpg, ncol=1, align='v'),
                 plot_grid(NULL, legend, ncol=1),
                 rel_widths=c(1, 0.2)))


## CIcyano_sat X CHLA
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= chla_ugL, y= CIcyano_sat, fill= waterbody), size= 3, shape= 21) +
  #labs(x= expression(paste("Chlorophyll-a (",mu,"g/L)")), y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  #scale_x_continuous(expand= c(0.02, 0)) +
  #scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  theme_sat

# ggsave(last_plot(), filename= "ci_mod_chla.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
#        path= "Data/Figures_output")




#### OLD PLOTS ######################################################################################

## CI-mod X CHLA
ggplot(data= ci_fs) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x= chla_ugL, y= CI_mod_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(paste("Chlorophyll-a (",mu,"g/L)")), y= "Field CI-mod") +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(expand= c(0.02, 0)) +
  scale_y_continuous(limits= c(-10, 60), breaks= seq(-10, 60, by= 10), expand= c(0.02, 0)) +
  theme_sat

ggsave(last_plot(), filename= "ci_mod_chla.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## Field CI-mod X SAT CIcyano-mod
ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point(aes(x= CIcyano_mod_sat, y= CI_mod_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[cyano-mod]~value), y= expression(Field~CI[mod]~value)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CImodF_CIcyanomodS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")


## Sat CI X SAT CIcyano
ci_lims <- c(6.309573e-05, 0.005)
ci_brks <- c(6.309573e-05, seq(0.001, 0.005, by= 0.001))
ci_labels <- c("DL", "0.001", "0.002", "0.003", "0.004", "0.005")

ggplot(data= ci_fs) +
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

ggsave(last_plot(), filename= "CIS_CIS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")




## Field CI-mod X SAT CI-mod
cimod_lims <- c(-10, 80)
cimod_brks <- c(-10, 1, seq(10, 80, by= 10))
cimod_labels <- c("-10", "DL", "10", "20", "30", "40", "50", "60", "70", "80")

ggplot(data= ci_fs) +
  geom_abline(aes(slope= 1, intercept= 0), linetype= "dashed", color= "gray50") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point(aes(x= CI_mod_sat, y= CI_mod_field, fill= waterbody), size= 3, shape= 21) +
  labs(x= expression(Satellite~CI[mod]~value), y= expression(Field~CI[mod]~value)) +
  scale_fill_discrete(name= "Waterbody") +
  scale_x_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  scale_y_continuous(limits= cimod_lims, breaks= cimod_brks, labels= cimod_labels, expand= c(0.02, 0)) +
  coord_equal() +
  theme_sat

ggsave(last_plot(), filename= "CImodF_CImodS.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
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

## CI-mod field X WATERBODY

ggplot(data= ci_fs, aes(x= pixel, y= CI_mod_field)) +
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

ggsave(last_plot(), filename= "ci_modF_wbd.png", height= 4.875, width= 6.5, units= "in", dpi= 300,
       path= "Data/Figures_output")



