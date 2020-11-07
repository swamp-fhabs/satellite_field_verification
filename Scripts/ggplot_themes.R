

#### GGPLOT THEMES ############################
theme_sat <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= "transparent", color= "black", linetype= "solid", size= 0.5),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black", size= 14),
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
