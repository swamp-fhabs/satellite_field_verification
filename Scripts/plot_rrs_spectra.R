library(ggplot2)
library(tidyverse)

sample_name <- "ClearLake_20190807-P2S1_1"

  
  
rrs_spec <- read_csv(str_c("Data/rrs_data/rrs-", sample_name, ".txt"), skip= 31, col_names = FALSE) %>% 
  rename(nm= X1, rrs= X2)

# rrs_spec <- read_csv("Data/rrs_data/rrs-LakeAlmanor_20190815-P3S1_1.txt", skip= 31, col_names = FALSE) %>% 
#   rename(nm= X1, rrs= X2)

# rrs_spec <- read_csv("Data/rrs_data/rrs-LakeSanAntonio_20190801-P3S2_1.txt", skip= 31, col_names = FALSE) %>% 
#   rename(nm= X1, rrs= X2)


ggplot(data= rrs_spec) +
  geom_vline(xintercept = c(620, 665, 681, 709), linetype= "dashed", size= 0.5) +
  scale_x_continuous(expand= c(0, 0), 
                     breaks= seq(350, 900, by= 50), 
                     labels= c("", "400", "", "500", "", "600", "", "700", "", "800", "", "900")) +
  scale_y_continuous(expand= c(0.02, 0)) +
  geom_line(aes(x= nm, y= rrs), size= 1) +
  labs(x= "nm", y= "Field Rrs", title= sample_name) +
  theme_classic(base_size = 18)
  