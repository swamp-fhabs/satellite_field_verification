


#### LIBRARIES
library(tidyverse)



## READ IN DATA
# note Windows file encoding
hh2 <- read.delim("./Data/20190705_135521/20190705_processed.dat", 
                   sep= "\t", 
                   header= FALSE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "UCS-2LE") %>%
  select(-V2) %>% 
  rename(file= V1, nm= V3, value= V4) %>% 
  as_tibble()

meta <- read_tsv("./Data/20190705_135521/20190705_metadata.txt")


hh2.filt <- hh2 %>% 
  filter(nm == 665 | nm == 681 | nm == 709) %>% 
  mutate(file_num= str_replace_all(file, "[^0-9]", ""))
hh2.filt






hh2.filt.w <- hh2.filt %>% 
  spread(nm, value) %>% 
  rename(nm665= `665`, nm681= `681`, nm709= `709`)

# From Wynne et al. 2008
calc_CI <- function(df){
  CI= with(df, -(nm681 - nm665 - (nm709 - nm665)*((681-665)/(709-665))) )
}

test <- hh2.filt.w %>% 
  filter(file == "Spec00001.asd.rad")



ci.test <- calc_CI(test)
ci.test
