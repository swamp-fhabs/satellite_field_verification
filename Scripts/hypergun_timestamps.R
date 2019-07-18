## Script to extract time stamps from HyperGun data files from Calaveras Reservoir 2018
library(tidyverse)

## LIST SAMPLE FOLDERS
sample_folders <- list.files("Data", pattern= "Calaveras*", full.names = TRUE)


## LOOP OVER ALL THE SAMPLE FOLDERS TO EXTRACT TIMING DATA
time_list <- map(sample_folders, function(folder){
  
  ## LIST HG FILES FOR PLATE, WATER, AND SKY
  hg_data <- list.files(folder, pattern = "^(water|sky|plate).*txt") 
  
  ## FUNCTION TO EXTRACT TIME STAMP FROM SPECIFIC HG FILES
  time_stamp <- map_chr(hg_data,
                        function(filename){
                          suppressMessages(read_delim(file.path(folder, filename),
                                                      delim= "\t",
                                                      col_names = FALSE,
                                                      skip= 4,
                                                      n_max = 1)) %>%
                            mutate(X3= as.character(X3)) %>%
                            select(X3) %>%
                            pull()
                        }
  )
  ## GET TYPE OF MEASUREMENT: PLATE, WATER, SKY
  type <- str_replace(hg_data, "_.*$", "")
  
  ## COMBINE INTO AN OUTPUT DATA FRAME
  hg_timings <- tibble(sample= str_replace(folder, "Data/", ""),
                       type, 
                       time_stamp) %>% 
    arrange(time_stamp)
  return(hg_timings)
})

write_tsv(do.call(rbind, time_list), path= "Data/hypergun_Calaveras_timestamps.tsv")


