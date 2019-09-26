## Script to collect and format the remote sensed reflectance data
## generated from the NOAA EXE program

## The files with the rrs are named "Seabass"
## rrs= remote sensed reflectance

library(tidyverse)


## Pathway to output of NOAA EXE program
base_dir <- "Data/20190801_LakeSanAntonio/noaa_files"
rrs_dir <- "Data/rrs_data"
sample_dirs <- list.dirs(base_dir)[-1]



## Copy and rename seabass files and place in a single directory
copy_rrs_files <- function(in_dir, out_dir, sample_name){
  
  # Copy file
  rrs_file <- list.files(in_dir, pattern= "seabass")
  file.copy(from= file.path(in_dir, rrs_file), to= out_dir)
  
  # Rename file
  new_name <- str_c("rrs", '-', sample_name, "-", str_replace(sample_dirs[1], "^.*/", ""), ".txt", sep= "")
  file.rename(from= file.path(out_dir, rrs_file),
              to= file.path(out_dir, new_name))
  
}


map(sample_dirs, function(x) copy_rrs_files(in_dir = x, 
                                            out_dir= rrs_dir,
                                            sample_name= "LakeSanAntonio_20190801"))


