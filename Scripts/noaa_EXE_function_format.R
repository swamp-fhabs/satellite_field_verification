## Script to write a batch file to run the noaa EXE program
## on each set of sample files

library(tidyverse)


## Specify pathways
base_dir <- "Data/20190801_LakeSanAntonio"
noaa_out_dir <- file.path(base_dir, "noaa_files")
ascii_dir <- file.path(base_dir, "ascii_export")
noaa_file_dirs <- list.dirs(noaa_out_dir, full.names= FALSE)[-1]
t.dir <- file.path(base_dir, "aatest")


## Copy files from ascii folder to sample folders (sample_dir)
# Uses variables noaa_out_dir and ascii_dir in the function
copy_ascii_files <- function(base_dir, samp_dir, ascii_dir, out_dir){

  # Read file and extract the ascii file names
  ascii_files <- read_delim(file.path(base_dir, samp_dir), 
                          delim= " ", 
                          col_names= FALSE) %>% 
    select(X3) %>% 
    pull()
  
  # Copy ascii files into the sample folder
  file.copy(from= file.path(ascii_dir, ascii_files), to= file.path(out_dir, samp_dir))
}
sapply(noaa_file_dirs, function(x) copy_ascii_files(samp_dir= x,
                                                   base_dir= noaa_out_dir,
                                                   ascii_dir = ascii_dir,
                                                   out_dir = noaa_out_dir))



### Write the NOAA EXE batch file
write_batch_file <- function(samp_dir, base_path, exe_path, cal_path,  batch_name, out_dir){
  require(tidyverse)
  
  ## Change directory to where ascii files are located
  dir_path <- str_c(base_path, "\\", samp_dir)
  change_dir <- str_c("cd", dir_path, sep= " ")
  
  ## Run the EXE file
  #command_text <- str_c(exe_path, "--cal", cal_path, "--file", str_c("..\\", samp_file), sep= " ")
  command_text <- str_c(exe_path, "--cal", cal_path, "--file", str_c(samp_dir, ".txt"), sep= " ")
  
  
  write_lines(change_dir, path= file.path(out_dir, str_c(batch_name, ".txt")), append= TRUE)
  write_lines(command_text, path= file.path(out_dir, str_c(batch_name, ".txt")), append= TRUE)
  write_lines("", path= file.path(out_dir, str_c(batch_name, ".txt")), append= TRUE)
}



## Run write_batch_file in all sample directories
exe_file <- "C:\\Users\\KBouma-Gregson\\Documents\\Satellite_CI_index\\satellite_field_verification_git\\Data\\ASD_processing\\test_asd_group.exe"
cal_file <- "C:\\Users\\KBouma-Gregson\\Documents\\Satellite_CI_index\\satellite_field_verification_git\\Data\\ASD_processing\\Raphe10%_Spectralon_10AA01-0517-8337.txt"
basePath <- "C:\\Users\\KBouma-Gregson\\Documents\\Satellite_CI_index\\satellite_field_verification_git\\Data\\20190801_LakeSanAntonio\\noaa_files"



sapply(dirs, function(x){ write_batch_file(samp_dir = x,
                                           base_path= basePath,
                                           exe_path = exe_file, 
                                           cal_path = cal_file, 
                                           batch_name = "batch_test3", 
                                           out_dir = t.dir)
})




# ## Make directories from the file names
# files <- list.files(t.dir, pattern= "*.txt")
# files_full <- list.files(t.dir, pattern= "*.txt", full.names= TRUE)
# dirs_full <- list.dirs(t.dir, full.names= TRUE)
# dirs <- list.dirs(noaa_out_dir, full.names= FALSE)[-1]
#dir_names <- str_replace(files, ".txt", "")

# Create new directories with sapply()
#sapply(file.path(t.dir, dir_names), dir.create)






