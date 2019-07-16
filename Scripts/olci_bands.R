## Script to extract OLCI bands from HH2 data.


extract_olci_bands <- function(spec){
  require(tidyverse)
  
  ## OLCI BANDS
  olci <- data.frame(band= c("b07_620", "b08_665", "b10_681", "b11_709"),
                     center=  c(620, 665, 681, 709),
                     width= c(10, 10, 7.5, 10),
                     stringsAsFactors = FALSE) %>% 
    mutate(min= center - width,
           max= center + width)
  
  
  ## FILTER SPECTRA BY OLCI BANDS
  spec_olci_bands <- do.call(rbind, apply(olci, 1, 
                                          function(x){
                                            spec %>% 
                                              filter(nm >= x["min"] & nm <= x["max"]) %>% 
                                              mutate(band= x["band"])
                                          }))
  
  ## AVERAGE OLCI BAND WIDTHS TOGETHER
  spec_olci_means <- spec_olci_bands %>% 
    group_by(spec_file, band) %>% 
    summarize(band_radiance= mean(radiance, na.rm= TRUE),
              band_sd= sd(radiance, na.rm= TRUE))
  return(spec_olci_means)
  
}




