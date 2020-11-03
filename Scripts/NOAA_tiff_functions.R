

#### FORMAT FUNCTIONS ######################################################

read_noaa_tiff <- function(tif.file){
  require(stringr)
  #multiband raster R
  #https://www.neonscience.org/dc-multiband-rasters-r
  
  # Tiff band labels (tags) provided by Shelly Tomlinson of NOAA through email on 2-Jul-2020
  tiff.tags <- c("Rrs_400", "Rrs_412", "Rrs_442", "Rrs_490", "Rrs_510", "Rrs_560", "Rrs_620", "Rrs_665", "Rrs_674", "Rrs_681", "Rrs_709", "Rrs_754", "Rrs_761", "Rrs_764", "Rrs_768", "Rrs_779", "Rrs_865", "Rrs_885", "Rrs_900", "Rrs_940", "Rrs_1012", "rhos_400", "rhos_412", "rhos_442", "rhos_490", "rhos_510", "rhos_560", "rhos_620", "rhos_665", "rhos_674", "rhos_681", "rhos_709", "rhos_754", "rhos_761", "rhos_764", "rhos_768", "rhos_779", "rhos_865", "rhos_885", "rhos_900", "rhos_940", "rhos_1012", "chl_oc4", "cloud_albedo", "hab_l2_flags")
  #tiff.tags[c(28, 29, 31, 32)] # bandds for SS(665) and SS(681)
  
  ## Read in raster file
  tif <- raster::stack(tif.file,
                       bands= c(28, 29, 31, 32))
  #names(tif) <- str_c(tiff.tags[c(28, 29, 31, 32)], names(tif), sep= ".")
  names(tif) <- tiff.tags[c(28, 29, 31, 32)]
  
  ## Transform multi-band raster stack into a matrix
  tif.mat <- raster::rasterToPoints(tif)
  return(tif.mat)  
}

extract_lake_pixels <- function(tif.matrix, lake.id, utm){
  require(sf)
  require(tidyverse)
  
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  # Read CA lakes shapefile
  lake.extract <- st_read("Data/LakeMaps/CA_Lakes") %>% 
    # extract lake of interest
    filter(., DFGWATERID == lake.id) %>% 
    # Transform to utm  
    st_transform(., crs= utm.crs)
  
  
  
  ## EXTRACT LAKE FROM NOAA TIF MATRIX
  pix.center.sf <- as_tibble(tif.matrix) %>% 
    filter(., 
           x > st_bbox(lake.extract)[1] - 300, 
           y > st_bbox(lake.extract)[2] - 300, 
           x < st_bbox(lake.extract)[3] + 300, 
           y < st_bbox(lake.extract)[4] + 300) %>% 
    st_as_sf(.,
             coords= c("x", "y"),
             crs= utm.crs) %>% 
    mutate(pix_FID= str_pad(as.character(seq(0:(nrow(.)-1))), width=2, pad= "0"))
  
  return(pix.center.sf)
}

find_sampling_pixels <- function(pix.center.sf, field.lat.long, utm){
  require(tidyverse)
  require(sf)
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  
  ## FIELD DATA
  
  ## Read lat long of radiometer sites
  field.sf.utm <- read_tsv(field.lat.long) %>% 
    # Make spatial feature object
    st_as_sf(.,
             coords= c("long", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= utm.crs)
  
  
  ## SATELLITE DATA
  # pix.center.sf <- as_tibble(tif.matrix) %>% 
  #   filter(., 
  #          x > st_bbox(field.sf.utm)[1] - 300, 
  #          y > st_bbox(field.sf.utm)[2] - 300, 
  #          x < st_bbox(field.sf.utm)[3] + 300, 
  #          y < st_bbox(field.sf.utm)[4] + 300) %>% 
  #   st_as_sf(.,
  #            coords= c("x", "y"),
  #            crs= utm.crs) %>% 
  #   mutate(pixID= str_pad(as.character(seq(1:nrow(.))), width=2, pad= "0"))
  # Identify pixel boundaries (150 meter square buffers)
  pix.buffer.sf <- st_buffer(pix.center.sf, dist= 150, endCapStyle = "SQUARE")
  
  ## JOIN FIELD AND SATELLITE DATA
  data.combined <- st_join(field.sf.utm, pix.buffer.sf)
  
  
  ## Extract distinct rows to identify the unique satellite pixels that were sampled
  data.combined.distinct <- as_tibble(data.combined) %>% 
    select(date, waterbody, pix_site, pix_FID, starts_with("rhos")) %>% 
    rename(site= pix_site) %>% 
    distinct() %>% 
    arrange(site)
  
  return(data.combined.distinct)
  
}

calc_CI <- function(df){
  
  # Wynne et al. 2008 equation #1
  CI_sat <-  with(df,
                  -1*(rhos_681 - rhos_665 - (rhos_709 - rhos_665) * ((681-665) / (709-665)))
  )
  
  # From Lunetta et al. 2015
  ss665_sat <-  with(df,
                     rhos_665 - rhos_620 + (rhos_620 - rhos_681) * ((665-620) / (681-620)) # I prefer this equation because it uses a + like in Wynne et al. 2008
                     #rhos_665 - rhos_620 - (rhos_681 - rhos_620) * ((665-620) / (681-620))
  )
  
  # From Lunetta et al. 2015
  CIcyano_sat <- ifelse(ss665_sat > 0 & CI_sat > 0, CI_sat, 0)
  
  ## Add parameters to data frame
  df <- mutate(df,
               ss665_sat= ss665_sat,
               CI_sat= CI_sat,
               CIcyano_sat= CIcyano_sat)
  
  
  ## The variations on the formula below generate the result as the ss665 equation above
  ## Across the publications it is common to change the sign of the second operator between + and -
  ## and then reverse the order of the bands in the parenthese. If you do this, the result is the same.
  # ss665_2 <-  with(df.wide,
  #                  rhos_665 - b07_620 - (rhos_681 - b07_620) * (665-620) / (681-620)
  # )
  
  
  return(df)
}


############################################################################


#### MAP FUNCTIONS #########################################################
lakes_shapefile= ca_lakes_utm
DFGWATER_ID= 6342
utm= 10
extract_lake_shapefile <- function(lakes_shapefile, DFGWATER_ID, utm= 10){
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  
  lake_extract <- lakes_shapefile[lakes_shapefile$DFGWATERID == DFGWATER_ID, ]
  lake_extract_sf <- sf::st_as_sf(lake_extract, crs= utm.crs)
  return(lake_extract_sf)
    
  # lake_data <- lake_extract@data %>% 
  #   as_tibble() %>%
  #   mutate(id= rownames(lake_extract@data))
  # lake.df <- tidy(lake_extract)
  # lsa.df.merge <- left_join(lake.df, lake_data)
  # return(lsa.df.merge)
}

extract_lake_map_pixels <- function(tif.matrix, lake.id, utm){
  require(sf)
  require(tidyverse)
  
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  # Read CA lakes shapefile
  lake.extract <- st_read("Data/LakeMaps/CA_Lakes") %>% 
    # extract lake of interest
    filter(., DFGWATERID == lake.id) %>% 
    # Transform to utm  
    st_transform(., crs= utm.crs)
  
  
  
  ## EXTRACT LAKE FROM NOAA TIF MATRIX
  pix.center.sf <- as_tibble(tif.matrix) %>% 
    filter(., 
           x > st_bbox(lake.extract)[1] - 300, 
           y > st_bbox(lake.extract)[2] - 300, 
           x < st_bbox(lake.extract)[3] + 300, 
           y < st_bbox(lake.extract)[4] + 300) %>% 
    st_as_sf(.,
             coords= c("x", "y"),
             crs= utm.crs) %>% 
    mutate(pix_FID= str_pad(as.character(seq(0:(nrow(.)-1))), width=2, pad= "0"))
  
  pix.border.sf <- st_buffer(pix.center.sf, dist= 150, endCapStyle = "SQUARE")
  
  
  return(list(center= pix.center.sf, border= pix.border.sf))
}

lakewide_pixels <- function(points.sf, lake.polygon.sf, lake_ID, dist_m= 300){
  require(sf)
  require(tidyverse)
  
  ## Select inner points
  inner.points <- st_join(points.sf, lake.polygon.sf) %>% filter(DFGWATERID == lake_ID)
  
  ## Convert polygon to linestring
  lake.border <- st_cast(lake.polygon.sf, to = "MULTILINESTRING")
  
  ## Select points <X meters from the line (default= 300 m)
  buffer.points <- st_join(select(inner.points, pix_FID, geometry), lake.border, join= st_is_within_distance, dist= dist_m) %>% filter(DFGWATERID == lake_ID)
  buffer.pix_FID <- unique(buffer.points$pix_FID)
  
  ## Filter by pix_FID
  final.points <- inner.points[inner.points$pix_FID %in% buffer.pix_FID == FALSE, ]
  return(final.points)
}

sampling_locations <- function(field.lat.long, utm){
  require(tidyverse)
  require(sf)
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  
  ## FIELD DATA
  
  ## Read lat long of radiometer sites
  field.sf.utm <- read_tsv(field.lat.long[1]) %>% 
    # Make spatial feature object
    st_as_sf(.,
             coords= c("long", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= utm.crs)
}

gpx_pix_site <- function(gpx_file, pix_site_file, utm= 10){
  require(tidyverse)
  require(tmaptools)
  require(sf)
  require(lubridate)
  
  ## UTM
  if(utm == 10){
    utm.crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  if(utm == 11){
    utm.crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  
  ## GPX time stamp at prime meridian UTC
  ## California is -8 UTC in winter and -7 UTC in summer during daylight savings time
  
  ## Read in GPX
  gpx_sf <- read_GPX(gpx_file)$track_points %>% 
    select(time) %>% 
    mutate(date_time_PST= with_tz(ymd_hms(str_replace(time, "\\+00", "")), tzone= "America/Los_Angeles")) %>% 
    mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins"))
  
  ## Transform into a tibble and format columns
  # use do.call to extract the lat/long geometry from class sf
  # gpx_df <- tibble(date_time_UTC= as.character(gpx_tp$time), 
  #                  lon= do.call(rbind, sf::st_geometry(gpx_tp$geometry))[, 1],
  #                  lat= do.call(rbind, sf::st_geometry(gpx_tp$geometry))[, 2]) %>% 
  #   mutate(date_time_PST= with_tz(ymd_hms(str_replace(.$date_time_UTC, "\\+00", "")), tzone= "America/Los_Angeles")) %>% 
  #   mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins"))
  
  radio <- read_tsv(pix_site_file) %>% 
    mutate(date_time_PST= ymd_hms(str_c("2020-07-08", time, sep=" "), tz= "America/Los_Angeles")) %>% 
    mutate(date_time_PST= round_date(date_time_PST, unit= "5 mins")) %>% 
    rename(pix_site= pixel)
  
  
  gpx_radio_sf <- left_join(gpx_sf, select(radio, pix_site, date_time_PST)) %>% 
    filter(!is.na(pix_site)) %>% 
    st_as_sf(.,
             coords= c("lon", "lat"),
             crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
    # Transform to from Lat/Long to UTM
    st_transform(., crs= utm.crs)
  
  return(gpx_radio_sf)
}

plot_bounding_box <-  function(data_list, field_df, shapefile_utm, bbox, scalebar_dist= 150, utm_epsg= 32610){
  require(ggplot2)
  require(sf)
  
  bbox_plot <- ggplot() +
    geom_polygon(data= shapefile_utm, aes(x= long, y= lat, group= id), fill= "skyblue", color= "black", alpha= 0.25) +
    geom_sf(data= data_list[[1]][["border"]], fill= "transparent") +
    #geom_sf(data= data_list[[1]][["center"]], size= 0.5, color= "black") +
    geom_sf_text(data= data_list[[1]][["center"]], aes(label= pix_FID), size= 3, color= "black") +
    geom_sf(data= field_df, size= 4, aes(fill= pix_site, color= pix_site)) +
    coord_sf(xlim= c(bbox[1], bbox[3]), 
             ylim= c(bbox[2], bbox[4]),
             crs= utm_epsg) +
    labs(x= "Longitude", y= "Latitude", title= unique(data_list[["samples"]]$waterbody)) +
    theme_bw()
  
  return(bbox_plot)
}

############################################################################

#### RADIOMETER FUNCTIONS ####
## in_dir= Pathway to output of NOAA EXE program rrs files
## out_path= path and filename for exported .tsv file with CI values
calc_CI_values <- function(in_dir, out_path){
  require(tidyverse)
  
  ## Read in rrs files
  rrs_files <- list.files(in_dir)
  sample_IDs <-  str_replace(rrs_files, ".txt", "") %>% str_replace(., "rrs-", "")
  
  rrs_list <- map(rrs_files, function(x) suppressMessages(read_csv(file.path(in_dir, x), 
                                                                   skip= 31, 
                                                                   col_names = FALSE)) %>% 
                    rename(nm= X1, rrs= X2))
  
  ## FILTER WAVELENGTHS TO MATCH OLCI BANDS: 620, 665, 681, AND 709
  extract_olci_bands <- function(rrs_file){
    require(tidyverse)
    
    ## OLCI BANDS
    olci <- data.frame(band= c("b07_620", "b08_665", "b10_681", "b11_709"),
                       center=  c(620, 665, 681, 709),
                       width= c(10, 10, 7.5, 10),
                       stringsAsFactors = FALSE) %>% 
      mutate(min= center - width/2,
             max= center + width/2)
    
    
    ## FILTER SPECTRA BY OLCI BANDS
    spec_olci_bands <- do.call(rbind, apply(olci, 1, 
                                            function(x){
                                              rrs_file %>% 
                                                filter(nm >= x["min"] & nm <= x["max"]) %>% 
                                                mutate(band= x["band"])
                                            }))
    
    ## AVERAGE OLCI BAND WIDTHS TOGETHER
    spec_olci_means <- spec_olci_bands %>% 
      group_by(band) %>% 
      summarize(rrs_avg= mean(rrs, na.rm= TRUE),
                sd= sd(rrs, na.rm= TRUE), .groups= "drop") %>% 
      rename(rrs= rrs_avg)
    return(spec_olci_means)
    
  }
  
  olci_band_list <- map(rrs_list, extract_olci_bands)
  names(olci_band_list) <- sample_IDs
  
  
  ## CALCULATE CYANOBACTERIAL INDEX
  ## CI from Wynne et al. 2008 
  ## SS(665) from Lunetta et al. 2015
  
  calc_CI <- function(df){
    
    df.wide <- df %>% 
      select(-sd) %>% 
      spread(band, rrs)
    
    # Wynne et al. 2008 equation #1
    CI <-  with(df.wide,
                -1*(b10_681 - b08_665 - (b11_709 - b08_665) * ((681-665) / (709-665)))
    )
    
    # From Lunetta et al. 2015
    ss665 <-  with(df.wide,
                   b08_665 - b07_620 + (b07_620 - b10_681) * ((665-620) / (681-620)) # I prefer this equation because it uses a + like in Wynne et al. 2008
                   #b08_665 - b07_620 - (b10_681 - b07_620) * ((665-620) / (681-620))
    )
    
    ## The variations on the formula below generate the result as the ss665 equation above
    ## Across the publications it is common to change the sign of the second operator between + and -
    ## and then reverse the order of the bands in the parenthese. If you do this, the result is the same.
    # ss665_2 <-  with(df.wide,
    #                  b08_665 - b07_620 - (b10_681 - b07_620) * (665-620) / (681-620)
    # )
    
    
    return(data.frame(CI, ss665))
  }
  
  ci_list <- map(olci_band_list, calc_CI)
  names(ci_list) <- sample_IDs
  
  
  # Make into a dataframes
  rrs_bands_df <- do.call(rbind, olci_band_list) %>% cbind(rep(names(olci_band_list), each= 4), .) %>% as_tibble()
  names(rrs_bands_df)[1] <- "uniqueID"
  
  # Extract information from uniqueID column for rrs_bands_df
  rrs_bands_df <- rrs_bands_df %>% 
    mutate(waterbody= do.call(rbind, str_split(uniqueID, "-"))[, 1], 
           sample= do.call(rbind, str_split(uniqueID, "-"))[, 2]) %>% 
    mutate(site= do.call(rbind, str_split(sample, "_"))[, 1],
           rep= do.call(rbind, str_split(sample, "_"))[, 2]) %>% 
    select(uniqueID, waterbody, sample, site, rep, band, rrs, sd) 
  
  # Extract information from uniqueID column for ci_df
  ci_df <- tibble(uniqueID= names(ci_list), 
                  CI_field= do.call(rbind, ci_list)[, 1],
                  ss665_field= do.call(rbind, ci_list)[, 2]) %>% 
    mutate(waterbody= do.call(rbind, str_split(uniqueID, "-"))[, 1], 
           sample= do.call(rbind, str_split(uniqueID, "-"))[, 2]) %>% 
    mutate(site= do.call(rbind, str_split(sample, "_"))[, 1],
           rep= do.call(rbind, str_split(sample, "_"))[, 2]) %>% 
    select(uniqueID, waterbody, sample, site, rep, CI_field, ss665_field) 
  
  # Calc modified CI and pixel values
  ci_df <- ci_df %>% 
    mutate(CI_mod_field= CI_field * 15805.18,
           CI_pix_field= (log10(CI_field)+4.2) / 0.012,
           CIcyano_field= ifelse(ss665_field <= 0, 0, CI_field)) %>% 
    mutate(CI_pix_field= ifelse(is.na(CI_pix_field), 0, CI_pix_field)) %>% 
    mutate(pixel= str_replace(site, "S[0-9]", "")) %>% 
    mutate(ss665_threshold= ifelse(ss665_field <= 0, "<0", ">0"))
  
  
  # Write files
  write_tsv(rrs_bands_df, path= file.path(out_path, "rrs_OLCI_band_values.tsv"))
  write_tsv(ci_df, path= file.path(out_path, "CI_field.tsv"))
  return(ci_df)
}
############################################################################
