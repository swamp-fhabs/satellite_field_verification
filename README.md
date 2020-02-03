# satellite_field_verification
California State Water Resources Control Board Office of Information Management and Analysis (OIMA)  
San Francisco Estuary Institute (SFEI)  

Research project to verify satellite derived Cyanobacterial Index in California lakes. This index uses satellite imagery from the OLCI sensor on Sentinel-3 to estimate cyanobacterial density in the surface of water bodies.

California satellite data webpage :https://mywaterquality.ca.gov/habs/data_viewer/#satellite

## Workflow
The README describes the workflow to convert raw radiometer data into remote sensed reflectance values, and then the calculate cyanobacterial index. The program, `test_asd_group.exe`, used to calculate remote sensed reflectance values from radiometer data was provided by NOAA scientists Michelle Tomlinson and Andrew Meredith. 

All scripts are written in R. Pathways in R scripts are relative to the git directory. If you create an Rstudio Project `.Rmd` file in the same directory as the git directory or `setwd()` to the git directory, then all pathways should work.

### Field data collection
On each lake, three sub-samples were collected within each satellite pixel. At each sampling site within the pixel, triplicate measurements of a calibrated 10% reflectance plate, water, and sky were made. Radiometer data was collected with a Malvern Panalytics ASD FieldSpec Handheld2 Pro (HH2) radiometer. Each of the triplicate measurement from the HH2 includes ten replicates, which are then averaged together to create a single spectra for the measurement. After collections, files are exported from the HH2 using the `asii export` function, which produces `.asd.txt` files with radiance values at each wavelength (nm).

### 1) Calculate remote sensed reflectance (rrs)
The script `radiometer_noaa_format.R` contains several functions to wrangle the ASCII export files (`.asd.txt`) from the ASD HH2, and prepare them to calculate remote sensed reflectance (rrs). 

#### A) Wrangle files
The replicate sample files need to be associated with a site and plate, water, or sky measurement. The function `format_radiometer_metadata` reads in the metadata file with the site and measurement type information. The function `export_spectra_files` exports a metadata file associating `.asd.txt` files with their sample site information. The function `make_spectra_plots` generates a `.jpg` figure of the raw radiance spectra of each plate, water, and sky measurement and puts all the exported `.jpg` files into a single directory. 

![exported spectra](https://github.com/swamp-fhabs/satellite_field_verification/blob/master/Data/20190801_LakeSanAntonio/spectra_out/P1S1_2.jpg)
**Figure 1** Example of exported spectra plots showing radiance values from the ASD from plate, sky, and water measurements taken at a single site. Each measurement involves collecting ten replicate measurements, which are then averaged together.

#### B) Remove erroneous measurements
In some sites multiple plate, sky, or water measurements were made due to errors or in the initial sample. The output spectra are manually looked at by the user and for sites where multiple measurements were made, the identity of the replicate to keep are added to the text file `type_rep_keep.txt`, which is in the same directory as the spectra figures. 

#### C) Prepare folder structure
The program to calculate remote sensed reflectance (rrs) requires all the `.asd.txt` spectra files be in a single directory. The function `noaa_format_export` writes a new `.txt` metadata file is created with a reduced number of `.asd.txt` files associated with the site.  Then the function `copy_ascii_files` copies the `asd.txt` files into a directories for each specific site, where the `.txt` metadata file also resides. 

#### D) Write batch script to run the rrs program
The program to calculate rrs (`test_asd_group.exe`) runs on a single directory at a time. The function needs a path to the program, the calibration file for the 10% reflectance plate, and the `.txt` metadata file. The program is then run from the directory where the `.asd.txt` files are located. The batch file is a text file with all the code to run the program in multiple directories and has a `.bat` extension. The R function `write_batch_file` creates the batch file.

#### E) Calculate rrs
Once the batch file is generated in step 4, then you can double-click on it in Windows explorer (outside of R), and it will sequentially change directories and run the `test_asd_group.exe` program. This program creates plots of the plate, water, and sky spectra, as well as an averaged rrs spectra and a `.txt` file of the rrs at each wavelength. The rrs output file is called `seabassXXXX.txt`. These files are then renamed to include their sample name and moved to a single directory.

![rrs spectra](https://github.com/swamp-fhabs/satellite_field_verification/blob/master/Data/20190801_LakeSanAntonio/noaa_files/P1S1_2/rrs_group0_2019_08_01___10_57_20_000000_UNKNOWN_UNKNOWN_sky_0.028.png)

**Figure 2** Remote sensed reflectance spectra generated by the `test_asd_group.exe` program. The program outputs the average of the ten replicate measurements to obtain the rrs for the water at that location.

### 2) Calculate cyanobacterial index (CI) values
The rrs values are used to calculate the cyanobacterial indices CI and CIcyano. The cyanobacterial index  (CI) was first published by Wynne et al. in 2008. The equation uses three wavelength bands (665, 681, and 709 nm) on the OCLI sensor to estimate cyanobacterial density. If the rrs value at 681 nm is lower than expected, then it is indicative of cyanobacteria in a waterbody. (For more information see, Wynne et al. 2008, Lunetta et al. 2015, and Stumpf et al. 2016). These CI calculations are performed in the R script `calc_CI_from_rrs,R`, which then exports a `.tsv` file with a CI value for each measurement. The CI value is derived from the spectral shape (SS) at 681 nm and is calculated with equation 1 in Wynne et al. 2008:

>SS(681) = rrs681 - rrs665 - (rrs709 - rrs665) * [(681-665)/(709-665)]

More negative SS(681) values represent higher cyanobacterial abundances in the surface waters of a pixel. To transform it into positive values the SS(681) is converted to CI by: 

>CI = -1*SS(681)

CI values <0 indicate no cyanobacteria present. However, certain water conditions can generate positive CI values, even when there are no cyanobacteria present. To reduce the frequency of false positives, Lunetta et al. 2015 proposed an additional indice, CIcyano, based on the spectral shape at 665 nm, which is calculated by: 

>SS(665) = rrs665 - rrs620 + (rrs620 - rrs681) * [(665-620)/(681-620)]

When SS(665) is >0 it indicates cyanobacteria present in the water and when it is <0, cyanobacteria is predicted to be absent. SS(665) is then transformed into an exclusion criteria value of 1 when SS(665)  >0 and 0 when SS(665) <0. 

> CIcyano = CI, if SS(665) > 0  

> CIcyano = 0, if SS(665) < 0
 
This will render all measurements with SS(665)< 0 as non-detects, even if they had a positive CI value.

The satellite returns an 8-bit pixel value from 0-250 for the CIcyano. Conversions between the satellite pixel value and the CIcyano value use a log10 scale and are given as: 

>CIcyano = 10^(0.012 * sat_pix_value - 4.2)  
>sat_pix_value = (log10(CIcyano) + 4.2) / 0.012

The CIcyano value is <1 and often a small decimal number. A modified value is used in our project to put the numbers on a scale of 1-1000. The modified CI is given as: 

>CI_mod = CIcyano * 15805.18


