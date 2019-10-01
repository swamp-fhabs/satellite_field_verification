# satellite_field_verification
Project to verify satellite derived Cyanobacterial Index.
California State Water Resources Control Board and San Francisco Estuary Institute.

Satellite data webpage :https://mywaterquality.ca.gov/habs/data_viewer/#satellite

## Workflow
The README describes the workflow to convert raw radiometer data into remote sensed reflectance values and the plot them. 

### Field data collection
On each lake, three sub-samples were collected within each satellite pixel. At each sampling site within the pixel, triplicate measurements of a calibrated 10% reflectance plate, water, and sky were made. Radiometer data was collected with a Malvern Panalytics ASD FieldSpec Handheld2 Pro (HH2) radiometer. Each of the triplicate measurement from the HH2 includes ten replicates, which are then averaged together to create a single spectra for the measurement. After collections, files are exported from the HH2 using the `asii export` function, which produces `.asd.txt` files with radiance values at each wavelength (nm).

### 1) Calculate remote sensed reflectance (rrs)
The script `radiometer_noaa_format.R` contains several functions to wrangle the ASCII export files (`.asd.txt`) from the ASD HH2, and prepare them to calculate remote sensed reflectance (rrs). 

#### A) Wrangle files
The replicate sample files need to be associated with a site and plate, water, or sky measurement. The function `format_radiometer_metadata` reads in the metadata file with the site and measurement type information. The function `export_spectra_files` exports a `.tsv` associating `.asd.txt` files with their sample site information. The function `make_spectra_plots` generates a `.jpg` figure of the raw radiance spectra of each plate, water, and sky measurement and puts all the exported `.jpg` files into a single directory. 

#### B) Remove erroneous measurements
In some sites multiple plate, sky, or water measurements were made due to errors or in the initial sample. The output spectra are manually looked at by the user and for sites where multiple measurements were made, the identity of the replicate to keep are added to the text file `type_rep_keep.txt`, which is in the same directory as the spectra figures. 

#### C) Prepare folder structure
The program to calculate remote sensed reflectance (rrs) requires all the `.asd.txt` spectra files be in a single directory. The function `noaa_format_export` writes a new `.txt` metadata file is created with a reduced number of files associated with the site.  Then the function `copy_ascii_files` copies the `asd.txt` files into a directories for each specific site, where the `.txt` metadata file also resides. 

#### D) Write batch script to run the rrs program
The program to calculate rrs (`test_asd_group.exe`) runs on a single directory at a time. The function needs a path to the program, the calibration file for the 10% reflectance plate, and the `.txt` metadata file. The program is then run from the directory where the `.asd.txt` files are located. The batch file is a text file with all the code to run the program in multiple directories and has a `.bat` extension. The R function `write_batch_file` creates the batch file.

#### E) Calculate rrs
Once the batch file is generated in step 4, then you can double-click on it in Windows explorer (outside of R), and it will sequentially change directories and run the rrs program. This program creates plots of the plate, water, and sky spectra, as well as an averaged rrs spectra and a `.txt` file of the rrs at each wavelength. The rrs output file is called `seabassXXXX.txt`. These files are then renamed to include their sample name and moved to a single directory.

### 2) Calculate cyanobacterial index (CI) values
