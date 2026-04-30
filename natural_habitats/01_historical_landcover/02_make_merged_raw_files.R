#### Goal ####
# The goal of this script is to merge the historical landcover tiles that were seperated from the rest of the raw files in step 1.
# Once merged they will be saved and then cropped to our study area and have the ecoregion analysis conducted.

#### Packages ####
library(terra)
library(sf)  # For system calls

#### File Paths ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/"
raw_data_file_path <- file.path(data_file_path, "natural_habitats/raw/")
int_data_file_path <- file.path(data_file_path, "natural_habitats/int/")
historical_landcover_int_data_file_path <- file.path(int_data_file_path, "historical_landcover")
raw_path_2005_data <- file.path(historical_landcover_int_data_file_path, "2005/all_raw_tifs/")
vrt_2005_path <- file.path(historical_landcover_int_data_file_path, "2005/historical_landcover_virtual_2005.vrt")
merged_2005_save_path <- file.path(historical_landcover_int_data_file_path, "2005/historical_landcover_merged_2005.tif")
raw_path_2015_data <- file.path(historical_landcover_int_data_file_path, "2015/all_raw_tifs/")
vrt_2015_path <- file.path(historical_landcover_int_data_file_path, "2015/historical_landcover_virtual_2015.vrt")
merged_2015_save_path <- file.path(historical_landcover_int_data_file_path, "2015/historical_landcover_merged_2015.tif")

#### Make Merged Files with GDAL Approach ####
tif_files_2005 <- list.files(raw_path_2005_data, pattern = "\\.tif$", full.names = TRUE)

# Create a text file with the list of input files
list_file <- file.path(historical_landcover_int_data_file_path, "2005/file_list.txt")
writeLines(tif_files_2005, list_file)

# Create the VRT file using gdalbuildvrt
vrt_cmd <- paste0("gdalbuildvrt -input_file_list ", list_file, " ", vrt_2005_path)
system(vrt_cmd)

# Check if VRT was created successfully
if (file.exists(vrt_2005_path)) {
  cat("VRT created successfully at:", vrt_2005_path, "\n")
  
  # Now use gdal_translate to create the merged GeoTIFF without loading into R memory
  translate_cmd <- paste0("gdal_translate -of GTiff ",
                          "-co COMPRESS=LZW ",
                          "-co BIGTIFF=YES ",
                          "-co TILED=YES ",
                          "-co BLOCKXSIZE=512 ",
                          "-co BLOCKYSIZE=512 ",
                          vrt_2005_path, " ", 
                          merged_2005_save_path)
  
  cat("Running gdal_translate...\n")
  system(translate_cmd)
  
  # Check if merged file was created
  if (file.exists(merged_2005_save_path)) {
    cat("Merged file created successfully at:", merged_2005_save_path, "\n")
    cat("Processing complete!\n")
  } else {
    cat("Error: Failed to create merged file\n")
  }
} else {
  cat("Error: Failed to create VRT file\n")
}

#### Repeat Process with 2015 Data ####
tif_files_2015 <- list.files(raw_path_2015_data, pattern = "\\.tif$", full.names = TRUE)

# Create a text file with the list of input files
list_file <- file.path(historical_landcover_int_data_file_path, "2015/file_list.txt")
writeLines(tif_files_2015, list_file)

# Create the VRT file using gdalbuildvrt
vrt_cmd <- paste0("gdalbuildvrt -input_file_list ", list_file, " ", vrt_2015_path)
system(vrt_cmd)

# Check if VRT was created successfully
if (file.exists(vrt_2015_path)) {
  cat("VRT created successfully at:", vrt_2015_path, "\n")
  
  # Now use gdal_translate to create the merged GeoTIFF without loading into R memory
  translate_cmd <- paste0("gdal_translate -of GTiff ",
                          "-co COMPRESS=LZW ",
                          "-co BIGTIFF=YES ",
                          "-co TILED=YES ",
                          "-co BLOCKXSIZE=512 ",
                          "-co BLOCKYSIZE=512 ",
                          vrt_2015_path, " ", 
                          merged_2015_save_path)
  
  cat("Running gdal_translate...\n")
  system(translate_cmd)
  
  # Check if merged file was created
  if (file.exists(merged_2015_save_path)) {
    cat("Merged file created successfully at:", merged_2015_save_path, "\n")
    cat("Processing complete!\n")
  } else {
    cat("Error: Failed to create merged file\n")
  }
} else {
  cat("Error: Failed to create VRT file\n")
}