# Load required libraries
library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)
library(purrr)
library(pbapply)
library(XML)
library(future.apply)
library(future)

# Set base directory
base_dir <- "/home/shares/wwri-wildfire"

# Load and transform study area shape
study_area_admin1_shape <- st_read(file.path(base_dir, "data/multi-domain-data/boundary-layers/processed/admin-boundary-layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(study_area_admin1_shape, 5070)

# Load original WUI VRT raster
wilderness_urban_interface <- rast(file.path(base_dir, "data/built-environment-domain-data/intermediate-products/study_area_wui_map.vrt"))

# Inspect raster
message("Raster resolution: ", paste(res(wilderness_urban_interface), collapse = " x "))
message("Extent: ", paste(ext(wilderness_urban_interface), collapse = ", "))

# Simulate progress bar for plotting chunks (if large raster)
message("Plotting raster with progress...")
pboptions(type = "txt")

# Divide into tiles for simulated progress
tiles <- split(wilderness_urban_interface, f = 1:nlyr(wilderness_urban_interface))

# Plot each tile with progress
pblapply(tiles, function(tile) {
  plot(tile, main = "Wilderness-Urban Interface", col = terrain.colors(20))
  Sys.sleep(0.3)  # Simulate delay
})

#### Reproject input rasters used in WUI .vrt ####

# Extract source file paths from VRT
vrt_path <- file.path(base_dir, "data/built-environment-domain-data/intermediate-products/study_area_wui_map.vrt")
vrt_xml <- xmlParse(vrt_path)
source_files_raw <- xpathSApply(vrt_xml, "//SourceFilename", xmlValue)

# Output directory for reprojected tiles
output_dir <- file.path(base_dir, "data/multi-domain-data/wilderness-urban-interface/processed/reprojected/")

# Set up parallel processing
plan(multisession, workers = 40)

# Step 4: Reproject each raster
reprojected_paths <- future_lapply(source_files_raw, function(path) {
  if (!file.exists(path)) return(NA)  # skip if file is missing
  
  r <- rast(path)
  out_path <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(path)), "_5070.tif"))
  r_proj <- terra::project(r, "EPSG:5070")
  writeRaster(r_proj, out_path, overwrite = TRUE)
  return(out_path)
})

# Filter out already-reprojected 'masked' tiles
filtered_files <- source_files_raw[!grepl("masked_rasters|masked_", source_files_raw)]

# Reproject the raster tiles
reprojected_paths <- future_lapply(filtered_files, function(path) {
  if (!file.exists(path)) return(NA)

  folder_name <- basename(dirname(path))  # For unique output name
  out_filename <- paste0(folder_name, "_WUI_5070.tif")
  out_path <- file.path(output_dir, out_filename)

  if (file.exists(out_path)) {
    message("Skipping existing file: ", out_filename)
    return(out_path)
  }

  r <- rast(path)
  r_proj <- terra::project(r, "EPSG:5070")
  writeRaster(r_proj, out_path, overwrite = TRUE)
  return(out_path)
})

#### Create new WUI .vrt in EPSG 5070 from reprojected rasters ####

# Re-list all 5070 .tif files
reprojected_tifs <- list.files(output_dir, pattern = "_5070.tif$", full.names = TRUE)

# Output VRT path
new_vrt_path <- file.path(base_dir, "data/built-environment-domain-data/intermediate-products/study_area_wui_map_5070.vrt")

# Write file list for gdalbuildvrt
tempfile_with_file_list <- tempfile(fileext = ".txt")
writeLines(reprojected_tifs, tempfile_with_file_list)

# Create VRT using GDAL
system2("gdalbuildvrt", args = c("-input_file_list", tempfile_with_file_list, new_vrt_path))

# Load and visualize reprojected VRT
wilderness_urban_interface_5070 <- terra::rast(new_vrt_path)

plot(wilderness_urban_interface_5070)

pboptions(type = "txt")
message("Plotting raster with progress...")

tiles_5070 <- split(wilderness_urban_interface_5070, f = 1:nlyr(wilderness_urban_interface_5070))

pblapply(tiles_5070, function(tile) {
  plot(tile, main = "Wilderness-Urban Interface 5070", col = terrain.colors(20))
  Sys.sleep(0.3)
})

