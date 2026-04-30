library(terra)
library(sf)
library(tidyverse)

# Check 1: No positive differences
places_status <- rast("/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/sense_of_place_iconic_places_status.tif")
places_resistance <- rast("/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/sense_of_place_iconic_places_resistance.tif")
places_recovery <- rast("/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/sense_of_place_iconic_places_recovery.tif")
places <- rast("/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/sense_of_place_iconic_places_domain_score_mean.tif")
states_vect <- vect("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")

# do calc like this because places status is a mask and we are using masked versions:
places_dif <- places_resistance - places # [resistance-(resistance & recovery)] 
places_dif

# places_resistance_only <- mean(c(places_status, places_resistance), na.rm = TRUE)
# places_dif <- places_resistance_only - places # [resistance-(resistance & recovery)] 
# places_dif
# SHOULD BE NO POSITIVES

# Check 2: Min/Max raster values
# get a list of all the tif files in the final_layers folder
final_layers_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_places/"

# make the list of tif files
tif_files <- list.files(final_layers_path, pattern = "\\.tif$", full.names = TRUE)

# initialize data.frame to hold min/max
file_info <- data.frame(
  file = basename(tif_files),   # just store the filename (optional)
  min  = NA_real_,
  max  = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(tif_files)) {
  # read in the raster
  r <- rast(tif_files[i])
  
  # compute min and max on‐disk (no need to pull all values into RAM)
  stats <- global(r, fun = c("min", "max"), na.rm = TRUE)
  # 'stats' is a 1×2 data.frame (min in column 1, max in column 2)
  file_info$min[i] <- stats[1, "min"]
  file_info$max[i] <- stats[1, "max"]
  
  # remove raster object and force garbage collection
  rm(r)
  gc()
}

print(file_info)
