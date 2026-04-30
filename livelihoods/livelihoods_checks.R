library(terra)
library(sf)
library(tidyverse)

# Check 1: No positive differences
livelihoods_status <- rast("/home/shares/wwri-wildfire/final_layers/2024/livelihoods/livelihoods_status.tif")
livelihoods_resistance <- rast("/home/shares/wwri-wildfire/final_layers/2024/livelihoods/livelihoods_resistance.tif")
livelihoods_recovery <- rast("/home/shares/wwri-wildfire/final_layers/2024/livelihoods/livelihoods_recovery.tif")
livelihoods <- rast("/home/shares/wwri-wildfire/final_layers/2024/livelihoods/livelihoods_domain_score_mean.tif")
states_vect <- vect("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")

livelihoods_resistance_only <- mean(c(livelihoods_status, livelihoods_resistance), na.rm = TRUE)
livelihoods_dif <- livelihoods_resistance_only - livelihoods # [resistance-(resistance & recovery)] 
plot(livelihoods_dif)
livelihoods_dif
# SHOULD BE NO POSITIVES

# Check 2: Min/Max raster values
# get a list of all the tif files in the final_layers folder
final_layers_path <- "/home/shares/wwri-wildfire/final_layers/2024/livelihoods/"

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
# Print the file information with min and max values
print(file_info)
