wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)
library(sf)
library(tidyverse)

# Check 1: No positive differences
species_status <- rast(file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species", "sense_of_place_iconic_species_status.tif"))
species_resistance <- rast(file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species", "sense_of_place_iconic_species_resistance.tif"))
species_recovery <- rast(file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species", "sense_of_place_iconic_species_recovery.tif"))
species <- rast(file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species", "sense_of_place_iconic_species_domain_score_mean.tif"))
states_vect <- vect(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))

species_resistance_only <- mean(c(species_status, species_resistance), na.rm = TRUE)
species_dif <- species_resistance_only - species # [resistance-(resistance & recovery)] 
plot(species_dif)
# SHOULD BE NO POSITIVES

# Check 2: Min/Max raster values
# get a list of all the tif files in the final_layers folder
final_layers_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species")

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
