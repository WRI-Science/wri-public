wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra) # For raster operations
library(sf) # For spatial operations
library(foreach) # For parallel processing
library(doParallel) # For parallel processing


#### Script Overview ####
# This script runs checks on the final species layers, including:
# 1. Ensuring no positive differences for the resistance-only scenario.
# 2. Classifying NA types based on status, resistance, and recovery values.
# 3. Checking the study area raster has a value in the domain score raster for every cell where study area raster is not NA.
# Note: Should probably add an indicator-level check once we have indicator files. This is less needed for this domain though.


#### Base Directories ####
final_layers_file_path <- file.path(wri_project_root, "final_layers")
multi_domain_data_path <- file.path(wri_project_root, "data", "multi_domain_data")
path_year <- "2024"
final_layers_output_path <- file.path(final_layers_file_path, path_year, "biodiversity") # output path for final layers
final_layers_checks_output_path <- file.path(final_layers_output_path, "final_checks") # output path for final checks


#### Boundary Layers ####
# For chunking during parallelization
states_vect <- vect(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))

# For the last check
study_area_template <- rast(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))


#### Data Layers ####
# Read in layers used for checks
biodiversity_status <- rast(file.path(final_layers_output_path, "biodiversity_status.tif"))
biodiversity_resistance <- rast(file.path(final_layers_output_path, "biodiversity_resistance.tif"))
biodiversity_recovery <- rast(file.path(final_layers_output_path, "biodiversity_recovery.tif"))
biodiversity_domain_score <- rast(file.path(final_layers_output_path, "biodiversity_domain_score.tif"))


#### Functions ####
# Function to classify NA types based on the status, resistance, and recovery values
classify_na_type <- function(v) {
  # s = status
  # r1 = resistance
  # r2 = recovery
  s  <- is.na(v[1]); r1 <- is.na(v[2]); r2 <- is.na(v[3])
  if    ( s &  r1 &  r2) return(NA)
  else if(!s &  r1 &  r2) return(1)
  else if( s & !r1 &  r2) return(2)
  else if( s &  r1 & !r2) return(3)
  else if(!s & !r1 &  r2) return(4)
  else if(!s &  r1 & !r2) return(5)
  else if( s & !r1 & !r2) return(6)
  else                    return(7)
}


#### Data Processing ####
# Check 1: No positive differences for resistance-only scenario
biodiversity_resistance_only <- mean(c(biodiversity_status, biodiversity_resistance), na.rm = TRUE)
biodiversity_dif <- biodiversity_resistance_only - biodiversity_domain_score # [resistance-(resistance & recovery)] 
plot(biodiversity_dif) # Should be no positives; should not improve with this calculation

# Write out the difference raster to inspect
writeRaster(biodiversity_dif, 
            file.path(final_layers_checks_output_path, "biodiversity_dif.tif"),
            overwrite = TRUE)


# Check 2: NA Checking (ideally want all 7s)
# Set up parallel backend
n_cores <- 14
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run parallel loop for NA checking
results <- foreach(i = 1:length(states_vect), .packages = c("terra", "sf")) %dopar% {
  # Read in necessary data in the parallel environment
  #### Base Directories ####
  final_layers_file_path <- file.path(wri_project_root, "final_layers")
  multi_domain_data_path <- file.path(wri_project_root, "data", "multi_domain_data")
  path_year <- "2024"
  final_layers_output_path <- file.path(final_layers_file_path, path_year, "biodiversity") # output path for final layers
  final_layers_checks_output_path <- file.path(final_layers_output_path, "final_checks") # output path for final checks
  
  #### Data Layers ####
  # Read in layers used for checks
  biodiversity_status <- rast(file.path(final_layers_output_path, "biodiversity_status.tif"))
  biodiversity_resistance <- rast(file.path(final_layers_output_path, "biodiversity_resistance.tif"))
  biodiversity_recovery <- rast(file.path(final_layers_output_path, "biodiversity_recovery.tif"))
  
  # For chunking during parallelization
  states_vect <- vect(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))
  
  #### Data Processing ####
  # Stack the layers of interest
  biodiversity_stack <- c(biodiversity_status, biodiversity_resistance, biodiversity_recovery)
  
  # Select state of interest
  state_geom <- states_vect[i]
  state_name <- tolower(gsub(" ", "_", state_geom$name)) # Get name for file name

  # Crop and mask raster stack to state/province polygon
  cropped <- crop(biodiversity_stack, state_geom)
  masked  <- mask(cropped, state_geom)

  # Run classification
  classified <- app(masked, classify_na_type)

  # Write output raster for the state
  out_path <- file.path(final_layers_checks_output_path, paste0("biodiversity_classified_", state_name, ".tif"))
  writeRaster(classified, out_path, overwrite=TRUE)

  return(out_path)
}

stopCluster(cl) # Stop the parallel cluster


# Read in the chunk files from the parallelization and merge them into one
# List all chunk files
chunk_files <- list.files(final_layers_checks_output_path, pattern = "^biodiversity_classified_.*\\.tif$", full.names = TRUE)

# Read in all rasters in a list
rasters_list <- lapply(chunk_files, rast)

# Merge all rasters
merged_raster <- do.call(merge, rasters_list)

# Save merged output
writeRaster(merged_raster,
            file.path(final_layers_checks_output_path, "biodiversity_classification_merged.tif"),
            overwrite = TRUE)

# Check 3: Ensure study area raster has a value in the domain score raster for every cell where study area raster is not NA

# Identify any gaps (TRUE cells)
gaps <- !is.na(study_area_template) & is.na(biodiversity_domain_score)
plot(gaps, main = "Uncovered Mask Cells (TRUE = gap)")

# Write out the gaps raster for inspection
writeRaster(gaps, 
            file.path(final_layers_checks_output_path, "biodiversity_gaps_in_domain_score.tif"),
            overwrite = TRUE)