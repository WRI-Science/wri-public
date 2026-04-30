wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)
library(sf)
library(foreach)
library(doParallel)

# Check 1: No positive differences
livelihoods_status <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_status.tif"))
livelihoods_resistance <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_resistance.tif"))
livelihoods_recovery <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_recovery.tif"))
livelihoods <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_domain_score_mean.tif"))
states_vect <- vect(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))

# Check 2: NA Checking (ideally want all 7s)
# Set up parallel backend
n_cores <- 14
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Classify function
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

# Parallel loop
results <- foreach(i = 1:length(states_vect), .packages = c("terra", "sf")) %dopar% {
  # Read in necessary data in the parallel environment
  states_vect <- vect(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))
  livelihoods_status <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_status.tif"))
  livelihoods_resistance <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_resistance.tif"))
  livelihoods_recovery <- rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_recovery.tif"))
  
  # Stack the layers of interest
  livelihoods_stack <- c(livelihoods_status, livelihoods_resistance, livelihoods_recovery)
  
  # Select state of interest
  state_geom <- states_vect[i]
  state_name <- tolower(gsub(" ", "_", state_geom$name)) # Get name for file name
  
  # Crop and mask raster stack to state/province polygon
  cropped <- crop(livelihoods_stack, state_geom)
  masked  <- mask(cropped, state_geom)
  
  # Run classification
  classified <- app(masked, classify_na_type)
  
  # Write output raster for the state
  out_path <- file.path(wri_project_root, "final_layers", "2024", "livelihoods", paste0("livelihoods_classified_", state_name, ".tif"))
  writeRaster(classified, out_path, overwrite=TRUE)
  
  return(out_path)
}

stopCluster(cl)

# Folder where files were saved
out_folder <- file.path(wri_project_root, "final_layers", "2024", "livelihoods")

# List all chunk files
chunk_files <- list.files(out_folder, pattern = "^livelihoods_classified_.*\\.tif$", full.names = TRUE)

# Read all rasters
rasters_list <- lapply(chunk_files, rast)

# Merge all rasters
merged_raster <- do.call(merge, rasters_list)

plot(merged_raster)

# Save merged output
writeRaster(merged_raster,
            file.path(out_folder, "livelihoods_classified_merged.tif"),
            overwrite = TRUE)

plot(rast(file.path(wri_project_root, "final_layers", "2024", "livelihoods", "livelihoods_classified_merged.tif")))
