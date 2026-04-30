wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal #### 
# The goal of this script is to compute the min and mean sd the NDVI data 
# across the entire year from the rasters generated in step 1. Once those rasters
# are generated we will use them to rescale the NDVI data for each ecoregion.
# For now the mean raster will be used for the final layers and the min will be 
# used in the sensitivity analysis.

# NOTE: Sensitiy testing output for 2024 is currently commented out becasue files need to be made from step 1.
# without the sensitivity testing this takes about 4 hours to run. With the sensitivity testing it takes about 8.5 hours.

#### Load Packages ####
library(tidyverse)
library(terra)

#### Set up file paths and options ####
# Setup terra options for parallel processing
terraOptions(
  threads = 12,        # Use 12 threads
  memfrac = 0.90      # Limit RAM usage to 90%
)

year_of_interest <- 2024

# Paths
natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")
int_ndvi_path <- paste0(natural_habitats_base_path, "int/NDVI/", year_of_interest, "/")
ndvi_sd_path <- paste0(int_ndvi_path, "/rolling_sd/")
# put mean in unrescaled indicators folder
output_mean_path <- paste0(natural_habitats_base_path, "unrescaled_indicators/", "NDVI_", year_of_interest, "_mean_sd.tif")
output_min_path <- paste0(int_ndvi_path, "mean_min_sd/", "NDVI_", year_of_interest, "_min_sd.tif")

# put mean in unrescaled indicators folder
rescaled_mean_path <- paste0(natural_habitats_base_path, "rescaled_indicators/", year_of_interest, "/", "NDVI_", year_of_interest, "_mean_sd_rescaled.tif")
#rescaled_min_path <- paste0(int_ndvi_path, "mean_min_sd/", "NDVI_", year_of_interest, "_min_sd_rescaled.tif")

final_layer_output_path_mask <- file.path(wri_project_root, "final_layers", year_of_interest, "natural_habitats", "indicators_mask", "natural_habitats_resistance_NDVI.tif")
final_layer_output_path_no_mask <- file.path(wri_project_root, "final_layers", year_of_interest, "natural_habitats", "indicators_no_mask", "natural_habitats_resistance_NDVI.tif")

# sensitivity_test_output_path <- paste0(natural_habitats_base_path, "sensitivity_testing/", "NDVI_", year_of_interest, "_min_sd_rescaled.tif")

# Ecoregion shapefile
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
ecoregion_intersection_path <- file.path(multi_domain_data_file_path, 
                                         "int/boundary_layers/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp")

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                   "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# ag/urban mask
ag_urban_mask_path <- paste0(natural_habitats_base_path, "int/esri_present_landcover/", year_of_interest, "/full_masks/full_ag_urban_mask_90m_5070.tif")
# if the ag_urban mask file does not exist use the previous years data
if (!file.exists(ag_urban_mask_path)) {
  ag_urban_mask_path <- paste0(natural_habitats_base_path, "int/esri_present_landcover/", year_of_interest - 1, "/full_masks/full_ag_urban_mask_90m_5070.tif")
}

# Override existing outputs?  (set to TRUE to force re-processing)
force_rerun <- TRUE

#### Process Mean and Min Rasters ####
# --- Load Raster Files ---
cat("Listing raster files...\n")
tif_files <- list.files(path = ndvi_sd_path, pattern = paste0(year_of_interest, ".*\\.tif$"), full.names = TRUE)
raster_stack <- rast(tif_files)

# --- Compute Mean Raster ---
cat("Computing mean raster...\n")
time_mean <- system.time({
  mean_raster <- app(raster_stack, fun = mean, na.rm = TRUE, filename = output_mean_path, overwrite = TRUE)
})
print(time_mean)

# --- Compute Min Raster ---
# cat("Computing min raster...\n")
# time_min <- system.time({
#   min_raster <- app(raster_stack, fun = min, na.rm = TRUE, filename = output_min_path, overwrite = TRUE)
# })
# print(time_min)

# Remove raster_stack, mean_raster and min_raster to free memory
rm(raster_stack, mean_raster, min_raster)

#### Function to Process Rescaled Rasters
# --- Function to Rescale, Reproject, and Save Raster ---
process_and_rescale_raster <- function(raster_path, output_path) {
  # If the output already exists and we're not forcing a re-run, skip everything
  if (file.exists(output_path) && !force_rerun) {
    cat("Output already exists, skipping:\n  ", output_path, "\n\n")
    return(invisible(NULL))
  }
  
  cat("\nProcessing:", raster_path, "\n")
  
  # Load Raster and Ecoregions
  r <- rast(raster_path)
  eco <- vect(ecoregion_intersection_path)
  
  # crs
  moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"
  
  cat("Reprojecting ecoregions and raster to moll_crs CRS...\n")
  r <- project(r, moll_crs, threads = TRUE)
  eco <- project(eco, moll_crs)
  
  # Compute 99th percentile NDVI per polygon
  cat("Extracting 99th percentile per polygon...\n")
  zs <- terra::extract(r, eco, fun = function(x) {
    if (all(is.na(x))) return(NA)
    quantile(x, probs = 0.99, na.rm = TRUE)
  })
  names(zs)[2] <- "p99"
  eco$p99 <- zs[ , "p99"]
  
  # Rasterize the p99 values
  cat("Rasterizing p99 values...\n")
  p99_r <- rasterize(eco, r, field = "p99", touches = TRUE)
  
  # Rescale raster by p99 and clamp
  cat("Rescaling and clamping raster...\n")
  scaled <- clamp(r / p99_r, lower = 0, upper = 1)
  
  # # Write native-resolution scaled raster
  # scaled_native_path <- paste0(output_prefix, "_rescaled_native.tif")
  # writeRaster(scaled, scaled_native_path, overwrite = TRUE)
  # cat("Saved rescaled raster:", scaled_native_path, "\n")
  
  # Reproject & resample
  target_crs <- "EPSG:5070"
  target_res <- 90
  cat("Reprojecting and resampling to EPSG:5070 at 90m...\n")
  scaled_proj <- project(scaled, target_crs, method = "bilinear", res = target_res,
                         threads = TRUE)
  
  # Rename raster layer
  names(scaled_proj) <- "rescaled_NDVI"
  
  # Run through raster formatting function
  cat("Aligning ouptut raster to template...\n")
  source(here::here("templates_and_functions", "align_raster_to_template.R"))
  template_raster <- rast(template_raster_path)
  scaled_proj <- align_raster_to_template(template_raster = template_raster,
                                          input_raster = scaled_proj,
                                          input_type = "continuous")
  rm(template_raster)
  
  
  # Save reprojected raster
  cat("Saving reprojected raster...\n")
  writeRaster(scaled_proj, output_path, overwrite = TRUE)
  cat("Saved reprojected raster:", output_path, "\n")
}

# --- Process Both Mean and Min Rasters ---
# process_and_rescale_raster(raster_path = output_min_path, 
#                            output_path = rescaled_min_path)
process_and_rescale_raster(raster_path = output_mean_path, 
                           output_path = rescaled_mean_path)

# Save min raster in sensitivity testing
# cat("Saving min raster for sensitivity testing...\n")
# sensitivity_test_raster <- rast(rescaled_min_path)
# writeRaster(sensitivity_test_raster, sensitivity_test_output_path, overwrite = TRUE)

# Also save mean raster as final layer
cat("Saving mean raster as final layer...\n")
final_raster <- rast(rescaled_mean_path)

writeRaster(final_raster, final_layer_output_path_no_mask, overwrite = TRUE)

print("Loading ag/urban mask...")
template_raster <- rast(template_raster_path)
ag_urban_mask <- rast(ag_urban_mask_path)
ag_urban_mask <- align_raster_to_template(input_raster = ag_urban_mask, 
                                          template_raster =  template_raster,
                                          input_type = "categorical")
rm(template_raster)

# mask to ag/urban inverse
print("Masking raster with ag/urban mask...")
masked_raster <- mask(final_raster, ag_urban_mask, inverse = TRUE)
rm(ag_urban_mask, final_raster)

writeRaster(masked_raster, final_layer_output_path_mask, overwrite = TRUE)

cat("\nAll processing complete.\n")
