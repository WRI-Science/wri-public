wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to process the raw NDVI data from NASA AppEEARS. 
# Here, we calculate the rolling standard deviation of the NDVI data for our study region
# for each tif in the download (about every 16 days). The data is downloaded 
# from https://appeears.earthdatacloud.nasa.gov/ using the extraction portal.
# The portal lets you set a date range and draw an area on the map to extract NDVI data.
# You can select the tif option and in about 12 hours you will receive a link in your email to download the data.

# The output from this process will go into step 2 where we rescale per ecoregion using two methods.
# It takes about 3 minutes to run on aurora

#### Packages ####
library(terra)      # Raster processing
library(parallel)   # Parallel processing
library(foreach)    # Parallel loop execution
#library(doParallel) # Backend for foreach (still loaded but not used below)
library(doSNOW)     # Alternative backend with progress support

#### Paths and Variables ####
year_of_interest <- "2024"

natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")
raw_NDVI_path <- paste0(natural_habitats_base_path, "raw/NDVI/", year_of_interest, "_AppEEARS/NDVI/")
output_path <- paste0(natural_habitats_base_path, "int/NDVI/", year_of_interest, "/rolling_sd/")

multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
study_area_shape_path <- paste0(multi_domain_data_file_path, "/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")

num_cores <- 12  

#### Function for SD ####
calculate_NDVI_SD <- function(NDVI_tif, raw_NDVI_path, output_path, study_area_path) {
  message("Starting file: ", NDVI_tif)
  
  study_area <- vect(study_area_path)  
  NDVI_tif_raw <- rast(paste0(raw_NDVI_path, NDVI_tif))
  
  name_extract <- substr(x = names(NDVI_tif_raw), start = 35, stop = 41)
  reprojected_study_area <- project(x = study_area, y = NDVI_tif_raw)
  study_area_buffer <- buffer(x = reprojected_study_area, width = 2500, joinstyle = "round")
  NDVI_test_tif_masked <- mask(x = NDVI_tif_raw, mask = study_area_buffer, touches = TRUE)
  
  NDVI_SD <- focal(x = NDVI_test_tif_masked, w = 3, fun = sd, na.rm = TRUE)
  
  terra::writeRaster(x = NDVI_SD,
                     filename = paste0(output_path, "NDVI_SD_", name_extract, ".tif"),
                     overwrite = TRUE)
  
  rm(NDVI_tif_raw, NDVI_test_tif_masked, NDVI_SD, study_area, reprojected_study_area, study_area_buffer)
  gc()
  
  message("Finished file: ", NDVI_tif)
}

#### Process Rasters ####

# Ensure output directory exists
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# List all TIFF files in the directory
list_of_NDVI_files <- list.files(path = raw_NDVI_path, pattern = "\\.tif$", full.names = FALSE)

# Set up doSNOW cluster
cl <- makeCluster(num_cores)
registerDoSNOW(cl)

# Export necessary vars and functions to workers (use snow's env param)
clusterExport(cl,
              c("raw_NDVI_path", "output_path", "study_area_shape_path", "calculate_NDVI_SD"),
              env = environment())

# Create a text progress bar
total_files <- length(list_of_NDVI_files)
pb <- txtProgressBar(min = 0, max = total_files, style = 3)

# Define a progress callback
progress <- function(n) {
  setTxtProgressBar(pb, n)
  if (n %% ceiling(total_files * 0.10) == 0 || n %% 5 == 0) {
    pct <- round(n / total_files * 100)
    cat(sprintf("\n[%d/%d] %d%% complete\n", n, total_files, pct))
    flush.console()
  }
}

opts <- list(progress = progress)

# Run the parallel loop with progress
foreach(
  file = list_of_NDVI_files,
  .packages = c("terra", "sf"),
  .options.snow = opts
) %dopar% {
  calculate_NDVI_SD(file, raw_NDVI_path, output_path, study_area_shape_path)
}

# Clean up
close(pb)
stopCluster(cl)
closeAllConnections()

terra::tmpFiles(remove = TRUE)
gc()

cat("Processing complete!\n")
