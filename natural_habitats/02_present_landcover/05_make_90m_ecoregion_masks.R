#### Goal ####

# The goal of this is script is to take the 10m raster data made in step 5 and make them into 90m rasters.
# Once we have 90m rasters, we can join them into one large mask for the desired data layer.
# The ag/urban mask will be used to remove ag/urban areas from the natural habitats data layer.
# In it's present state this takes about 3 hours to run on Aurora using 4 cores.

#### Warning ####
# Rerunning this script with its current settings takes 21 hours! 

#### Packages ####
library(tidyverse)
library(terra)
library(pbmcapply)

#### File paths and setup ####
year        <- 2023
redo_all    <- TRUE
n_cores     <- 12                   
terraOptions(threads = 2)           # keep 2 threads per worker
terraOptions(memfrac = 0.6)         # use up to 60% RAM per process
options(terra.mem.limit = 8000)     # MB

data_root    <- "/home/shares/wwri-wildfire/data/natural_habitats"
present_landcover_path <- file.path(data_root,
                                    "int/esri_present_landcover",
                                    as.character(year))
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

template_raster_path <- file.path(
  "/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/",
  "admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"
)
template_raster <- rast(template_raster_path)

moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

masks <- list(
  ag_urban    = list(in_dir = file.path(present_landcover_path, "ag_urban_moll_10m"),
                     out_dir = file.path(present_landcover_path, "ag_urban_5070_90m")),
  rangeland   = list(in_dir = file.path(present_landcover_path, "rangeland_moll_10m"),
                     out_dir = file.path(present_landcover_path, "rangeland_5070_90m")),
  bare_ground = list(in_dir = file.path(present_landcover_path, "bare_ground_moll_10m"),
                     out_dir = file.path(present_landcover_path, "bare_ground_5070_90m"))
)

#### Function to process a single mask ####
process_mask <- function(mask_name, paths) {
  cat("\n=======================================\n")
  cat("Processing mask:", mask_name, "\n")
  cat("=======================================\n")
  dir.create(paths$out_dir, showWarnings = FALSE, recursive = TRUE)
  
  tif_files  <- list.files(paths$in_dir, pattern="\\.tif$", full.names=TRUE)
  total_files <- length(tif_files)
  cat("Found", total_files, "tiles to process for", mask_name, "\n")
  
  aggregate_raster <- function(tif_file) {
    base_name   <- tools::file_path_sans_ext(basename(tif_file))
    output_file <- file.path(paths$out_dir, paste0(base_name, ".tif"))
    
    if (file.exists(output_file) && !redo_all) {
      return(list(status="skipped", file=base_name))
    }
    
    #### Step 1: Reproject to EPSG:5070 at 10 m resolution ####
    cat("- [", base_name, "] Step 1/3: Reproject → EPSG:5070 (10 m)\n")
    rast_in <- rast(tif_file)
    crs(rast_in) <- moll_crs
    rast10_5070 <- project(
      rast_in,
      "EPSG:5070",
      method = "near",
      res    = c(10, 10)
    )
    
    #### Step 2: Aggregate 9×9 (10 m → 90 m) using max ####
    cat("- [", base_name, "] Step 2/3: Aggregate 9×9 → 90 m (max)\n")
    agg90 <- aggregate(
      rast10_5070,
      fact  = 9,
      fun   = max,
      na.rm = TRUE
    )
    
    #### Step 3: Resample onto 90 m template ####
    cat("- [", base_name, "] Step 3/3: Resample → 90 m template\n")
    final90 <- resample(
      agg90,
      template_raster,
      method = "near"
    )
    
    writeRaster(
      final90,
      output_file,
      overwrite = TRUE,
      gdal      = c("COMPRESS=LZW")
    )
    
    # cleanup memory
    rm(rast_in, rast10_5070, agg90, final90); gc()
    list(status="done", file=base_name)
  }
  
  results <- pbmclapply(tif_files,
                        aggregate_raster,
                        mc.cores = n_cores)
  statuses <- sapply(results, `[[`, "status")
  
  cat("\nSummary for", mask_name, "mask:\n")
  cat(" Total:   ", total_files, "\n",
      " Done:    ", sum(statuses=="done"), "\n",
      " Skipped: ", sum(statuses=="skipped"), "\n",
      " Errors:  ", sum(statuses=="error"), "\n")
  
  if (any(statuses=="error")) {
    cat("\nErrors (up to 5):\n")
    errs <- results[statuses=="error"]
    for (e in errs[1:min(5, length(errs))]) {
      cat(" -", e$file, ":", e$msg, "\n")
    }
  }
  
  invisible(results)
}

#### Run Process for All Masks ####
for (mask_name in names(masks)) {
  process_mask(mask_name, masks[[mask_name]])
}