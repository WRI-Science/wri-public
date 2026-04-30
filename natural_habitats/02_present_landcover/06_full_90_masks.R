wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal #### 
# The goal of this script is to take the 90m ecoregion layers make in step 5 and make them into a single mask.
# The ag/urban mask will be used to remove ag/urban areas from every natural habitats indicator layer.

# This takes about 30 minutes to run with 3 cores on Aurora.

#### Packages ####
library(tidyverse)
library(terra)
library(pbmcapply)
library(sf)

#### File Paths and Setup ####
year     <- 2023
redo_all <- TRUE

# setup number of cores for each operation
num_cores_clean <- 3   # for the tile‐cleaning step
num_cores_mask  <- 3    # for building the three masks

data_root               <- file.path(wri_project_root, "data", "natural_habitats")
present_landcover_path  <- file.path(data_root, "int/esri_present_landcover", as.character(year))

masks <- list(
  ag_urban = list(
    in_dir  = file.path(present_landcover_path, "ag_urban_5070_90m"),
    out_dir = file.path(present_landcover_path, "full_masks")
  ),
  rangeland = list(
    in_dir  = file.path(present_landcover_path, "rangeland_5070_90m"),
    out_dir = file.path(present_landcover_path, "full_masks")
  ),
  bare_ground = list(
    in_dir  = file.path(present_landcover_path, "bare_ground_5070_90m"),  
    out_dir = file.path(present_landcover_path, "full_masks")
  )
)

# Ensure output directory exists
dir.create(file.path(present_landcover_path, "full_masks"), recursive = TRUE, showWarnings = FALSE)

# Template Raster (for masking & extent/resolution)
template_raster_path <- file.path(
  file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers"),
  "admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"
)

source(here::here("templates_and_functions", "align_raster_to_template.R"))

template_raster <- rast(template_raster_path)
e <- ext(template_raster)
r <- res(template_raster)

#### Function to build one mask ####
# 1) CLEAN: write 0→NA into temp_90m_clean/
clean_ecoregion_rasters_no_zero <- function(mask_name, spec, num_cores_clean) {
  in_dir     <- spec$in_dir
  clean_dir  <- file.path(in_dir, "temp_90m_clean")
  if (!dir.exists(clean_dir)) dir.create(clean_dir)
  
  tiles <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  
  pbmclapply(
    tiles,
    function(tile) {
      r <- rast(tile)
      r[r == 0] <- NA
      out_tile <- file.path(clean_dir, basename(tile))
      writeRaster(r, out_tile, overwrite = TRUE)
      # clean up within the worker (though pbmclapply forks so this is less critical)
      rm(r); gc()
      return(out_tile)
    },
    mc.cores = num_cores_clean
  )
  
  message("Cleaned ", length(tiles), " tiles for mask ", mask_name,
          " → ", clean_dir)
  
  # remove the list of paths and force a GC
  rm(cleaned_tiles); gc()
  invisible(clean_dir)
}

# 2) BUILD: merge the cleaned tiles into the final full mask
make_full_mask <- function(mask_name, spec) {
  out_file <- file.path(
    spec$out_dir,
    paste0("full_", mask_name, "_mask_90m_5070.tif")
  )
  if (!redo_all && file.exists(out_file)) {
    message("Skipping existing: ", mask_name)
    return(out_file)
  }
  
  clean_dir <- file.path(spec$in_dir, "temp_90m_clean")
  cleaned_tiles <- list.files(clean_dir, pattern = "\\.tif$", full.names = TRUE)
  
  message("Building mask from cleaned tiles: ", mask_name)
  
  # 1) VRT (nodata already carried in from the cleaned TIFFs)
  vrt_file <- tempfile(paste0("vrt_", mask_name, "_"), fileext = ".vrt")
  sf::gdal_utils(
    "buildvrt",
    source      = cleaned_tiles,
    destination = vrt_file,
    options     = c("-vrtnodata", "")
  )
  # done with the list of cleaned_tiles → free memory
  rm(cleaned_tiles); gc()
  
  # 2) Warp (crop + resample + merge)
  warp_opts <- c(
    "-te", e[1], e[3], e[2], e[4],
    "-tr", r[1], r[2],
    "-r",  "near"
  )
  sf::gdal_utils(
    "warp",
    source      = vrt_file,
    destination = out_file,
    options     = warp_opts
  )
  
  # 3) Align 
  m  <- rast(out_file)
  mm <- align_raster_to_template(
    input_raster    = m,
    template_raster = template_raster,
    input_type      = "categorical"
  )
  
  writeRaster(mm, filename = out_file, overwrite = TRUE)
  
  # cleanup large SpatRasters, the VRT, then GC
  rm(m, mm); unlink(vrt_file); gc()
  
  message("Done: ", mask_name, " → ", out_file)
  return(out_file)
}

#### Test functions ####
# uncomment to test the functions
# redo_all         <- TRUE
# num_cores_clean  <- 3    # for the cleaning step
# num_cores_mask   <- 1     # only building one mask
# mask_name        <- "ag_urban"
# spec             <- masks[[mask_name]]
# 
# # ---- 1) CLEAN THE ag_urban TILES ----
# message("Cleaning tiles for mask: ", mask_name)
# clean_dir <- clean_ecoregion_rasters_no_zero(
#   mask_name, spec, num_cores_clean
# )
# message("Cleaned tiles are in: ", clean_dir)
# # (Optional) list the first few cleaned files:
# print(list.files(clean_dir, pattern="\\.tif$", full.names=TRUE)[1:5])
# 
# # free any leftover lists/rasters from cleaning
# if (exists("cleaned_tiles")) rm(cleaned_tiles)
# gc()
# 
# 
# # ---- 2) BUILD THE FULL ag_urban MASK ----
# message("Building full mask for: ", mask_name)
# out_file <- make_full_mask(mask_name, spec)
# message("Generated mask at: ", out_file)
# 
# # ---- 3) QUICK VISUAL CHECK ----
# r <- rast(out_file)
# plot(r, main = paste0(mask_name, " full mask (90m, only 1 → present)"))

#### Main: run in parallel ####
# ---- 1) CLEAN STEP (parallel over tiles within each mask) ----
pbmclapply(
  names(masks),
  function(name) {
    clean_ecoregion_rasters_no_zero(name, masks[[name]], num_cores_clean)
  },
  mc.cores = num_cores_mask
)
# free any leftover lists/rasters from cleaning
if (exists("cleaned_tiles")) rm(cleaned_tiles)
gc()

# ---- 2) MASK BUILD STEP (parallel over the three masks) ----
pbmclapply(
  names(masks),
  function(name) {
    make_full_mask(name, masks[[name]])
  },
  mc.cores = num_cores_mask
)

gc()

