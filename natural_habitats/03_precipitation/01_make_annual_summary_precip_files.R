wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to make an annual summary file for each year of the 
# terraclimate data. This step does not need to be run if the annual summary outputs already exist, 
# which the script will check for.

#### Packages ####
library(terra)
library(parallel)

#### Files Paths and Setup ####

natural_habitats_root <- file.path(wri_project_root, "data", "natural_habitats")
raw_ppt_path <- paste0(natural_habitats_root, "raw/terraclimate/ppt/")

output_dir <- paste0(natural_habitats_root, "int/terraclimate/ppt_annual_sums/")

# if the output directory does not exist, make it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

# establish the number of cores to use
num_cores <- 12

#### Functions ####
sum_monthly_to_annual <- function(r, na.rm = TRUE) {
  if (!inherits(r, "SpatRaster")) {
    stop("Input must be a terra SpatRaster.")
  }
  if (nlyr(r) != 12) {
    warning("Expected 12 layers; found ", nlyr(r), " layers.")
  }
  # force single‐threaded inside each worker
  out <- app(r,
             fun   = function(x) sum(x, na.rm = na.rm),
             cores = 1)
  return(out)
}

process_one_year <- function(nc_path, output_dir, na.rm) {
  year      <- sub(".*_([0-9]{4})\\.nc$", "\\1", basename(nc_path))
  message("Worker on year ", year)
  
  # read, sum, name
  r_monthly <- rast(nc_path)
  annual    <- sum_monthly_to_annual(r_monthly, na.rm = na.rm)
  names(annual) <- paste0("ppt_annual_", year)
  
  # write compressed GeoTIFF
  out_fn <- file.path(output_dir,
                      paste0("ppt_annual_", year, ".tif"))
  writeRaster(
    annual,
    filename  = out_fn,
    filetype  = "GTiff",
    overwrite = TRUE,
    gdal   = c("COMPRESS=DEFLATE")
  )
  message(" → Written: ", out_fn)
  
  # cleanup
  rm(r_monthly, annual)
  tmpFiles(remove = TRUE)
  gc()
  
  return(out_fn)
}

process_parallel <- function(input_dir,
                             output_dir,
                             na.rm    = TRUE,
                             ncores   = 8) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  nc_files <- list.files(input_dir,
                         pattern    = "^TerraClimate_ppt_[0-9]{4}\\.nc$",
                         full.names = TRUE)
  if (length(nc_files) == 0) {
    stop("No .nc files found in ", input_dir)
  }
  
  cl <- makeCluster(ncores)
  # ensure each worker loads terra
  clusterEvalQ(cl, library(terra))
  # export helper & parameters
  clusterExport(cl, c("sum_monthly_to_annual", "process_one_year"),
                envir = environment())
  
  # run in parallel
  results <- parLapply(
    cl,
    nc_files,
    fun      = process_one_year,
    output_dir = output_dir,
    na.rm       = na.rm
  )
  
  stopCluster(cl)
  message("All done: processed ", length(results), " years.")
  return(results)
}
#### Main Processing ####

# Check if processing is needed
nc_files <- list.files(raw_ppt_path, pattern = "^TerraClimate_ppt_[0-9]{4}\\.nc$")
tif_files <- list.files(output_dir, pattern = "^ppt_annual_[0-9]{4}\\.tif$")

if (length(nc_files) == length(tif_files)) {
  message("Annual summary files already exist for all years. Skipping processing.")
} else {
  #### Main Processing ####
  outs <- process_parallel(
    input_dir  = raw_ppt_path,
    output_dir = output_dir,
    na.rm      = TRUE,
    ncores     = num_cores   
  )
  print(outs)
}
