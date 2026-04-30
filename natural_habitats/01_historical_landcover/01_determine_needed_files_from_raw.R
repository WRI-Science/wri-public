#The goal of this script is to extract the historical landcover data downloaded 
# and get only the layers we need. Once we have those layers they need to be combined 
# and set up for our study region.

#Source: https://zenodo.org/records/15063683

# Required packages
library(terra)
library(stringr)
library(future)
library(future.apply)  # for parallel processing

# File paths
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/"
raw_data_file_path <- file.path(data_file_path, "natural_habitats/raw/")
historical_landcover_raw_data_file_path <- file.path(raw_data_file_path, "historical_landcover")
int_data_file_path <- file.path(data_file_path, "natural_habitats/int/")
historical_landcover_int_data_file_path <- file.path(int_data_file_path, "historical_landcover")
save_path_2005_data <- file.path(historical_landcover_int_data_file_path, "2005/all_raw_tifs/")
save_path_2015_data <- file.path(historical_landcover_int_data_file_path, "2015/all_raw_tifs/")

# Number of cores to use for parallel processing (adjust as needed)
num_cores <- 24  

# Create output directories if they don't exist
if (!dir.exists(save_path_2005_data)) dir.create(save_path_2005_data, recursive = TRUE)
if (!dir.exists(save_path_2015_data)) dir.create(save_path_2015_data, recursive = TRUE)

# Identify all subfolders to process, excluding 'archive'
fldrs <- list.dirs(historical_landcover_raw_data_file_path, full.names = TRUE, recursive = FALSE)
fldrs <- fldrs[basename(fldrs) != "archive"]

# Gather all Annual V1.1 TIFF files across those subfolders
pattern <- "_Annual_V1\\.1\\.tif$"
tif_files <- unlist(lapply(fldrs, list.files, pattern = pattern, full.names = TRUE))

# Function to process a single TIFF
tprocess_tif <- function(tif) {
  fname <- basename(tif)
  
  # Load raster
  r <- rast(tif)
  
  # Check layer count
  if (nlyr(r) != 23) {
    return(list(status = "bad", file = tif, reason = sprintf("wrong layer count (%d)", nlyr(r))))
  }
  
  # Extract extent code E/W with N only; skip S extents silently
  extent_code <- str_extract(fname, "[EW]\\d+N\\d+")
  if (is.na(extent_code)) {
    return(list(status = "skip", file = tif, reason = "extent not E/W with N"))
  }
  
  # Subset layers for 2005 and 2015
  idx_2005 <- 2005 - 2000 + 1
  idx_2015 <- 2015 - 2000 + 1
  lc2005 <- r[[idx_2005]]; names(lc2005) <- "landcover_2005"
  lc2015 <- r[[idx_2015]]; names(lc2015) <- "landcover_2015"
  
  # Write to respective directories
  out2005 <- file.path(save_path_2005_data, paste0(extent_code, "_landcover_2005.tif"))
  out2015 <- file.path(save_path_2015_data, paste0(extent_code, "_landcover_2015.tif"))
  writeRaster(lc2005, out2005, overwrite = TRUE)
  writeRaster(lc2015, out2015, overwrite = TRUE)
  
  return(list(status = "ok", file = tif))
}

# Parallel processing
plan(multisession, workers = num_cores)
results <- future_lapply(tif_files, tprocess_tif)

# Collate results
res_df <- do.call(rbind, lapply(results, function(x) data.frame(file = x$file, status = x$status, reason = ifelse(is.null(x$reason), NA, x$reason), stringsAsFactors = FALSE)))

# List bad files (excluding skips)
bad_files <- res_df$file[res_df$status == "bad"]
if (length(bad_files) > 0) {
  message("Files that failed processing:")
  print(bad_files)
} else {
  message("No bad files encountered.")
}

# Summary counts
n_ok <- sum(res_df$status == "ok")
n_skip <- sum(res_df$status == "skip")
n_bad <- length(bad_files)
message(sprintf("Summary: processed=%d, skipped(E/S)=%d, bad=%d", n_ok, n_skip, n_bad))

gc()