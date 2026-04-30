#### Goal #### 
# the goal of this script is to take the annual precipication files and make 
# the 30 year stack, dataframe, and summary statistics we will use for rescaling.
# This step does not need to be run if the 30 year ouptputs already exist, 
# which the script will check for. With 12 workers this took about 30 min to run.

#### Packages ####
library(terra)

#### Files Paths and Setup ####
# use 1991-2020 for the 30 year stack
process_years <- 1991:2020

natural_habitats_root <- "/home/shares/wwri-wildfire/data/natural_habitats/"
annual_ppt_path <- paste0(natural_habitats_root, "int/terraclimate/ppt_annual_sums/")
ppt_1991_2020_path <- paste0(natural_habitats_root, "int/terraclimate/ppt_1991_2020/")

# if ppt_1991_2020_path does not exist, make it
if (!dir.exists(ppt_1991_2020_path)) {
  dir.create(ppt_1991_2020_path, recursive = TRUE)
  message("Created output directory: ", ppt_1991_2020_path)
}

# if the final output file exists skip the rest of the script
if (file.exists(paste0(ppt_1991_2020_path, "ppt_1991_2020_summary_stats.csv"))) {
  message("Output file already exists. Exiting script.")
  quit()
}

# get ppt files
files <- file.path(annual_ppt_path, sprintf("ppt_annual_%d.tif", process_years))

multi_domain_root <- "/home/shares/wwri-wildfire/data/multi_domain_data/"
study_region_shape_path <- paste0(multi_domain_root, "/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")

# template raster
template_raster_path <- file.path(multi_domain_root, "/int/boundary_layers",
                                  "admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

#### Main Processing ####
# Read all annual rasters into one SpatRaster (30 layers)
ppt_stack <- rast(files)
names(ppt_stack) <- as.character(process_years)  # optional: name layers by year

# Read study‐area polygon and reproject to raster CRS
# don't need to make moll crs because we are just getting to our area
study_vec    <- vect(study_region_shape_path)
study_vec_pr <- project(study_vec, crs(ppt_stack))

# Crop the stack to the study‐area extent
ppt_crop <- crop(ppt_stack, study_vec_pr)

# Mask: retain all cells that intersect or touch the polygon
ppt_masked <- mask(ppt_crop, study_vec_pr, touches = TRUE)

# Now save the masked stack
output_path <- file.path(ppt_1991_2020_path, "ppt_annual_1991_2020_wwri_study_area.tif")
writeRaster(
  ppt_masked,
  filename = output_path,
  filetype = "GTiff",
  overwrite = TRUE,
  gdal     = c("COMPRESS=LZW")
)

# Coerce to data.frame with cell numbers and coordinates
df <- as.data.frame(ppt_masked, 
                    cells = TRUE,   # adds a column "cell" with the terra cell ID
                    xy    = TRUE,   # adds columns "x" and "y" with coords
                    na.rm = FALSE)  # keep NA rows if you want (default)

# Identify the precipitation columns by year
year_cols <- as.character(1991:2020)

# Flag rows with at least one non-NA value
keep_rows <- rowSums(!is.na(df[, year_cols])) > 0

# Subset the full table to those rows
df_filtered <- df[keep_rows, ]

# Write the full set of columns to CSV
write.csv(
  df_filtered,
  file = paste0(ppt_1991_2020_path, "ppt_annual_1991_2020_wwri_study_area.csv"),
  row.names = FALSE
)

cat(
  "Exported", nrow(df_filtered),
  "rows (cells) with ≥1 valid year, including x, y, cell, and all precipitation layers.\n"
)

# Load data
df <- read.csv(paste0(ppt_1991_2020_path, "ppt_annual_1991_2020_wwri_study_area.csv"))

# Define year columns using actual column names (e.g., X1991 to X2020)
year_cols <- paste0("X", 1991:2020)

# Compute summary statistics
df$mean_ppt  <- rowMeans(df[, year_cols], na.rm = TRUE)
df$min_ppt   <- apply(df[, year_cols], 1, min, na.rm = TRUE)
df$max_ppt   <- apply(df[, year_cols], 1, max, na.rm = TRUE)
df$sd_ppt    <- apply(df[, year_cols], 1, sd, na.rm = TRUE)
df$range_ppt <- df$max_ppt - df$min_ppt
df$median_ppt <- apply(df[, year_cols], 1, median, na.rm = TRUE)

# Percentiles: quantile returns a named vector, so we extract specific values
df$p1_ppt  <- apply(df[, year_cols], 1, function(x) quantile(x, probs = 0.01, na.rm = TRUE))
df$p99_ppt <- apply(df[, year_cols], 1, function(x) quantile(x, probs = 0.99, na.rm = TRUE))

# Select summary columns to save
summary_output <- df[, c("x", "y", "cell", "mean_ppt", "min_ppt", "max_ppt",
                         "sd_ppt", "range_ppt", "median_ppt", "p1_ppt", "p99_ppt")]

# Save to CSV
write.csv(
  summary_output,
  file = paste0(ppt_1991_2020_path, "ppt_1991_2020_summary_stats.csv"),
  row.names = FALSE
)

cat("Extended summary stats saved for", nrow(summary_output), "cells.\n")