wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to generate ecoregion assignments for the treemap data.
# The output will be a csv with the xy coordinate and the ecoregion assignment 
# for each point. This step must be completed to start on the next steps of 
# the analysis where we generate the indicators. With current parameters this 
# takes about 1.5 hours to complete

# -------------------------------
# 0) Packages and Parameters
# -------------------------------
library(sf)
library(data.table)
library(foreach)
library(doParallel)
library(progress)
library(terra)

# How many parallel workers?
num_workers <- 40

# How many rows per chunk? 
chunk_size  <- 2000000  

# Percent of the file to process (1 = 100%).  If you ever want a quick test on 1 million rows,
# set pct_to_process <- 0.001 (for ~0.1% of a 1 billion‐row file).
pct_to_process <- 1 

# file paths:
multi_domain_data_path = file.path(wri_project_root, "data", "multi_domain_data")
raw_treemap_data_base = paste0(multi_domain_data_path, "raw/treemap/from_publication_zip/Data/")
template_raster_path    <- paste0(raw_treemap_data_base, "TreeMap2016.tif")

merged_csv_path         <- paste0(multi_domain_data_path, "int/treemap/study_area_treemap_2016_all_layers.csv")
ecoregion_shapes_path   <- paste0(multi_domain_data_path, 
                                  "/int/boundary_layers/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp")

# Where to write each chunk’s “(rowID, x,y, NA_L3KEY)” output:
chunk_output_dir <- paste0(multi_domain_data_path, "/int/treemap/iter_chunks_for_ecoregions/")

#  final single file
final_output_csv <- paste0(multi_domain_data_path, "/int/treemap/treemap_xy_with_ecoregion.csv")

# Create output folder if it doesn’t exist
if (!dir.exists(chunk_output_dir)) {
  dir.create(chunk_output_dir, recursive = TRUE)
}

# -------------------------------
# 1) Read “template” raster solely to grab its CRS
# -------------------------------
library(terra)  # only to read the raster’s CRS
template_rast <- rast(template_raster_path)
target_crs     <- crs(template_rast)  # e.g. “EPSG:5070” or PROJ4 string
rm(template_rast)
gc()

# -------------------------------
# 2) Load & reproject ecoregion polygons ONCE (master process)
# -------------------------------
message("Reading and reprojecting ecoregions (master)…")
ecoregions_master <- st_read(ecoregion_shapes_path, quiet = TRUE)
# Keep only the field “NA_L3KEY” plus geometry
ecoregions_master <- ecoregions_master[, c("NA_L3KEY", attr(ecoregions_master, "sf_column"))]
# Reproject to match SCANFI raster’s CRS
ecoregions_master <- st_transform(ecoregions_master, st_crs(target_crs))

# We do NOT keep a giant copy of merged CSV in memory. We'll read only x,y per chunk.

# -------------------------------
# 3) Figure out “x” and “y” column indices in the big CSV
# -------------------------------
header_cols <- names(fread(merged_csv_path, nrows = 0))
x_col <- which(header_cols == "X")
y_col <- which(header_cols == "Y")

if (length(x_col) != 1 || length(y_col) != 1) {
  stop("Cannot find exactly one column named 'X' or 'Y' in the CSV header.")
}
message("→ CSV column ‘X’ is index: ", x_col, 
        " ; CSV column ‘Y’ is index: ", y_col)

# -------------------------------
# 4) Count total rows in the merged CSV
# -------------------------------
wc_out      <- system(paste("wc -l", merged_csv_path), intern = TRUE)
total_lines <- as.numeric(sub(" .*", "", wc_out))
total_rows  <- total_lines - 1  # subtract 1 for header
message("Total data rows in CSV: ", format(total_rows, big.mark = ","))

effective_rows <- round(total_rows * pct_to_process)
message("→ Will process ", format(effective_rows, big.mark = ","), 
        " rows (", pct_to_process * 100, "%).")

# -------------------------------
# 5) Compute how many chunks
# -------------------------------
total_chunks <- ceiling(effective_rows / chunk_size)
message("Total chunks to process: ", total_chunks)

# -------------------------------
# 6) Set up parallel cluster & export objects to each worker
# -------------------------------

message("Setting up ", num_workers, " workers in a cluster…")
cl <- makeCluster(num_workers)
registerDoParallel(cl)

# Export to each worker:
#  • target_crs             (CRS string or object for sf)
#  • ecoregions_master      (the full polygon layer, pre‐reprojected)
#  • merged_csv_path, x_col, y_col, chunk_size, effective_rows
clusterExport(cl, 
              varlist = c("target_crs", 
                          "ecoregions_master", 
                          "merged_csv_path", 
                          "x_col", "y_col", 
                          "chunk_size", 
                          "effective_rows"),
              envir = environment())

# Each worker must load sf + data.table and keep a local copy of ecoregions_master
clusterEvalQ(cl, {
  library(sf)
  library(data.table)
  # `ecoregions_master` is already exported; each worker can use it directly.
  NULL
})

# -------------------------------
# 7) Progress bar (master)
# -------------------------------
pb <- progress_bar$new(
  total  = total_chunks,
  format = "  Chunk :current/:total [:bar] :percent  eta: :eta"
)
progress <- function() {
  pb$tick()
}

# -------------------------------
# 8) Parallel foreach: process each chunk of rows
# -------------------------------
message("Commencing parallel chunk‐processing with sf…")
start_time <- Sys.time()

foreach(chunk_id = 1:total_chunks,
        .packages = c("sf","data.table"),
        .options.multicore = list(progress = progress)
) %dopar% {
  # ==== Inside each worker for chunk ‘chunk_id’ ====
  
  # 8.1) Determine which rows in the original CSV this chunk covers
  start_row <- (chunk_id - 1) * chunk_size + 1
  rows_left <- effective_rows - ((chunk_id - 1) * chunk_size)
  this_nrows <- if (rows_left >= chunk_size) chunk_size else rows_left
  
  # 8.2) Read only “x” and “y” for this chunk (header=FALSE, select by position)
  dt_chunk <- fread(
    merged_csv_path,
    skip      = start_row,
    nrows     = this_nrows,
    header    = FALSE,
    select    = c(x_col, y_col),
    col.names = c("X","Y")
  )
  
  # If, for some reason, no rows were read, skip
  if (nrow(dt_chunk) == 0) {
    return(NULL)
  }
  
  # 8.3) Convert dt_chunk → sf POINTS, using target_crs
  pts_chunk_sf <- st_as_sf(
    dt_chunk,
    coords = c("X","Y"),
    crs    = target_crs,
    remove = FALSE
  )
  
  # 8.4) Spatial join: get NA_L3KEY for points that fall inside polygons
  #      Left‐join style: all pts_chunk_sf keep their row order; those that fall inside get a new column NA_L3KEY
  joined <- st_join(pts_chunk_sf, 
                    ecoregions_master["NA_L3KEY"], 
                    join = st_intersects)
  
  # joined now has columns: x, y, geometry (POINT), NA_L3KEY (from polygons or NA)
  
  # 8.5) Nearest‐neighbor fallback for any points with NA
  missing_idx <- which(is.na(joined$NA_L3KEY))
  if (length(missing_idx) > 0) {
    # Find the nearest polygon index for each missing point
    pts_missing_sf <- joined[missing_idx, ]
    nn_idx <- st_nearest_feature(pts_missing_sf, ecoregions_master)
    # Fill them in:
    joined$NA_L3KEY[missing_idx] <- ecoregions_master$NA_L3KEY[nn_idx]
  }
  
  # 8.6) Build a small data.table for this chunk: (orig_row_id, x, y, NA_L3KEY)
  chunk_dt <- data.table(
    orig_row_id = seq(start_row, length.out = nrow(dt_chunk)),
    x           = dt_chunk$x,
    y           = dt_chunk$y,
    NA_L3KEY    = joined$NA_L3KEY
  )
  
  # 8.7) Write out this chunk’s CSV
  out_file <- file.path(chunk_output_dir, sprintf("chunk_%04d.csv", chunk_id))
  fwrite(chunk_dt, out_file)
  
  # 8.8) Clean up
  rm(dt_chunk, pts_chunk_sf, joined, chunk_dt, pts_missing_sf)
  gc()
  
  NULL
}  # end foreach

end_time <- Sys.time()
message("Parallel processing complete.")
message("  Start:   ", format(start_time, "%Y-%m-%d %H:%M:%S"))
message("  End:     ", format(end_time,   "%Y-%m-%d %H:%M:%S"))
message("  Elapsed: ", 
        round(difftime(end_time, start_time, units="mins"), 2), 
        " minutes")

stopCluster(cl)

# -------------------------------
# 9) Combine all chunk CSVs into one giant table
# -------------------------------
# WARNING: If you truly have ~1 billion rows, reading all at once will blow RAM.
all_chunk_files <- list.files(chunk_output_dir, 
                              pattern = "^chunk_.*\\.csv$", 
                              full.names = TRUE)
all_assignments  <- rbindlist(lapply(all_chunk_files, fread))
fwrite(all_assignments, final_output_csv)
message("Wrote final combined file to: ", final_output_csv)