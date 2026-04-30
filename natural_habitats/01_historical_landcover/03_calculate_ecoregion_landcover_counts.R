#### Goal ####
# The goal of this script is to take the merged area file from step 2 and calculate the counts
# of each landcover designation in the ecoregions that intersect the study area.
# The output of this script will go into step 4 which will calculate the % natural of each ecoregion
# based on the landcover desinations.
# In total this takes about 3.5 hours to run currently. Ecoregion processing 
# could be parallelized but that has not been done yet

#### Packages ####
library(terra)
library(dplyr)
library(tidyr)
library(parallel)  # For potential parallel processing

# Set memory options for terra
terraOptions(memfrac=0.8)  # Use up to 80% of available memory

#### File Paths ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/"
raw_data_file_path <- file.path(data_file_path, "natural_habitats/raw/")
int_data_file_path <- file.path(data_file_path, "natural_habitats/int/")
historical_landcover_int_data_file_path <- file.path(int_data_file_path, "historical_landcover")

# Land cover raster paths
merged_2005_landcover_path <- file.path(historical_landcover_int_data_file_path, "2005/historical_landcover_merged_2005.tif")
merged_moll_crs_2005_landcover_path <- file.path(historical_landcover_int_data_file_path, "2005/historical_landcover_merged_2005_moll_crs.tif")
merged_2015_landcover_path <- file.path(historical_landcover_int_data_file_path, "2015/historical_landcover_merged_2015.tif")
merged_moll_crs_2015_landcover_path <- file.path(historical_landcover_int_data_file_path, "2015/historical_landcover_merged_2015_moll_crs.tif")

# Ecoregion shapefile path
intersecting_ecoregions_shapefile_path <- file.path(multi_domain_data_file_path, 
                                                    "int/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp")

#### Target CRS ####
moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

#### Read & dissolve ecoregion shapes ####
message("Reading raw ecoregion shapes…")
ecoregions_geo <- vect(intersecting_ecoregions_shapefile_path)

message("Dissolving by NA_L3CODE…")
ecoregions_combined <- aggregate(ecoregions_geo, by = "NA_L3CODE")

# Copy over a representative name
ecoregions_combined$NA_L3NAME <- sapply(
  ecoregions_combined$NA_L3CODE,
  function(code) {
    unique(ecoregions_geo$NA_L3NAME[ecoregions_geo$NA_L3CODE == code])[1]
  }
)

message("Reprojecting all to Mollweide…")
ecoregions_moll <- project(ecoregions_combined, moll_crs)

#### Land Cover Classification Dictionary ####
# Complete land cover classification values and descriptions
landcover_classes <- data.frame(
  lc_id = c(0, 10, 11, 12, 20, 51, 52, 61, 62, 71, 72, 81, 82, 91, 92, 
            120, 121, 122, 130, 140, 150, 152, 153, 181, 182, 183, 184, 
            185, 186, 187, 190, 200, 201, 202, 210, 220, 250),
  lc_description = c("Filled value", "Rainfed cropland", "Herbaceous cover cropland", 
                     "Tree or shrub cover (Orchard) cropland", "Irrigated cropland", 
                     "Open evergreen broadleaved forest", "Closed evergreen broadleaved forest", 
                     "Open deciduous broadleaved forest (0.15<fc<0.4)", "Closed deciduous broadleaved forest (fc>0.4)", 
                     "Open evergreen needle-leaved forest (0.15< fc <0.4)", "Closed evergreen needle-leaved forest (fc >0.4)", 
                     "Open deciduous needle-leaved forest (0.15< fc <0.4)", "Closed deciduous needle-leaved forest (fc >0.4)", 
                     "Open mixed leaf forest (broadleaved and needle-leaved)", "Closed mixed leaf forest (broadleaved and needle-leaved)", 
                     "Shrubland", "Evergreen shrubland", "Deciduous shrubland", "Grassland", "Lichens and mosses", 
                     "Sparse vegetation (fc<0.15)", "Sparse shrubland (fc<0.15)", "Sparse herbaceous (fc<0.15)", 
                     "Swamp", "Marsh", "Flooded flat", "Saline", "Mangrove", "Salt marsh", "Tidal flat", 
                     "Impervious surfaces", "Bare areas", "Consolidated bare areas", "Unconsolidated bare areas", 
                     "Water body", "Permanent ice and snow", "Filled value")
)

#### Main Processing Functions ####

# Function to process a single ecoregion
process_ecoregion <- function(eco_polygon, landcover_raster, landcover_classes) {
  # Extract ecoregion attributes
  eco_id <- eco_polygon$NA_L3CODE
  eco_name <- eco_polygon$NA_L3NAME
  
  # Ensure both datasets are in the same CRS
  message("  Verifying coordinate reference systems match...")
  if (!identical(crs(eco_polygon, proj=TRUE), crs(landcover_raster, proj=TRUE))) {
    message("  CRS mismatch detected—reprojecting polygon on the fly")
    eco_polygon <- project(eco_polygon, crs(landcover_raster))
  }
  
  # Get bounding box of the ecoregion to crop raster (for efficiency)
  eco_extent <- ext(eco_polygon)
  
  # Crop the raster to the ecoregion extent
  message("  Cropping raster to ecoregion extent...")
  cropped_raster <- crop(landcover_raster, eco_extent)
  
  # Mask the cropped raster by the ecoregion boundary
  message("  Masking raster with ecoregion boundary...")
  masked_raster <- mask(cropped_raster, eco_polygon)
  
  # Free memory
  rm(cropped_raster)
  
  # Count cell frequencies
  message("  Calculating cell counts...")
  # get the two columns out of freq()
  fc <- as.data.frame(freq(masked_raster))
  
  # build your tidy table by explicitly assigning:
  class_counts <- data.frame(
    lc_id = fc[[ "value" ]],  # if terra named it "value"
    count = fc[[ "count" ]]   # and this one "count"
  )
  
  
  # Remove NA values if present
  class_counts <- class_counts[!is.na(class_counts$lc_id), ]
  
  # Free memory
  rm(masked_raster)
  gc()
  
  # Join with landcover class descriptions
  message("  Joining with landcover descriptions...")
  result <- class_counts %>%
    left_join(landcover_classes, by = "lc_id") %>%
    mutate(eco_id = eco_id,
           eco_name = eco_name)
  
  return(result)
}

process_year <- function(year) {
  # Determine file paths based on year
  if (year == 2005) {
    orig_path <- merged_2005_landcover_path
    moll_path <- merged_moll_crs_2005_landcover_path
  } else if (year == 2015) {
    orig_path <- merged_2015_landcover_path
    moll_path <- merged_moll_crs_2015_landcover_path
  } else {
    stop("Invalid year specified. Must be 2005 or 2015.")
  }
  
  # Output CSV path
  output_csv <- file.path(historical_landcover_int_data_file_path, 
                          paste0("landcover_by_ecoregion_", year, ".csv"))
  
  # If results CSV already exists, read and return
  if (file.exists(output_csv)) {
    message(paste0("Output CSV for ", year, " already exists. Reading existing file..."))
    return(read.csv(output_csv))
  }
  
  # Check for existing Mollweide projection
  if (file.exists(moll_path)) {
    message(paste0("Found existing Mollweide raster for ", year, ". Reading ..."))
    landcover <- rast(moll_path)
  } else {
    # Read the original landcover raster
    message(paste0("Reading ", year, " landcover data..."))
    landcover <- rast(orig_path)
    message(paste0("Original CRS: ", crs(landcover)))
    
    # Crop to study area extent in geographic CRS
    message("Cropping to study area extent (geographic)...")
    landcover_crop <- crop(landcover, ext(ecoregions_geo))
    
    # Build a blank Mollweide template with desired extent & 30 m resolution
    message("Building Mollweide template (30m)...")
    template <- rast(
      ext = ext(ecoregions),       # extent in Mollweide
      res = 30,                     # 30 x 30 m cells
      crs = moll_crs                # Mollweide CRS
    )
    
    # Reproject using the template, nearest neighbor, writing to disk
    message("Reprojecting to Mollweide (30m) using template, saving ...")
    landcover <- project(
      x        = landcover_crop,   
      y        = template,         
      method   = "near",          
      filename = moll_path,        
      overwrite= TRUE
    )
    message("Mollweide raster saved to: ", moll_path)
  }
  
  # Check raster dimensions and resolution
  message(paste0("Raster dimensions: ", paste(dim(landcover), collapse=" x ")))
  message(paste0("Raster resolution: ", paste(res(landcover), collapse=" x "), " meters"))
  
  
  # Now process each ecoregion:
  message(paste0("\nProcessing ", year, " landcover by ecoregion…"))
  total_ecos <- nrow(ecoregions_moll)
  message("Total unique ecoregions: ", total_ecos)
  
  results_list <- vector("list", total_ecos)
  
  for (i in seq_len(total_ecos)) {
    eco_poly <- ecoregions_moll[i, ]
    eco_code <- eco_poly$NA_L3CODE
    eco_name <- eco_poly$NA_L3NAME
    message(paste0("  [", i, "/", total_ecos, "] ", eco_code, " – ", eco_name))
    
    # run your robust function (with auto‐reproject inside)
    res_df <- tryCatch(
      process_ecoregion(eco_poly, landcover, landcover_classes),
      error = function(e) {
        message("    ERROR: ", e$message)
        return(NULL)
      }
    )
    if (!is.null(res_df) && nrow(res_df)>0) {
      results_list[[i]] <- res_df
    }
    if (i %% 10 == 0) { gc(); message("    Memory cleaned.") }
  }
  
  # bind, save, return as before…
  valid <- results_list[!vapply(results_list, is.null, logical(1))]
  final_tbl <- bind_rows(valid) %>%
    select(eco_id, eco_name, lc_id, lc_description, count) %>%
    mutate(year = year)
  
  write.csv(final_tbl,
            file.path(historical_landcover_int_data_file_path,
                      paste0("landcover_by_ecoregion_", year, ".csv")),
            row.names = FALSE)
  message("Saved output for ", year)
  
  return(final_tbl)
}

#### Main Script ####

start_time <- Sys.time()

# Preprocessed shapes live in 'ecoregions_moll' already
message("---- Processing 2005 ----")
out2005 <- process_year(2005)

message("---- Processing 2015 ----")
out2015 <- process_year(2015)

message("Total time: ", round(difftime(Sys.time(), start_time, units="mins"),2), " mins")