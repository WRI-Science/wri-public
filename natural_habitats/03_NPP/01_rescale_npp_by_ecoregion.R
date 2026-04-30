wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf) # For reading and manipulating spatial data
library(tidyverse) # For data manipulation
library(terra) # For raster processing
library(here) # To assemble file paths within project

#### Script Overview ####
# The purpose of this script is to prepare the NPP data from the MOD17A3HGF product (https://lpdaac.usgs.gov/products/mod17a3hgfv061/). We access this data through the APPEEARS service (https://appeears.earthdatacloud.nasa.gov), which prepares data by year to your study area/bounding box for you and saves steps of processing.

# User guide may be helpful if you have questions: https://lpdaac.usgs.gov/documents/972/MOD17_User_Guide_V61.pdf


#### Base Directories ####

multi_domain_data_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "natural_habitats")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "natural_habitats")

# Set up equal area crs for rescaling by ecoregion
moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"


#### Data Layers ####

# Read in template raster to resample to at end
study_area_rast <- rast(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

# Read in ecoregions that intersect with our study area
na_ecoregions_study_area <- st_read(file.path(multi_domain_data_path, "int", "boundary_layers", "epa_ecoregions_north_america_level_iii", "intersecting_ecoregion_shapes", "ecoregions_intersecting_study_area.shp"))

# Read in npp data for 2024
npp <- rast(file.path(data_file_path, "npp", "MOD17A3HGF.061_Npp_500m_doy2024001_aid0001.tif"))


#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))


#### Data Processing ####
# Edit ecoregions file for this script's purposes
na_ecoregions_study_area <- na_ecoregions_study_area %>%
  select(NA_L3CODE) %>%
  st_transform(., st_crs(npp))

na_ecoregions_study_area_moll <- na_ecoregions_study_area %>%
  select(NA_L3CODE) %>%
  st_transform(., moll_crs)

# Round the values in the raster
npp_rounded <- round(npp, digits = 4) # Necessary because otherwise it doesn't recognize the values properly in the next step

# Reclassify fill values for our purposes

# Codes defined here: https://lpdaac.usgs.gov/products/mod17a3hgfv061/ under Npp_500m Fill Value Classes
# Pasted below as well for convenience:

# Value	Description
# 32761	Land cover assigned as "unclassified" or not able to determine
# 32762	Land cover assigned as urban/built-up
# 32763	Land cover assigned as "permanent" wetlands/inundated marshland
# 32764	Land cover assigned as perennial snow, ice
# 32765	Land cover assigned as barren, sparse veg (rock, tundra, desert)
# 32766	Land cover assigned as perennial salt or inland fresh water
# 32767	Fill Value

# In the data, these are represented as 3.2761, 3.2762, 3.2763, 3.2764, 3.2765, and 3.2766 (3.2767 is not in the data)
# 3.2763, 3.2764, 3.2765 get classified as 0 - these would likely have 0 NPP
# 3.2761, 3.2762, 3.2766, 3.2767 get classified as NA - these would likely not get NPP values

npp_processed <- ifel(npp_rounded %in% c(3.2763, 3.2764, 3.2765), 0, npp_rounded)
npp_processed <- ifel(npp_processed %in% c(3.2761, 3.2762, 3.2766, 3.2767), NA, npp_processed) # Including 3.2767 in case it is present in the future

# See how it looks after reclassification
plot(npp_processed) # Looks good


# Process the npp data for further use
# Note: If you don't classify before everything else, it can skew the values near edges of water bodies especially, so changing the order may not produce as accurate of results
npp_processed <- npp_processed %>%
  crop(., na_ecoregions_study_area) %>% # Crop to our study area ecoregions; leaving this first crop in reduces the amount of warnings
  project(., moll_crs) %>% # Project to our desired crs for equal area rescaling
  crop(., na_ecoregions_study_area_moll) %>% # Re-crop to ensure projecting didn't skew things
  mask(., na_ecoregions_study_area_moll) # Mask to our study area ecoregions
   

# Check out current result
plot(npp_processed) # We got some warnings in the previous step and I think those might be related to the parts at the poles; I don't think the data was overall affected by this

# Convert the ecoregions to a raster so we can group npp values by ecoregion
ecoregion_raster <- rasterize(na_ecoregions_study_area_moll, npp_processed, field = "NA_L3CODE")

# Stack the NPP and ecoregions rasters
npp_stack <- c(npp_processed, ecoregion_raster)
names(npp_stack) <- c("npp", "ecoregion_id") # Make clear what each layer is

# Convert the stack to a dataframe
npp_df <- as.data.frame(npp_stack, xy = TRUE, na.rm = TRUE) # Can't remove NAs since we converted gapfill values to NAs

# Get 1st and 99th percentile of NPP by ecoregion
npp_percentiles <- npp_df %>%
  group_by(ecoregion_id) %>%
  summarize(
    p1 = quantile(npp, 0.01, na.rm = TRUE),
    p99 = quantile(npp, 0.99, na.rm = TRUE),
    .groups = "drop"
  )

# Rescale each ecoregion based on the 1st and 99th percentiles of NPP
# If value is less than p1 or higher than p99 = 0
# Otherwise, rescale based on p1 and p99
# 0 is 1, p99 = 0, linear between
npp_rescaled <- npp_df %>%
  left_join(npp_percentiles, by = "ecoregion_id") %>%
  mutate(
    npp_rescaled = case_when(
      npp == 0 ~ 1,
      npp >= p99 ~ 0,
      TRUE ~ 1 - (npp / p99)
    )
  ) %>%
  select(x, y, npp_rescaled)

# Convert rescaled NPP back to a raster
npp_rescaled_raster <- rast(npp_rescaled, type = "xyz")

# Set the CRS to match the original NPP raster before converting to df
# It may not have a CRS set when first converted back, so we manually assign what it should be
crs(npp_rescaled_raster) <- crs(npp_processed)

# Plot to check all looks as expected
plot(npp_rescaled_raster) # Looks good

# Align with our template
npp_rescaled_raster_aligned <- align_raster_to_template(study_area_rast, npp_rescaled_raster)

# Plot the rescaled raster
plot(npp_rescaled_raster_aligned, main = "NPP Rescaled by Ecoregion") # Looks good

# Write out rescaled raster
writeRaster(npp_rescaled_raster_aligned, file.path(wri_project_root, "final_layers", "2024", "natural_habitats", "indicators", "natural_habitats_resistance_npp.tif"), overwrite = TRUE)