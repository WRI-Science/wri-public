#### Goal ###
# The goal of this script is to rescale the VPD data from TerraClimate to 0-1
# based on the biome specific thresholds.
# The script assumes that the raw terraclimate data has been downloaded for 
# the year of interest using the file paths below.
# Thresholds from: 
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-022-34966-3/MediaObjects/41467_2022_34966_MOESM1_ESM.pdf
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(terra)
library(here)

#### File paths and Setup ####
year <- 2024

natural_habitats_root <- "/home/shares/wwri-wildfire/data/natural_habitats/"
raw_vpd_path <- paste0(natural_habitats_root, "raw/terraclimate/vpd/TerraClimate_vpd_", year, ".nc")

multi_domain_root <- "/home/shares/wwri-wildfire/data/multi_domain_data/"
study_region_shape_path <- paste0(multi_domain_root, "/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")
wwf_ecoregion_path <- paste0(natural_habitats_root, "raw/wwf_ecoregions/shapefiles/wwf_terr_ecos.shp")

# template raster
template_raster_path <- file.path(multi_domain_root, "/int/boundary_layers",
                                  "admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")
# source alignment function
source(here("templates_and_functions", "align_raster_to_template.R"))

# ag/urban mask
ag_urban_mask_path <- paste0(natural_habitats_root, "int/esri_present_landcover/", year, "/full_masks/full_ag_urban_mask_90m_5070.tif")
# if the ag_urban mask file does not exist use the previous years data
if (!file.exists(ag_urban_mask_path)) {
  ag_urban_mask_path <- paste0(natural_habitats_root, "int/esri_present_landcover/", year - 1, "/full_masks/full_ag_urban_mask_90m_5070.tif")
}

# save paths
rescaled_save_path <- paste0(natural_habitats_root, "rescaled_indicators/", year, "/")

final_layer_root <- "/home/shares/wwri-wildfire/final_layers/"
final_layer_save_path_no_mask <- paste0(final_layer_root, year, "/natural_habitats/indicators_no_mask/")
final_layer_save_path_mask <- paste0(final_layer_root, year, "/natural_habitats/indicators_mask/")

# make all save paths if they do not exist
if (!dir.exists(rescaled_save_path)) {
  dir.create(rescaled_save_path, recursive = TRUE)
}
if (!dir.exists(final_layer_save_path_no_mask)) {
  dir.create(final_layer_save_path_no_mask, recursive = TRUE)
}
if (!dir.exists(final_layer_save_path_mask)) {
  dir.create(final_layer_save_path_mask, recursive = TRUE)
}


# crs
moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

#### Load and Prepare VPD and Study Region ####
print("Loading VPD raster...")
vpd_raster <- rast(raw_vpd_path)

print("Loading study region shapefile...")
study_region <- vect(study_region_shape_path)

# Check and align CRS
print("Checking CRS...")
if (crs(study_region) != crs(vpd_raster)) {
  print("CRS do not match, reprojecting study region to VPD CRS...")
  study_region <- project(study_region, vpd_raster)
} else {
  print("CRS match.")
}

# Buffer the study area
print("Buffering study region...")
buffered_region <- buffer(study_region, width = 5000)

# Crop and mask VPD raster
print("Cropping and masking VPD raster to reduce from global...")
vpd_raster_cropped <- crop(vpd_raster, buffered_region)
vpd_raster_masked <- mask(vpd_raster_cropped, buffered_region, touches = TRUE)

# Coverting VPD raster to moll_weide projection
print("Reprojecting VPD raster to Mollweide projection for working with ecoregions...")
vpd_raster_moll <- project(vpd_raster_masked, moll_crs)
# force assign the crs because the project function does not assign it
crs(vpd_raster_moll) <- moll_crs

#### Prepare WWF ecoregions and convert to raster ####

print("Loading WWF ecoregion shapefile...")
wwf_ecoregions <- vect(wwf_ecoregion_path)

# Check and align CRS
if (crs(wwf_ecoregions) != crs(vpd_raster_moll)) {
  print("CRS do not match, reprojecting WWF ecoregions to VPD CRS...")
  wwf_ecoregions <- project(wwf_ecoregions, vpd_raster_moll)
} else {
  print("CRS match.")
}

# Crop and mask WWF ecoregions
print("Cropping and masking WWF ecoregions...")
# project the buffered region to the same CRS as the vpd_raster_moll
buffered_region <- project(buffered_region, crs(vpd_raster_moll))
wwf_ecoregions_cropped <- crop(wwf_ecoregions, buffered_region)
wwf_ecoregions_masked <- mask(wwf_ecoregions_cropped, buffered_region)

# Rasterize ecoregions using BIOME field
print("Rasterizing WWF ecoregions by BIOME...")
biome_raster <- rasterize(wwf_ecoregions_masked, vpd_raster_moll, field = "BIOME", touches = TRUE)

#### Combine biome and VPD data ####

print("Converting rasters to data frames and merging...")
biome_values <- as.data.frame(biome_raster, xy = TRUE)
vpd_df <- as.data.frame(vpd_raster_moll, xy = TRUE)

biome_vpd_df <- merge(vpd_df, biome_values, by = c("x", "y"))

print("Calculating maximum VPD across all months...")
max_vpd <- max(biome_vpd_df[, paste0("vpd_", 1:12)], na.rm = TRUE)

# Assign biome-specific VPD thresholds
print("Assigning VPD thresholds by BIOME...")
biome_vpd_df <- biome_vpd_df %>%
  mutate(vpd_threshold = case_when(
    BIOME == 1 ~ NA_real_,                  # not in study region
    BIOME == 2 ~ NA_real_,                  # not in study region
    BIOME == 3 ~ 2.54,                      # Tropical and subtropical coniferous forests
    BIOME == 4 ~ 1.30,                      # Temperate broadleaf and mixed forests
    BIOME == 5 ~ 1.54,                      # Temperate coniferous forests
    BIOME == 6 ~ 1.11,                      # Taiga and Boreal forest
    BIOME == 7 ~ NA_real_,                  # not in study region
    BIOME == 8 ~ (1.34 + 1.5) / 2,          # need to confirm value ~ Temperate grasslands, savannas, and shrublands. For now we are using average of values we have for biome 4 and 5
    BIOME == 9 ~ NA_real_,                  # not in study region
    BIOME == 10 ~ NA_real_,                 # not in study region
    BIOME == 11 ~ 1.11,                     # need to confirm value ~ Tundra for now will use Boreal forest value
    BIOME == 12 ~ 2.31,                     # Mediterranean forests, woodlands, and scrub
    BIOME == 13 ~ 2.31,                     # need to confirm value ~ Deserts and xeric shrublands, for now will use Mediterranean forests, woodlands, and scrub value
    BIOME == 14 ~ NA_real_,                 # not in study region
    BIOME == 99 ~ max_vpd,                  # Rock and Ice (max value because it does not burn)
    TRUE ~ NA_real_                         # Default case if no match
  ))

#### Count months above VPD threshold and rescale 0-1 ####

print("Counting months above VPD threshold...")
biome_vpd_df_count <- biome_vpd_df %>%
  rowwise() %>%
  mutate(months_above_threshold = sum(c_across(starts_with("vpd_")) > vpd_threshold, na.rm = FALSE)) %>%
  ungroup()

print("Calculating 99th percentile for rescaling...")
rescale_quantile <- quantile(biome_vpd_df_count$months_above_threshold, 0.99, na.rm = TRUE)

print("Rescaling resistance values...")
biome_vpd_df_count <- biome_vpd_df_count %>%
  select(x, y, BIOME, months_above_threshold) %>%
  mutate(
    rescaled_months_above_threshold = ifelse(
      months_above_threshold > rescale_quantile,
      rescale_quantile,
      months_above_threshold
    ),
    rescaled_months_above_threshold = 1 - (rescaled_months_above_threshold / rescale_quantile)
  )

#### Convert resistance values back to raster ####

print("Converting rescaled data to raster...")
biome_vpd_df_count_for_map <- biome_vpd_df_count %>%
  select(x, y, resistance = rescaled_months_above_threshold) %>%
  mutate(resistance = as.numeric(resistance))

biome_vpd_resistance_raster <- rast(biome_vpd_df_count_for_map, type = "xyz", crs = crs(vpd_raster_moll))

# Reproject to EPSG:5070
print("Reprojecting raster to EPSG:5070...")
biome_vpd_resistance_raster <- project(biome_vpd_resistance_raster, "EPSG:5070")

#### Resample to 90m, align, and save ####

print("Reading 90-meter resolution template...")
template_raster <- rast(template_raster_path)

print("Resampling resistance raster to 90-meter resolution...")
resampled_raster <- resample(biome_vpd_resistance_raster, template_raster, method = "bilinear")
# remove to clear memory
rm(biome_vpd_resistance_raster)

print("Aligning resistance raster to template...")
resampled_raster <- align_raster_to_template(input_raster = resampled_raster, 
                                             template_raster =  template_raster,
                                             input_type = "continuous")

print("Saving unmasked final raster...")
writeRaster(resampled_raster, paste0(final_layer_save_path_no_mask, "natural_habitats_resistance_vpd.tif"), overwrite = TRUE)

print("Loading ag/urban mask...")
ag_urban_mask <- rast(ag_urban_mask_path)
ag_urban_mask <- align_raster_to_template(input_raster = ag_urban_mask, 
                                          template_raster =  template_raster,
                                          input_type = "categorical")

# mask to ag/urban inverse
print("Masking resistance raster with ag/urban mask...")
masked_raster <- mask(resampled_raster, ag_urban_mask, inverse = TRUE)

print("Saving masked final raster...")
writeRaster(masked_raster, paste0(final_layer_save_path_mask, "natural_habitats_resistance_vpd.tif"), overwrite = TRUE)

writeRaster(masked_raster, paste0(rescaled_save_path, "natural_habitats_resistance_vpd.tif"), overwrite = TRUE)

print("Script completed successfully.")
