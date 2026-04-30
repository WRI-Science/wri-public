wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal #### 
# The goal of this script is to take the annual data and compare it to the 
# 30-year data to rescale it for the resistance layer. This produces the final 
# output for the indicator and assumed the previous ppt scripts have run.
# Currently this takes about 45 min to run on aurora.

#### Packages ####
print("Loading libraries...")
library(terra)
library(tidyverse)

### File Paths and Setup ####
year_of_interest <- 2024

natural_habitats_root <- file.path(wri_project_root, "data", "natural_habitats")
annual_ppt_path <- paste0(natural_habitats_root, "int/terraclimate/ppt_annual_sums/")
ppt_annual_raster_path <- paste0(annual_ppt_path, "ppt_annual_", year_of_interest, ".tif")

multi_domain_root <- file.path(wri_project_root, "data", "multi_domain_data")
study_region_shape_path <- paste0(multi_domain_root, "/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")
summary_stats_path <- paste0(natural_habitats_root, "int/terraclimate/ppt_1991_2020/ppt_1991_2020_summary_stats.csv")

# template raster
template_raster_path <- file.path(multi_domain_root, "/int/boundary_layers",
                                  "admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")
# source alignment function
source(here::here("templates_and_functions", "align_raster_to_template.R"))

# ag/urban mask
ag_urban_mask_path <- paste0(natural_habitats_root, "int/esri_present_landcover/", year_of_interest, "/full_masks/full_ag_urban_mask_90m_5070.tif")

# if the ag_urban mask file does not exist use the previous years data
if (!file.exists(ag_urban_mask_path)) {
  ag_urban_mask_path <- paste0(natural_habitats_root, "int/esri_present_landcover/", year_of_interest - 1, "/full_masks/full_ag_urban_mask_90m_5070.tif")
}

# save paths
rescaled_save_path <- paste0(natural_habitats_root, "rescaled_indicators/", year_of_interest, "/")

final_layer_root <- file.path(wri_project_root, "final_layers")
final_layer_save_path_no_mask <- paste0(final_layer_root, year_of_interest, "/natural_habitats/indicators_no_mask/")
final_layer_save_path_mask <- paste0(final_layer_root, year_of_interest, "/natural_habitats/indicators_mask/")

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

#### Main Processing ####
# 1. Read inputs
print("Reading precipitation raster and summary CSV...")
ppt_raster         <- rast(ppt_annual_raster_path)
summary_stats      <- read.csv(summary_stats_path)

print("Reading study-area shapefile and reprojecting...")
print("Reprojecting study-area shapefile to match raster CRS just to crop closer to study region for further processing")
study_vec          <- vect(study_region_shape_path)
study_vec_pr       <- project(study_vec, crs(ppt_raster))

# 2. Crop and mask to study area
print("Cropping raster to study-area extent...")
ppt_crop           <- crop(ppt_raster, study_vec_pr)

print("Masking raster by study-area polygon (touches = TRUE)...")
ppt_masked         <- mask(ppt_crop, study_vec_pr, touches = TRUE)

# 3. Convert to data.frame
print("Converting masked raster to data.frame (with cell IDs and xy)...")
ppt_df             <- as.data.frame(
  ppt_masked,
  cells = TRUE,
  xy    = TRUE,
  na.rm = FALSE
)

ppt_df <- ppt_df %>%
  rename(ppt_value = !!paste0("ppt_annual_", year_of_interest))

# 4. Filter out NA cells
print("Filtering out NA values...")
ppt_df <- ppt_df %>%
  filter(!is.na(ppt_value))

# 5. Join with summary statistics
print("Joining with summary statistics by cell ID...")
ppt_stats_df       <- ppt_df %>%
  left_join(summary_stats, by = "cell")

# 6. Compute rescale parameters
print("Computing slopes and intercepts for rescaling...")
ppt_stats_df       <- ppt_stats_df %>%
  mutate(
    slope_up     = 1 / (mean_ppt - p1_ppt),
    intercept_up = -p1_ppt * (1 / (mean_ppt - p1_ppt)),
    slope_down   = -1 / (p99_ppt - mean_ppt),
    intercept_down = p99_ppt * (1 / (p99_ppt - mean_ppt))
  )

# 7. Rescale precipitation
print("Applying rescale function to precipitation values...")
ppt_rescaled_df <- ppt_stats_df %>%
  mutate(rescaled_ppt = case_when(
    ppt_value <= p1_ppt                                     ~ 0,
    ppt_value >= p99_ppt                                    ~ 0,
    ppt_value == mean_ppt                                   ~ 1,
    ppt_value < mean_ppt & ppt_value > p1_ppt               ~ (ppt_value * slope_up) + intercept_up,
    ppt_value > mean_ppt & ppt_value < p99_ppt              ~ (ppt_value * slope_down) + intercept_down,
    TRUE                                                    ~ NA_real_
  ))

# Optional: quick histogram to inspect distribution
# print("Plotting rescaled distribution (commented out)...")
# ppt_rescaled_df %>%
#   ggplot(aes(x = rescaled_ppt)) +
#   geom_histogram(bins = 30) +
#   labs(title = "Rescaled Precipitation Distribution",
#        x = "Rescaled Precipitation",
#        y = "Count") +
#   theme_minimal()

# 8. Rasterize back to native grid
print("Rasterizing rescaled values back to native grid...")
precip_pts        <- vect(
  ppt_rescaled_df %>% select(x = x.x, y = y.x, rescaled_ppt),
  geom = c("x", "y"),
  crs  = crs(ppt_raster)
)
precip_native_rast <- rasterize(
  x     = precip_pts,
  y     = ppt_raster,
  field = "rescaled_ppt"
)

if (is.na(crs(precip_native_rast))) {
  crs(precip_native_rast) <- crs(ppt_raster)
}

# 9. Reproject to 90m Albers grid
print("Loading 90m template raster for target grid...")
template_rast <- rast(template_raster_path)

print("Reprojecting precip raster into template raster (Albers 90m grid, bilinear)...")
albers90 <- project(
  precip_native_rast,
  template_rast,
  method = "bilinear"
)

albers90 <- align_raster_to_template(input_raster = precip_native_rast,
                                     template_raster = template_rast,
                                     input_type = "continuous")

# assign variable name to raster
names(albers90) <- "recovery_ppt"


print(paste("Writing 90m Albers-projected raster to", final_layer_root))
writeRaster(
  albers90,
  paste0(rescaled_save_path, "natural_habitats_recovery_ppt.tif"),
  overwrite = TRUE,
  filetype  = "GTiff"
)

writeRaster(
  albers90,
  paste0(final_layer_save_path_no_mask, "natural_habitats_recovery_ppt.tif"),
  overwrite = TRUE,
  filetype  = "GTiff"
)

# 10. Mask with ag/urban mask
# mask to ag/urban inverse
print("Loading ag/urban mask...")
ag_urban_mask <- rast(ag_urban_mask_path)
ag_urban_mask <- align_raster_to_template(input_raster = ag_urban_mask, 
                                          template_raster =  template_rast,
                                          input_type = "categorical")

print("Masking resistance raster with ag/urban mask...")
masked_raster <- mask(albers90, ag_urban_mask, inverse = TRUE)

print("Saving masked final raster...")
writeRaster(
  masked_raster,
  paste0(final_layer_save_path_mask, "natural_habitats_recovery_ppt.tif"),
  overwrite = TRUE,
  filetype  = "GTiff"
)

# Optional: verify with a quick plot (commented out)
# print("Plotting final 90m raster (commented out)...")
# plot(precip_5070)

# End of script
print("All steps completed successfully.")
