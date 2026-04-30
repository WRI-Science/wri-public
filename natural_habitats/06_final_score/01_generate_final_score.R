wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is the generate the final domain score for natural habitats!
# it takes about 11 minutes to run on aurora

#### Packages ####
library(tidyverse)
library(terra)

#### Setup and File Paths ####
year_of_interest <- 2024
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")

# Setup terra options for parallel processing
terraOptions(
  threads = 12,        # Use 12 threads
  memfrac = 0.80      # Limit RAM usage to 90%
)

not_masked_indicator_data_path <- file.path(wri_project_root, "final_layers", year_of_interest, "natural_habitats", "indicators_no_mask")
masked_indicator_path <- file.path(wri_project_root, "final_layers", year_of_interest, "natural_habitats", "indicators_mask")

# Template raster path for alignment function
template_raster_path <- file.path(
  multi_domain_data_file_path, 
  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"
)
template_raster <- rast(template_raster_path)
message("Template raster loaded from: ", template_raster_path)

source(here::here("templates_and_functions", "align_raster_to_template.R"))
message("Alignment function sourced")

# ag/urban mask
ag_urban_mask_path <- paste0(
  natural_habitats_base_path, "int/esri_present_landcover/", year_of_interest,
  "/full_masks/full_ag_urban_mask_90m_5070.tif"
)

message("Checking ag/urban mask path: ", ag_urban_mask_path)
# if the ag_urban mask file does not exist use the previous years data
if (!file.exists(ag_urban_mask_path)) {
  message("Current year mask not found; using previous year: ", year_of_interest - 1)
  ag_urban_mask_path <- paste0(
    natural_habitats_base_path, "int/esri_present_landcover/", year_of_interest - 1,
    "/full_masks/full_ag_urban_mask_90m_5070.tif"
  )
}
ag_urban_mask <- rast(ag_urban_mask_path)
ag_urban_mask <- align_raster_to_template(
  template_raster = template_raster,
  input_raster = ag_urban_mask,
  input_type = "categorical"
)
message("Ag/urban mask aligned to template")

# save paths
status_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_status.tif"
)
resilience_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_resilience.tif"
)
final_scores_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_domain_score_masked.tif"
)
message("Output paths defined")

#### Calculate Final Scores ####
message("Processing FINAL SCORE calculation")
# read in the status and resilience rasters
status_raster <- rast(status_save_path)
resilience_raster <- rast(resilience_save_path)

# calculate the final scores by taking the mean of status and resilience
# na.rm = FALSE in case there are any places where we have a resilience of 0 but a status of NA
final_scores_raster <- mean(c(status_raster, resilience_raster), na.rm = FALSE)
# this is the old method to calculate final score
# final_scores_raster <- status_raster * resilience_raster

# make sure it is masked
final_scores_raster <- mask(final_scores_raster, ag_urban_mask, inverse = TRUE)

# multiply by 100 to get the final scores
final_scores_raster <- final_scores_raster * 100

# change the layer names to wwri_score
names(final_scores_raster) <- "wwri_score"

# save the final scores raster
message("Writing final scores raster to: ", final_scores_save_path)
writeRaster(final_scores_raster, 
            filename = final_scores_save_path, 
            overwrite = TRUE)

# remove the rasters from memory
rm(status_raster, resilience_raster, final_scores_raster)
gc()
message("All processing complete")