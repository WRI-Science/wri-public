wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### GOAL ####
# The goal of this script is to make the recovery layer from the indicators for 
# the natural habitats domain. The individual indicator layers should be finalized 
# and this is putting it all together. It will read in the individual indicator layers
# and calculate the recovery score for natural habitats that will then be used 
# for resilience.

# This script takes about 25 minutes to run on aurora with its current settings.

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

recovery_traits <- "natural_habitats_recovery_tree_traits.tif"
recovery_diversity <- "natural_habitats_recovery_diversity.tif"
recovery_ppt <- "natural_habitats_recovery_ppt.tif"

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

recovery_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_recovery.tif"
)

#### Function ####

check_if_masked_raster_exists <- function(raster_name){
  message("-- Checking masked raster for: ", raster_name)
  if (!file.exists(paste0(masked_indicator_path, raster_name))) {
    message("   > Masked raster not found: creating ", raster_name)
    # if it does not exist make it
    raster_path <- paste0(not_masked_indicator_data_path, raster_name)
    no_mask_raster <- rast(raster_path)
    variable_name <- names(no_mask_raster)[1]
    
    # align the raster to the template, it should be already
    no_mask_raster <- align_raster_to_template(
      template_raster = template_raster,
      input_raster = no_mask_raster,
      input_type = "continuous"
    )
    
    mask_raster <- mask(no_mask_raster, ag_urban_mask, inverse = TRUE)
    names(mask_raster) <- variable_name
    
    writeRaster(mask_raster, 
                filename = paste0(masked_indicator_path, raster_name), 
                overwrite = TRUE)
    message("   > Masked raster created: ", raster_name)
  } else { 
    message("   > Masked raster exists: ", raster_name)
    mask_raster <- rast(paste0(masked_indicator_path, raster_name))
  }
  return(mask_raster)
}

#### Process Recovery Indicators ####
message("Processing RECOVERY indicators")
# recovery traits
recovery_traits_raster <- check_if_masked_raster_exists(recovery_traits)
# recovery diversity
recovery_diversity_raster <- check_if_masked_raster_exists(recovery_diversity)
# recovery ppt
recovery_ppt_raster <- check_if_masked_raster_exists(recovery_ppt)

message("Aligning recovery rasters to template")
# Conduct data checks to make sure the rasters are the same size, crs, and extent
recovery_traits_raster <- align_raster_to_template(template_raster = template_raster, 
                                                   input_raster = recovery_traits_raster,
                                                   input_type = "continuous")
recovery_diversity_raster <- align_raster_to_template(template_raster = template_raster, 
                                                      input_raster = recovery_diversity_raster,
                                                      input_type = "continuous")
recovery_ppt_raster <- align_raster_to_template(template_raster = template_raster,
                                                input_raster = recovery_ppt_raster,
                                                input_type = "continuous")

## Calculate recovery ##
message("Calculating mean recovery")
recovery_raster_all_layers <- c(recovery_traits_raster, 
                                recovery_diversity_raster, 
                                recovery_ppt_raster)
# remove the individual recovery rasters from memory
rm(recovery_traits_raster, recovery_diversity_raster, 
   recovery_ppt_raster)

# take the mean across layers
recovery_mean_raster <- mean(recovery_raster_all_layers, na.rm = TRUE)
# make sure it is masked
recovery_mean_raster <- mask(recovery_mean_raster, ag_urban_mask, inverse = TRUE)

# change the layer names to recovery
names(recovery_mean_raster) <- "recovery"

# Save the aligned recovery raster
message("Writing recovery raster to: ", recovery_save_path)
writeRaster(recovery_mean_raster, 
            filename = recovery_save_path, 
            overwrite = TRUE)

# remove the recovery rasters from memory
rm(recovery_raster_all_layers, recovery_mean_raster)
gc()
message("Recovery processing complete")