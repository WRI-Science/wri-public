wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# the goal of this script is to calculate resilience with the recovery and resistance layers.
# The output from this will be combined with status to calculate the final score for the domain.

# this script takes about 25 minutes to run on aurora with its current settings.

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

resistance_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_resistance.tif"
)
recovery_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_recovery.tif"
)
resilience_save_path <- paste0(
  file.path(wri_project_root, "final_layers"), year_of_interest,
  "/natural_habitats/natural_habitats_resilience.tif"
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

calc_resilience <- function(resistance, recovery) {
  message("-- Computing resilience composite via vector math + cover()")
  
  # 1) pure composite where both exist
  composite <- 1 - (1 - resistance) * (1 - recovery)
  names(composite) <- "resilience_temp"
  
  # 2) if composite is NA but resistance exists, fall back to resistance
  step2 <- cover(composite, resistance)
  
  # 3) if still NA but recovery exists, fall back to recovery
  resilience <- cover(step2, recovery)
  names(resilience) <- "resilience"
  
  return(resilience)
}

#### Resilience Processing ####
message("Processing RESILIENCE indicator")
# Calculate resilience as the mean of resistance and recovery rasters
# read in the resistance and recovery rasters we made
resistance_raster <- rast(resistance_save_path)
recovery_raster <- rast(recovery_save_path)

# make sure they are aligned
resistance_raster <- align_raster_to_template(template_raster = template_raster,
                                              input_raster = resistance_raster,
                                              input_type = "continuous")

recovery_raster <- align_raster_to_template(template_raster = template_raster,
                                            input_raster = recovery_raster,
                                            input_type = "continuous")

# now the two rasters have identical rows/cols/ext/res/crs
terra::compareGeom(resistance_raster, recovery_raster)  # should silently return TRUE

resilience_raster <- calc_resilience(resistance = resistance_raster, 
                                     recovery = recovery_raster)

# make sure it is masked
resilience_raster <- mask(resilience_raster, ag_urban_mask, inverse = TRUE)

# change the layer names to resilience
names(resilience_raster) <- "resilience"

# save the resilience raster
message("Writing resilience raster to: ", resilience_save_path)
writeRaster(resilience_raster, 
            filename = resilience_save_path, 
            overwrite = TRUE)
# remove the resilience rasters from memory
rm(resistance_raster, recovery_raster, resilience_raster)
gc()
message("Resilience processing complete")
