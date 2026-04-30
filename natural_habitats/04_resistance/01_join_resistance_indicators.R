#### GOAL ####
# The goal of this script is to make the resistance layer from the indicators for 
# the natural habitats domain. The individual indicator layers should be finalized 
# and this is putting it all together. It will read in the individual indicator layers
# and calculate the resistance score for natural habitats that will then be used 
# for resilience.
# This script takes about 35 minutes to run on aurora with its current settings.

#### Packages ####
library(tidyverse)
library(terra)

#### Setup and File Paths ####
year_of_interest <- 2024
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"

# Setup terra options for parallel processing
terraOptions(
  threads = 12,        # Use 12 threads
  memfrac = 0.80      # Limit RAM usage to 90%
)

not_masked_indicator_data_path <- paste0("/home/shares/wwri-wildfire/final_layers/", year_of_interest, "/natural_habitats/indicators_no_mask/")
masked_indicator_path <- paste0("/home/shares/wwri-wildfire/final_layers/", year_of_interest, "/natural_habitats/indicators_mask/")

resistance_traits <- "natural_habitats_resistance_tree_traits.tif"
resistance_density <- "natural_habitats_resistance_density.tif"
resistance_ndvi <- "natural_habitats_resistance_NDVI.tif"
resistance_npp <- "natural_habitats_resistance_npp.tif"
resistance_vpd <- "natural_habitats_resistance_vpd.tif"

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
  "/home/shares/wwri-wildfire/final_layers/", year_of_interest,
  "/natural_habitats/natural_habitats_resistance.tif"
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

#### Process Resistance Indicators ####
message("Processing RESISTANCE indicators")
# resistance traits
resistance_traits_raster <- check_if_masked_raster_exists(resistance_traits)
# resistance density
resistance_density_raster <- check_if_masked_raster_exists(resistance_density)
# resistance NDVI
resistance_ndvi_raster <- check_if_masked_raster_exists(resistance_ndvi)
# resistance NPP
resistance_npp_raster <- check_if_masked_raster_exists(resistance_npp)
# resistance VPD
resistance_vpd_raster <- check_if_masked_raster_exists(resistance_vpd)

message("Aligning resistance rasters to template")
# Conduct data checks to make sure the rasters are the same size, crs, and extent
resistance_traits_raster <- align_raster_to_template(template_raster = template_raster, 
                                                     input_raster = resistance_traits_raster,
                                                     input_type = "continuous")
resistance_density_raster <- align_raster_to_template(template_raster = template_raster, 
                                                      input_raster = resistance_density_raster,
                                                      input_type = "continuous")
resistance_ndvi_raster <- align_raster_to_template(template_raster = template_raster,
                                                   input_raster = resistance_ndvi_raster,
                                                   input_type = "continuous")
resistance_npp_raster <- align_raster_to_template(template_raster = template_raster,
                                                  input_raster = resistance_npp_raster,
                                                  input_type = "continuous")
resistance_vpd_raster <- align_raster_to_template(template_raster = template_raster,
                                                  input_raster = resistance_vpd_raster,
                                                  input_type = "continuous")

## Calculate resistance ##
message("Calculating mean resistance")
resistance_raster_all_layers <- c(resistance_traits_raster, 
                                  resistance_density_raster, 
                                  resistance_ndvi_raster, 
                                  resistance_npp_raster,
                                  resistance_vpd_raster)

# remove the individual resistance rasters from memory
rm(resistance_traits_raster, resistance_density_raster, 
   resistance_ndvi_raster, resistance_npp_raster, resistance_vpd_raster)

# take the mean across layers
resistance_mean_raster <- mean(resistance_raster_all_layers, na.rm = TRUE)

# make sure it is masked
resistance_mean_raster <- mask(resistance_mean_raster, ag_urban_mask, inverse = TRUE)

# change the layer names to resistance
names(resistance_mean_raster) <- "resistance"

# Save the aligned resistance raster
message("Writing resistance raster to: ", resistance_save_path)
writeRaster(resistance_mean_raster, 
            filename = resistance_save_path, 
            overwrite = TRUE)

# remove the resistance rasters from memory
rm(resistance_raster_all_layers, resistance_mean_raster)
gc()
message("Resistance processing complete")