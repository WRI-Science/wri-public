wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse) # For data manipulation
library(sf) # For spatial data manipulation
library(terra) # For raster data manipulation
library(here) # To assemble file paths within project


#### Script Overview ####
# This script reads in drought plan scores for US states and Canadian provinces, filters to the states of interest, rescores the data to a 0-1 scale, rasterizes the scores to a 90 m resolution, and writes out the raster layer.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "water") # Base directory for water
final_layers_file_path <- file.path(wri_project_root, "final_layers")
multi_domain_data_path <- file.path(wri_project_root, "data", "multi_domain_data") # Base directory for multi-domain data
path_year <- "2024" # Update this if needed for the year of the data
raw_data_file_path <- file.path(data_file_path, "raw", path_year) # Directory for raw data
final_layers_output_path <- file.path(final_layers_file_path, path_year, "water") # output path for final layers


#### Boundary Layers ####
# Read in spatial outlines to rasterize with scores
state_provinces <- st_read(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp"))

# Read in study area raster to rasterize to
study_area_rast <- rast(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))


#### Data Layers ####
# Read in state drought plan data
us_drought_plans <- read_csv(file.path(raw_data_file_path, "us_drought_plan_scores.csv")) %>%
  janitor::clean_names()


#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))


#### Data Processing ####
# Canadian provinces: manually construct a df -- scored by Izzy Sofio using similar methods as US data source
# This is already just for BC and Yukon so no filtering needed
canadian_scores <- tibble::tibble(
  state = c("British Columbia", "Yukon"),
  climate_score = c(6, 4), # BC climate score = 6, Yukon = 4
  water_score = c(4, 4), # BC Flood plan (water) = 4, Yukon water plan = 4
  drought_score = c(9, 0), # BC Drought plan = 9, Yukon = does not have
  multi_hazard_score = c(4, 2), # BC Climate Risk Assessment (like Multi-haz) = 4, Yukon Multi-haz = 2
) %>%
  mutate(total = climate_score + water_score + drought_score + multi_hazard_score) # Calculate total from all scores

# Set up US filter to only states/provinces of interest
interested_states <- c("New Mexico", "Arizona", "California", "Nevada", "Utah", "Colorado", "Montana", "Idaho", "Wyoming", "Washington", "Oregon", "Alaska", "British Columbia", "Yukon") # States in study area

# Combine US and Canada data, keeping only states of interest, and rescale the total score to a 0-1 scale based on its maximum possible score of 36 (4 categories, each with a max of 9 points)
drought_plans_filtered <- us_drought_plans %>%
  bind_rows(., canadian_scores) %>% # Add in Canadian province scores
  filter(state %in% interested_states) %>% # Filter to states of interest
  mutate(score_rescaled = scales::rescale(total, to = c(0, 1), from = c(0, 36))) %>% # Rescale drought plan scores 0 to 1
  select(state, score_rescaled) # Select only state and rescaled score columns


# Left join the score data to the state shapes
us_drought_plans_w_spatial <- left_join(drought_plans_filtered, state_provinces, by = c("state" = "name")) %>%
  st_set_geometry("geometry")


# Rasterize layer to 90 m
drought_plans_rast <- terra::rasterize(us_drought_plans_w_spatial,
                                         study_area_rast,
                                         field = "score_rescaled",
                                         fun = "mean")

# Ensure alignment with template
drought_plans_rast <- align_raster_to_template(study_area_rast, drought_plans_rast)

# Plot
plot(drought_plans_rast,
     main = "Drought Plan Scores")

# Write out aligned drought plan score raster
writeRaster(drought_plans_rast, file.path(final_layers_output_path, "indicators", "water_resistance_drought_plans.tif"), overwrite = TRUE)