#### Goal ####
# The goal of this script is to combine the two parts of the status calculation 
# for natural habitats, make it into a raster, and save  the raster as the status layer

#### Packages ####
library(tidyverse)
library(terra)

#### Setup and File Paths ####
year_of_interest <- 2024
# source alignment function
source(here::here("templates_and_functions", "align_raster_to_template.R"))

natural_habitats_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_base_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

protected_areas_path <- file.path(natural_habitats_path, "int/cec_protected_areas/ecoregion_protection_rescaled.csv")

change_in_percent_natural_path <- file.path(natural_habitats_path, "int/landcover_change/", paste0("change_in_percent_natural", as.character(year_of_interest), ".csv"))

if (!file.exists(change_in_percent_natural_path)) {
  message("Current year mask not found; using previous year: ", year_of_interest - 1)
  change_in_percent_natural_path <- paste0(
    natural_habitats_path, "int/landcover_change/change_in_percent_natural", year_of_interest - 1 , ".csv")
}

ecoregion_intersection_path <- file.path(multi_domain_data_base_path, 
                                         "int/boundary_layers/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp")

# Sample raster path
sample_raster_path <- file.path(multi_domain_data_base_path, 
                                "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# Template raster path for alignment function
template_raster_path <- file.path(
  multi_domain_data_base_path, 
  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"
)
template_raster <- rast(template_raster_path)

# ag/urban mask
ag_urban_mask_path <- paste0(
  natural_habitats_path, "int/esri_present_landcover/", year_of_interest,
  "/full_masks/full_ag_urban_mask_90m_5070.tif"
)

message("Checking ag/urban mask path: ", ag_urban_mask_path)
# if the ag_urban mask file does not exist use the previous years data
if (!file.exists(ag_urban_mask_path)) {
  message("Current year mask not found; using previous year: ", year_of_interest - 1)
  ag_urban_mask_path <- paste0(
    natural_habitats_path, "int/esri_present_landcover/", year_of_interest - 1,
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
  "/home/shares/wwri-wildfire/final_layers/", year_of_interest,
  "/natural_habitats/natural_habitats_status.tif"
)

#### Main Processing ####
# read in the ecoregion protection rescaled data and the change in percent natural data
ecoregion_protection_rescaled <- read_csv(protected_areas_path)
change_in_percent_natural <- read_csv(change_in_percent_natural_path)

# join the two dataframes on the ecoregion code
ecoregion_status <- ecoregion_protection_rescaled %>%
  left_join(change_in_percent_natural, by = c("NA_L3CODE" = "ecoregion_code")) %>% 
  select(NA_L3CODE, NA_L3NAME, NA_L3KEY, rescaled_protected, rescaled_change_2005) %>% 
  mutate(status = (rescaled_protected + rescaled_change_2005)/2)

# make a quick histogram of the status values
# hist(ecoregion_status$status, 
#      main = "Ecoregion Status Values", 
#      xlab = "Status Value", 
#      breaks = 20, 
#      col = "lightblue")

#### Read in Shapefile, merge with percent natural data ####
# read in the ecoregion shapefile
ecoregion_intersection <- vect(ecoregion_intersection_path)

# Merge the percent_natural data into the shapefile using terra::merge
status_ecoregion_shape_merged <- merge(
  ecoregion_intersection,
  ecoregion_status,
  by.x = "NA_L3CODE",
  by.y = "NA_L3CODE",
  all.x = TRUE
)

#### Now Rasterize the merged shapefile ####
# Load the template raster (EPSG:5070, 90m resolution)
template_raster <- rast(sample_raster_path)

# Reproject the merged shapefile to match the raster CRS
status_ecoregion_5070 <- project(status_ecoregion_shape_merged, crs(template_raster))

# Rasterize the status field using mean to resolve overlaps
status_ecoregion_raster <- rasterize(
  status_ecoregion_5070,
  template_raster,
  field = "status",
  fun = "mean"
)

# rename the layers
names(status_ecoregion_raster) <- "status"

# Pass through alignment function with the template raster
status_ecoregion_raster <- align_raster_to_template(
  template_raster = template_raster,
  input_raster = status_ecoregion_raster,
  input_type = "continuous"
)

mask_raster <- mask(status_ecoregion_raster, ag_urban_mask, inverse = TRUE)

names(mask_raster) <- "status"

# save! 
writeRaster(
  mask_raster,
  filename = status_save_path,
  overwrite = TRUE
)
