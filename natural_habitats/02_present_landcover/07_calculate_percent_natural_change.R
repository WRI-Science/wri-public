wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal #### 
# The goal of this script is to calculate the change in percent natural from 
# 2005 and 2015 to the present year for each ecoregion. The output for this script can then be rescaled 
# for the status layer of natural habitats.

# This takes about 30 minutes to run on aurora

#### Packages ####
library(terra)
library(tidyverse)
library(data.table)
library(here)

#### File Paths ####
year_of_data <- 2023
output_index_year <- 2024

# these paths will need to be updated with our new file structure once they are generated in the correct spots
natural_habitats_path <- file.path(wri_project_root, "data", "natural_habitats")
present_percent_natural_path <- file.path(natural_habitats_path, "int/esri_present_landcover/", as.character(year_of_data), "percent_natural_calculation", paste0("ecoregion_natural_extent_pct_", year_of_data, ".csv"))

percent_natural_2005_path <- file.path(natural_habitats_path, "int/historical_landcover/percent_natural_ecoregion_2005.csv")
percent_natural_2015_path <- file.path(natural_habitats_path, "int/historical_landcover/percent_natural_ecoregion_2015.csv")
change_in_percent_natural_save_path <- file.path(natural_habitats_path, "int/landcover_change/", paste0("change_in_percent_natural", as.character(year_of_data), ".csv"))

# presently, we are using the 2005 layer as the indicator layer and the 2015 will be for sensitivity testing
# this means the 2005 one will be saved in two places
percent_change_2005_raster_path <- file.path(natural_habitats_path, "rescaled_indicators/natural_habitats_status_extent_change_2005.tif")
percent_change_2005_raster_final_layers_path <- file.path(wri_project_root, "final_layers", as.character(output_index_year), "natural_habitats", "indicators_no_mask", "natural_habitats_status_extent_change_2005.tif")
percent_change_2005_raster_final_layers_mask_path<- file.path(wri_project_root, "final_layers", as.character(output_index_year), "natural_habitats", "indicators_mask", "natural_habitats_status_extent_change_2005.tif")
percent_change_2015_raster_path <- file.path(natural_habitats_path, "sensitivity_testing/natural_habitats_status_extent_change_2015.tif")

# ecoregion shapefiles
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

ecoregion_intersection_path <- file.path(multi_domain_data_file_path, 
                                         "int/boundary_layers/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp")
# ag/urbann mask
ag_urban_mask_path <- file.path(natural_habitats_path, "int/esri_present_landcover", as.character(year_of_data), "full_masks", "full_ag_urban_mask_90m_5070.tif")

# Sample raster path
sample_raster_path <- file.path(multi_domain_data_file_path, 
                                "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# source alignment function
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Calculate difference in percent natural ####

current_natural <- fread(present_percent_natural_path) %>% 
  rename(
    percent_natural_present = "Percent Natural",
    rescaled_percent_natural_present = "Normalized Percent Natural",
    ecoregion_code = "Ecoregion Code"
  ) %>%
  unique()

# read in the 2005 percent natural data
percent_natural_2005 <- fread(percent_natural_2005_path) %>% 
  # select the columns we want
  select(ecoregion_code = eco_id,
         ecoregion_name = eco_name,
         percent_natural_2005 = percent_natural)

# read in the 2015 percent natural data
percent_natural_2015 <- fread(percent_natural_2015_path) %>% 
  # select the columns we want
  select(ecoregion_code = eco_id,
         ecoregion_name = eco_name,
         percent_natural_2015 = percent_natural)

# join the two dataframes together
# left join with the 2005 and 2015 one as the base because it only includes the 
# ecoregions in our study area
percent_natural <- left_join(percent_natural_2005, percent_natural_2015) %>%
  left_join(current_natural, by = "ecoregion_code") %>%
  # calculate the difference in percent natural
  mutate(
    rescaled_change_2005 = percent_natural_present / percent_natural_2005,
    rescaled_change_2015 = percent_natural_present / percent_natural_2015) %>% 
  # remove the geometry column for now
  select(-geometry) %>% 
  # in the rescaled values are greater than 1, set them to 1
  mutate(
    rescaled_change_2005 = ifelse(rescaled_change_2005 > 1, 1, rescaled_change_2005),
    rescaled_change_2015 = ifelse(rescaled_change_2015 > 1, 1, rescaled_change_2015)
  )

# write the output to a csv file
fwrite(percent_natural, change_in_percent_natural_save_path)

#### Read in Shapefile, merge with percent natural data ####
# read in the ecoregion shapefile
ecoregion_intersection <- vect(ecoregion_intersection_path)

# Merge the percent_natural data into the shapefile using terra::merge
ecoregion_merged <- merge(
  ecoregion_intersection,
  percent_natural %>% 
    # remove the ecoregion_name column for merge
    select(-ecoregion_name),
  by.x = "NA_L3CODE",
  by.y = "ecoregion_code",
  all.x = TRUE
)

#### Now Rasterize the merged shapefile ####
# Load the template raster (EPSG:5070, 90m resolution)
template_raster <- rast(sample_raster_path)

# Reproject the merged shapefile to match the raster CRS
ecoregion_5070 <- project(ecoregion_merged, crs(template_raster))

# Make 2005 and 2015 rasters
# Rasterize the percent_natural field using mean to resolve overlaps
natural_raster_2005 <- rasterize(
  ecoregion_5070,
  template_raster,
  field = "rescaled_change_2005",
  fun = "mean"
)

natural_raster_2015 <- rasterize(
  ecoregion_5070,
  template_raster,
  field = "rescaled_change_2015",
  fun = "mean"
)

# Pass through alignment function with the template raster
natural_raster_2005 <- align_raster_to_template(
  template_raster,
  natural_raster_2005,
  input_type = "continuous"
)

ag_urban_mask <- rast(ag_urban_mask_path)
# Align the ag/urban mask to the template raster
ag_urban_mask <- align_raster_to_template(
  template_raster = template_raster,
  input_raster = ag_urban_mask,
  input_type = "categorical"
)

natural_raster_2005_masked <- mask(natural_raster_2005, ag_urban_mask, inverse = TRUE)

natural_raster_2015 <- align_raster_to_template(
  template_raster,
  natural_raster_2015,
  input_type = "continuous"
)

# rename the layers
names(natural_raster_2005) <- "rescaled_change_2005"
names(natural_raster_2005_masked) <- "rescaled_change_2005"
names(natural_raster_2015) <- "rescaled_change_2015"

# Save the rasters
writeRaster(
  natural_raster_2005,
  filename = percent_change_2005_raster_path,
  overwrite = TRUE
)
writeRaster(
  natural_raster_2015,
  filename = percent_change_2015_raster_path,
  overwrite = TRUE
)
# Save the 2005 raster to the final layers path with no mask
writeRaster(
  natural_raster_2005,
  filename = percent_change_2005_raster_final_layers_path,
  overwrite = TRUE
)
# Save the 2005 raster to the final layers path with mask
writeRaster(
  natural_raster_2005_masked,
  filename = percent_change_2005_raster_final_layers_mask_path,
  overwrite = TRUE
)
