wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf)
library(tidyverse)
library(tidycensus)
library(terra)
sf::sf_use_s2(TRUE) # did not need it to be off to get valid shapes, but could turn it off
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))

census_api_key <- Sys.getenv('CENSUS_API_KEY')
study_area_raster <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "us_wwri_study_area_raster-mask-lvl-0-90m-with-na.tif")) # US study area raster to rasterize to

conus_states_of_interest_cwpps <- st_read(file.path(wri_project_root, "data", "communities", "raw", "us_cwpps", "conus", "GIS Data", "All CWPPs", "CWPP_Boundaries.shp")) %>%
  st_zm() %>%
  st_transform(., st_crs(study_area_raster)) # got warnings when transforming this to 4269 originally - not sure if this would still happen given changes to the code
conus_states_of_interest_atts <- read_csv(file.path(wri_project_root, "data", "communities", "raw", "us_cwpps", "conus", "Spreadsheets", "Data", "cwpp_attributes.csv"))
conus_states_of_interest_metadata <- read_csv(file.path(wri_project_root, "data", "communities", "raw", "us_cwpps", "conus", "Spreadsheets", "Documentation", "metadata.csv"))
alt_data_source_conus_states_of_interest <- readRDS(file.path(wri_project_root, "data", "communities", "raw", "us_cwpps", "conus", "alt-source-from-paper", "19193663", "dat.rds")) # %>% # found 837 cwpps based on paper; plancnt is how many plans a uid (actor) was involved with
  # select(cwpp) %>%
  # distinct()

# read in the drawn alaska cwpps
alaska_cwpps <- st_read(file.path(wri_project_root, "data", "communities", "raw", "us_cwpps", "alaska", "cwpps_with_shapes_and_images_clean.kml")) %>%
  select(name = Name, geometry) %>%
  filter(grepl("Shape", name)) %>%
  st_zm() %>%
  st_transform(., st_crs(study_area_raster))

# get the already created CDP or census shapefiles for remaining alaska cwpps
incorporation_data_folder_path <- file.path(wri_project_root, "data", "multi_domain_data", "raw", "us_census_designated_places", "2024")
incorporation_data_shapefile_paths <- list.files(incorporation_data_folder_path, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
alaska_cdps_of_interest <- lapply(incorporation_data_shapefile_paths, st_read) %>%
  bind_rows() %>%
  select(name = NAME, geometry) %>%
  filter(grepl("Mentasta|Ruby", name)) %>%
  st_transform(., st_crs(study_area_raster))

census_api_key <- Sys.getenv('CENSUS_API_KEY')

# get counties for alaska
interested_states <- c("AK")
variables_of_interest <- c("S0101_C01_001")


#### GETTING TRACT DATA ####
# get 2018-2022 5 year ACS data (most recent) for tracts (can easily gapfill with county)
alaska_county_of_interest <- map_df(interested_states, ~get_acs(geography = "county",
                                                              variables = variables_of_interest,
                                                              state = .x,
                                                              geometry = TRUE,
                                                              year = 2022,
                                                              survey = "acs5",
                                                              key = census_api_key)) %>%
  rename(geo_id = GEOID, name = NAME) %>%
  filter(grepl("Fairbanks North Star Borough", name)) %>%
  select(name, geometry) %>%
  st_transform(., st_crs(study_area_raster))

# combine all alaska shapes
alaska_cwpps_full <- rbind(alaska_cwpps, alaska_cdps_of_interest, alaska_county_of_interest)
unique(st_is_valid(alaska_cwpps_full))

st_write(alaska_cwpps_full, file.path(wri_project_root, "data", "communities", "int", "2024", "alaska_cwpp_shapes_clean_full.shp"), append = FALSE)
#test <- st_read(file.path(wri_project_root, "data", "sense-of-place-domain-data", "us-cwpps", "alaska", "alaska_cwpp_shapes_clean_full.shp"))

  

# combine alaska with the other cwpps
conus_states_of_interest_cwpps_simple <- conus_states_of_interest_cwpps %>%
  select(name = Name, geometry) 
st_is_valid(conus_states_of_interest_cwpps_simple)
conus_states_of_interest_cwpps_simple_valid <- st_make_valid(conus_states_of_interest_cwpps_simple)
unique(st_is_valid(conus_states_of_interest_cwpps_simple_valid))

conus_states_of_interest_cwpps_simple_joined <- rbind(alaska_cwpps_full, conus_states_of_interest_cwpps_simple_valid)
unique(st_is_valid(conus_states_of_interest_cwpps_simple_joined))

# rasterize using sum to get the number of cwpps touching each cell
us_cwpps_rast <- terra::rasterize(conus_states_of_interest_cwpps_simple_joined,
                                 study_area_raster,
                                 field = 1, # doing field = 1 makes it not continuous; need continuous for plotting properly with a base plot but unsure how to get it
                                 background = 0)#,
                                 #fun = "mean")
# Warning message:
# [rasterize] you cannot use 'sum' and 'touches' at the same time 
plot(us_cwpps_rast) # double check all polygons got counted, especially ones in alaska - im not sure if they did

# ensure anything outside the study area are counted as NA
# study_area_vect <- st_read(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_admin_0.shp")) %>%
#   st_transform(5070)
# us_masked_raster <- mask(us_cwpps_rast, study_area_raster)
# 
# 
# # rescaled masked raster
# us_masked_raster_min <- minmax(us_masked_raster)["min",]
# us_masked_raster_max <- minmax(us_masked_raster)["max",]
# 
# us_masked_raster_rescaled <- (us_masked_raster - us_masked_raster_min) / (us_masked_raster_max - us_masked_raster_min)
# 
# plot(us_masked_raster_rescaled)

# align to template
us_cwpps_rast_aligned <- align_raster_to_template(study_area_raster, us_cwpps_rast, input_type = "categorical")

# write out the full raster
writeRaster(us_cwpps_rast_aligned, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_cwpps.tif"), overwrite = TRUE) # this is currently in 5070

# us_masked_raster_rescaled_4269 <- us_masked_raster_rescaled %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(us_masked_raster_rescaled_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "cwpps_4269.tif"), overwrite = TRUE)


# Canada CWPPs are on hold for now