wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse)
library(sf)
library(cancensus)
library(terra)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))
study_area_rast <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")) # study area raster to rasterize to

# combine US and Canada census data
can_census_variables_communities_full <- read_csv(file.path(wri_project_root, "data", "communities", "int", "2024", "canada_census", "can_census_variables_communities_full.csv"))
us_acs_variables_communities_full <- read_csv(file.path(wri_project_root, "data", "communities", "int", "2024", "acs", "us_acs_2019_2023_variables_communities_full.gpkg"))
census_variables_communities_full <- rbind(can_census_variables_communities_full, us_acs_variables_communities_full)
write_csv(census_variables_communities_full, file.path(wri_project_root, "data", "communities", "int", "2024", "census_variables_communities_full.csv"))
# ensure no duplicate geo ids between countries
# > length(unique(census_variables_communities_full$geo_id))
# [1] 19131
# all good as is same length as dataframe

# reverse direction of all indicators except 200k household
census_variables_communities_full_correct_direction <- census_variables_communities_full %>%
  mutate(poverty_inverted = scales::rescale(poverty, to = c(1, 0)),
         owner = scales::rescale(owner, to = c(0, 1)),
         age_65_plus_inverted = scales::rescale(age_65_plus, to = c(1, 0)),
         disability_inverted = scales::rescale(disability, to = c(1, 0)),
         no_vehicle_inverted = scales::rescale(no_vehicle, to = c(1, 0)),
         greater_than_200k = scales::rescale(greater_than_200k, to = c(0, 1)) # ensure it's between 0 (min) and 1 (max) but don't invert
  ) %>%
  select(-poverty, -age_65_plus, -disability, -no_vehicle)

write_csv(census_variables_communities_full_correct_direction, file.path(wri_project_root, "data", "communities", "int", "2024", "census_variables_communities_full_correct_direction.csv"))

# add back in the spatial data
# clean the tract data for only spatial info
regions_of_interest <- c("59", "60")
can_subdivs_spatial_only <- get_census(dataset = 'CA21', 
                                       regions = list(PR = regions_of_interest),
                                       vectors = c("v_CA21_1"), 
                                       level = 'CSD',
                                       geo_format = "sf") %>%
  select(geo_id = GeoUID) %>%
  distinct() %>% # may not be needed, but just in case
  st_transform(5070) 

us_acs_variables_communities <- st_read(file.path(wri_project_root, "data", "communities", "raw", "2024", "acs", "us_acs_2019_2023_variables_communities.gpkg"))
us_acs_variables_communities_cleaned_spatial_only <- us_acs_variables_communities %>%
  select(-NAME, -moe) %>%
  filter(variable == "S0101_C01_001") %>%
  select(geo_id, geometry = geom) %>%
  distinct() %>% # may not be needed, but just in case
  st_transform(5070) 

census_spatial_info <- rbind(can_subdivs_spatial_only, us_acs_variables_communities_cleaned_spatial_only)

# our two sources for census spatial info differ so need to add in that spatial info
us_census_spatial_alt <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "us_census_tract", "us_census_tracts.shp")) %>% select(geo_id = GEOID)
can_census_spatial_alt <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "canada_census_subdivisions", "canada_census_subdivisions.shp")) %>% select(geo_id = CSDUID)
  
spatial_alt_full <- rbind(us_census_spatial_alt, can_census_spatial_alt)

us_geo_id_difference <- setdiff(us_acs_variables_communities_cleaned_spatial_only$geo_id, spatial_alt_full$geo_id)

can_geo_id_difference <- setdiff(can_subdivs_spatial_only$geo_id, spatial_alt_full$geo_id)

us_extra_geometries_needed <- us_acs_variables_communities_cleaned_spatial_only %>% filter(geo_id %in% us_geo_id_difference)

can_extra_geometries_needed <- can_subdivs_spatial_only %>% filter(geo_id %in% can_geo_id_difference)

all_extra_geometries_needed <- rbind(us_extra_geometries_needed, can_extra_geometries_needed) 

census_variables_communities_full_spatial_initial <- census_variables_communities_full_correct_direction %>%
  left_join(census_spatial_info, by = "geo_id") %>%
  select(-geometry) %>%
  left_join(spatial_alt_full, by = "geo_id") %>%
  st_set_geometry("geometry") %>%
  st_transform(5070) 

census_variables_communities_full_spatial_additional <- census_variables_communities_full_spatial_initial %>%
  filter(st_is_empty(geometry)) %>%
  st_drop_geometry() %>%
  left_join(all_extra_geometries_needed, by = "geo_id") %>%
  st_set_geometry("geometry") %>%
  st_transform(5070) 

census_variables_communities_full_spatial_initial_good_geoms <- census_variables_communities_full_spatial_initial %>% filter(!st_is_empty(geometry))

census_variables_communities_full_spatial <- rbind(census_variables_communities_full_spatial_initial_good_geoms, census_variables_communities_full_spatial_additional)


# double check all geometries are valid before rasterizing
census_variables_communities_full_spatial_valid_check <- st_is_valid(census_variables_communities_full_spatial)
unique(census_variables_communities_full_spatial_valid_check)
# [1] TRUE
# since no FALSE, all are valid

# rasterize vector ACS data to 100 m study area raster
poverty_rast <- terra::rasterize(census_variables_communities_full_spatial,
                                 study_area_rast,
                                          field = "poverty_inverted",
                                          #background = NA,
                                          fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

owner_rast <- terra::rasterize(census_variables_communities_full_spatial,
                               study_area_rast,
                                         field = "owner",
                                         #background = NA,
                                         fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

greater_than_200k_rast <- terra::rasterize(census_variables_communities_full_spatial,
                                           study_area_rast,
                                                    field = "greater_than_200k",
                                                    #background = NA,
                                                    fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

age_65_plus_rast <- terra::rasterize(census_variables_communities_full_spatial,
                                     study_area_rast,
                                              field = "age_65_plus_inverted",
                                              #background = NA,
                                              fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

disability_rast <- terra::rasterize(census_variables_communities_full_spatial,
                                    study_area_rast,
                                             field = "disability_inverted",
                                             #background = NA,
                                             fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

no_vehicle_rast <- terra::rasterize(census_variables_communities_full_spatial,
                                    study_area_rast,
                                             field = "no_vehicle_inverted",
                                             #background = NA,
                                             fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

# population_rast <- terra::rasterize(census_variables_communities_full_spatial,
#                                     study_area_rast,
#                                              field = "population",
#                                              background = NA,
#                                              fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

# average income: 200k+ and poverty level first to create income indicator
income_raster_stack <- c(poverty_rast, greater_than_200k_rast)
income_rast <- terra::mean(income_raster_stack, na.rm = TRUE)

# plot the raster
plot(poverty_rast)
plot(owner_rast)
plot(greater_than_200k_rast)
plot(age_65_plus_rast)
plot(disability_rast)
plot(no_vehicle_rast)
plot(income_rast)
#plot(population_rast)

# make raster writing function since needing to do it many times
write_rasters <- function(raster_obj, base_filename, template_raster, input_type = "continuous") {
  # Align to template raster
  raster_aligned <- align_raster_to_template(template_raster, raster_obj, input_type = input_type)
  
  # Write out aligned raster
  writeRaster(raster_aligned, paste0(base_filename, ".tif"), overwrite = TRUE)
  
  # # Optional: Create EPSG:4269 version
  # raster_4269 <- terra::project(raster_aligned, "EPSG:4269")
  # writeRaster(raster_4269, paste0(base_filename, "_4269.tif"), overwrite = TRUE)
}


# write out my rasters
#write_rasters(poverty_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_poverty"), study_area_rast)
write_rasters(owner_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_owners"), study_area_rast)
#write_rasters(greater_than_200k_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_greater_than_200k"), study_area_rast)
write_rasters(age_65_plus_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_age_65_plus"), study_area_rast)
write_rasters(disability_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_disability"), study_area_rast)
write_rasters(no_vehicle_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_no_vehicle"), study_area_rast)
write_rasters(income_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_recovery_income"), study_area_rast)
# write_rasters(population_rast, file.path(wri_project_root, "domains", "sense-of-place", "communities", "population_rast"))