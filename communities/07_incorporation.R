wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf)
library(tidyverse)
library(tidycensus)
library(terra)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))

census_api_key <- Sys.getenv('CENSUS_API_KEY')
study_area_raster <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")) # study area raster to rasterize to


incorporation_data_folder_path <- file.path(wri_project_root, "data", "multi_domain_data", "raw", "boundary_layers", "us_census_designated_places", "2024")
incorporation_data_shapefile_paths <- list.files(incorporation_data_folder_path, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
incorporation_data_shapefile <- lapply(incorporation_data_shapefile_paths, st_read) %>%
  bind_rows()

# > table(incorporation_data_shapefile$CLASSFP)
# 
# C1   C7   C8   C9   M2   U1   U2 
# 2316    1    1    4   36 3320  241 
# codes defined here: https://www.census.gov/library/reference/code-lists/class-codes.html

incorporation_data_shapefile_filtered <- incorporation_data_shapefile %>%
  filter(CLASSFP %in% c("C1", "C7", "C8"))

# check validity
incorporation_data_shapefile_filtered_valid_check <- st_is_valid(incorporation_data_shapefile_filtered)
unique(incorporation_data_shapefile_filtered_valid_check)
# [1] TRUE

# convert to correct crs
incorporation_data_shapefile_filtered_transformed_for_rast <- incorporation_data_shapefile_filtered %>% 
  st_transform(., st_crs(study_area_raster))
unique(st_is_valid(incorporation_data_shapefile_filtered_transformed_for_rast)) # just to doublecheck nothing changed with the transformation

# # make each shape worth 1
# incorporation_data_shapefile_filtered_with_val <- incorporation_data_shapefile_filtered_transformed_for_rast %>%
#   mutate(value = 1)

# rasterize using sum to get the number of cwpps touching each cell
us_incorporation_rast <- terra::rasterize(incorporation_data_shapefile_filtered_transformed_for_rast,
                                     study_area_raster,
                                     field = 1,
                                     background = 0)
# none of them overlap, so it's all just 1

# study_area_mask_NA <- classify(study_area_raster, cbind(0, NA), include.lowest = TRUE) # fix study area rast to have NA instead of 0 for use in masking
# us_incorporation_masked_raster <- mask(us_incorporation_rast, study_area_mask_NA)

# plot to check
plot(us_incorporation_rast)


# Canada
# https://www2.gov.bc.ca/gov/content/governments/local-governments/governance-powers/incorporation-restructuring/incorporation-classification
# villages, towns, districts, cities
library(cancensus)
#### PULL ALL CANCENSUS DATA NEEDED ####
# follow instructions here to get key: https://mountainmath.github.io/cancensus/
# set_cancensus_api_key("API_KEY", install = TRUE) # only need to do once
# set_cancensus_cache_path("/home/egg/tmp", install = TRUE) # only need to do once
csd_data <- list_census_regions("CA21") %>%
  filter(level == "CSD" & PR_UID %in% c("59", "60"))
incorporated_types <- c("RGM", "CY", "DM", "VL", "T")
# https://www12.statcan.gc.ca/census-recensement/2021/ref/symb-ab-acr-eng.cfm#cst
# https://wiki.openstreetmap.org/wiki/Canada_admin_level
# C – City
# T – Town
# VL – Village
# RGM – Regional Municipality
# DM – District Municipality ?

# csd_data_bc <- read_csv(file.path(wri_project_root, "data", "sense-of-place-domain-data", "canada-incorporated-places", "CSD_bc.csv"))
# csd_data_yk <- read_csv(file.path(wri_project_root, "data", "sense-of-place-domain-data", "canada-incorporated-places", "CSD_yk.csv"))
# csd_data_combined <- rbind(csd_data_bc, csd_data_yk)

# csd_data_combined_names <- csd_data_combined %>%
#   select(name = CSDname)
# 
# csd_data_names <- csd_data %>%
#   select(name)
# 
# View(setdiff(csd_data_combined_names, csd_data_names))
# View(setdiff(csd_data_names, csd_data_combined_names))
# 
# unique(csd_data_combined$CSDtype)
# unique(csd_data$municipal_status)

# csd_data_combined_incorporated <- csd_data_combined %>%
#   filter(CSDtype %in% incorporated_types)

csd_data_incorporated <- csd_data %>%
  filter(municipal_status %in% incorporated_types) %>%
  select(geo_id = region)

regions_of_interest <- c("59", "60")
can_subdivs_spatial_only <- get_census(dataset = 'CA21', 
                                       regions = list(PR = regions_of_interest),
                                       vectors = c("v_CA21_1"), 
                                       level = 'CSD',
                                       geo_format = "sf") %>%
  select(geo_id = GeoUID) %>%
  distinct() %>% # may not be needed, but just in case
  st_transform(., st_crs(study_area_raster))

csd_data_incorporated_spatial <- csd_data_incorporated %>%
  left_join(., can_subdivs_spatial_only, by = c("geo_id")) %>%
  #mutate(value = 1) %>%
  st_set_geometry("geometry")

# rasterize using sum to get the number of cwpps touching each cell
can_incorporation_raster <- terra::rasterize(csd_data_incorporated_spatial,
                                          study_area_raster,
                                          field = 1,
                                          background = 0)
# none of them overlap, so it's all just 1

#study_area_mask_NA <- classify(study_area_raster, cbind(0, NA)) # fix study area rast to have NA instead of 0 for use in masking
# can_incorporation_masked_raster <- mask(can_incorporation_raster, study_area_raster)

# plot to check
plot(can_incorporation_raster)
  

# combine canada and US data (can do this after rasterizing because no rescaling needed right now)
combined_incorporation_raster <- mosaic(us_incorporation_rast, can_incorporation_raster, fun = "max")

plot(combined_incorporation_raster)

# align to template
combined_incorporation_raster_aligned <- align_raster_to_template(study_area_raster, combined_incorporation_raster, input_type = "categorical")

plot(combined_incorporation_raster_aligned)

# write out the full raster
writeRaster(combined_incorporation_raster_aligned, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_incorporation.tif"), overwrite = TRUE) # this is currently in 5070

# combined_incorporation_raster_4269 <- combined_incorporation_raster %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(combined_incorporation_raster_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "incorporation_4269.tif"), overwrite = TRUE)













# # check intersections with counties
# # attach the geo_ids for each county of interest
# interested_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")
# county_geo_ids <- map_df(interested_states, ~get_acs(geography = "county",
#                                                      variables = "S0101_C01_001", # arbitrary variable (chose one of the most basic ones)
#                                                      state = .x,
#                                                      geometry = TRUE,
#                                                      year = 2022,
#                                                      survey = "acs5",
#                                                      key = census_api_key)) %>%
#   select(geo_id = GEOID, name = NAME)
# 
# st_crs(county_geo_ids) == st_crs(incorporation_data_shapefile_filtered) # check the crs is the same
# # [1] TRUE
# incorporation_sf_with_counties <- st_join(incorporation_data_shapefile_filtered, county_geo_ids, join = st_intersects) # st_nearest_feature does not grab the correct feature for Anchorage
# 
# explore_duplicates <- incorporation_sf_with_counties %>%
#   group_by(NAME, GEOID) %>%
#   summarize(count = n()) %>%
#   filter(count > 1)
# 
# no_duplicates <- incorporation_sf_with_counties %>%
#   group_by(NAME, GEOID) %>%
#   summarize(count = n()) %>%
#   filter(count == 1)
# 
# # anchorage_polygons <- incorporation_data_shapefiles_list[[1]] %>%
# #   filter(NAME == "Anchorage")
# # 
# # anchorage_matches <- county_geo_ids %>%
# #   filter(name %in% c("Matanuska-Susitna Borough, Alaska", "Kenai Peninsula Borough, Alaska", "Anchorage Municipality, Alaska", "Chugach Census Area, Alaska"))
# # 
# # plot(anchorage_polygons$geometry, col = "red")
# # plot(anchorage_matches$geometry[[3]], add = TRUE)
# 
# # a couple of them just barely touches
# # 3 is a perfect match
# # this is why i'm choosing to take just the majority area one if area is 99% in one county
# 
# 
# explore_duplicates_valid <- st_is_valid(explore_duplicates)
# county_geo_ids_valid <- st_is_valid(county_geo_ids)
# unique(explore_duplicates_valid)
# unique(county_geo_ids_valid)
# 
# # explore_duplicates <- st_make_valid(explore_duplicates)
# # county_geo_ids <- st_make_valid(county_geo_ids)
# 
# intersections <- st_intersection(explore_duplicates, county_geo_ids)
# 
# intersections_valid_check <- st_is_valid(intersections) # make valid if needed
# unique(intersections_valid_check)
# valid_intersections <- st_make_valid(intersections)
# intersections_valid_check <- st_is_valid(intersections)
# unique(intersections_valid_check)
# 
# 
# 
# incorporation_data_fixed_dupes <- valid_intersections  %>%
#   mutate(overlap_area = st_area(.)) %>%
#   #filter(NAME == "Anchorage") %>%
#   group_by(NAME, GEOID) %>%
#   mutate(total_area = sum(overlap_area),
#          area_pct = round(as.numeric((overlap_area/total_area)*100), 8),
#          check_100 = sum(area_pct)) %>%
#   #filter(area_prop >= 99) # this would filter out cases where the overlap is 33-33-33 for example, which we dont want to happen
#   filter(!(area_pct <= 0.005)) %>% # this would remove any low areas while keeping split cases
#   select(colnames(no_duplicates))
# 
# incorporation_data_full <- rbind(incorporation_data_fixed_dupes, no_duplicates)
