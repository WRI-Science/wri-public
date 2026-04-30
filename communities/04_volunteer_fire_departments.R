wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse)
library(tidycensus)
library(pdftools)
library(sf)
library(cancensus)
library(tidygeocoder)
library(terra)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))
census_api_key <- Sys.getenv('CENSUS_API_KEY') # retrieve key stored in Renv file
study_area_lvl_2 <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_2.shp")) # for the vector shapes for the data
study_area_raster <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")) # study area raster to rasterize to
study_area_raster_moll <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na_moll.tif")) # study area raster because we use nearest feature and want spacing to be equal (distance calc)
study_area_vect <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_0_moll.shp"))

# US
# read in US fire department data: https://apps.usfa.fema.gov/registry/download
us_fire_depts <- read_csv(file.path(wri_project_root, "data", "communities", "us_fire_departments", "usfa-registry-national.csv")) %>%
  janitor::clean_names()

us_fire_stations <- read_csv(file.path(wri_project_root, "data", "communities", "us_fire_departments", "usfa-registry-station.csv")) %>%
  janitor::clean_names() %>%
  select(fdid, fire_dept_name, hq_state, county, station_name, dept_type)
# > unique(us_fire_stations$dept_type)
# [1] "Volunteer"        "Career"          
# [3] "Mostly volunteer" "Mostly career"  

# create summary table (for GitHub)
# note: volunteer firefighters are ~80% of wildfire fighting capacity but they often don't have places they associate with
# reason to potentially include paid per call too: https://www.ci.fridley.mn.us/Faq.aspx?QID=278
us_fire_depts_summ <- us_fire_depts %>%
  group_by(dept_type) %>%
  summarize(career_firefighters = sum(active_firefighters_career),
            volunteer_firefighters = sum(active_firefighters_volunteer),
            paid_per_call_firefighters = sum(active_firefighters_paid_per_call),
            total_firefighters = career_firefighters + volunteer_firefighters + paid_per_call_firefighters,
            number_of_depts = n())

# set up lists of states we want as well as full names associated with abbreviations for use later
interested_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")
state_abbreviations <- c(
  "New Mexico" = "NM", 
  "Arizona" = "AZ",
  "California" = "CA",
  "Nevada" = "NV",
  "Utah" = "UT",
  "Colorado" = "CO",
  "Montana" = "MT",
  "Idaho" = "ID",
  "Wyoming" = "WY",
  "Washington" = "WA",
  "Oregon" = "OR",
  "Alaska" = "AK"
)

# right now this is keeping volunteer and mostly volunteer dept_type for our purposes
# group by and summarize counts by county
# this is the part i would change if, for example, instead we want all fire depts and to count up the number of volunteer and paid per call here. we do likely want to count firefighters rather than departments, but don't know in exactly what form yet
us_fire_depts_w_stations <- us_fire_depts %>%
  select(fdid, fire_dept_name, hq_state, county, dept_type) %>%
  full_join(., us_fire_stations, by = c("fdid", "dept_type", "fire_dept_name", "hq_state", "county"))

nrow(us_fire_depts_w_stations) - sum(is.na(us_fire_depts_w_stations$station_name)) == nrow(us_fire_stations) - sum(is.na(us_fire_stations$station_name)) # should be TRUE to ensure all stations got captured
#sum(!is.na(us_fire_dpts_w_stations$station_name)) + sum(is.na(us_fire_stations$station_name)) == nrow(us_fire_stations) # should be TRUE to ensure all stations got captured

us_fire_stations_clean <- us_fire_depts_w_stations %>%
  filter(dept_type %in% c("Volunteer", "Mostly volunteer") & hq_state %in% interested_states) %>%
  rename(state = hq_state) %>%
  group_by(county, state) %>%
  summarize(
    num_vol_fire_stations = n()
  )

# test out ppc and vol firefighters instead
# us_fire_stations_clean <- us_fire_dpts %>%
#   filter(hq_state %in% interested_states) %>%
#   rename(state = hq_state) %>%
#   group_by(county, state) %>%
#   summarize(
#     num_vol_fire_dpts = sum(active_firefighters_volunteer + active_firefighters_paid_per_call)
#   )

# get the geo_ids for each county of interest
county_geo_ids <- map_df(interested_states, ~get_acs(geography = "county",
                                                              variables = "S0101_C01_001", # arbitrary variable (chose one of the most basic ones)
                                                              state = .x,
                                                              geometry = TRUE,
                                                              year = 2022,
                                                              survey = "acs5",
                                                              key = census_api_key)) %>%
  st_drop_geometry() # we will use our own study area file for consistency with other domains

# clean up the geoid data so that it can attach to the fire dept data
county_geo_ids_clean <- county_geo_ids %>%
  select(geo_id = GEOID, county = NAME, population = estimate) %>%
  separate(county, into = c("county", "state"), sep = ", ", remove = FALSE) %>% # get county and state separate
  mutate(county = str_replace(county, " (County|Borough|Census Area|Municipality|City and Borough)", ""),
         county = str_replace(county, "-", " "),
         county = str_replace(county, "Lake and Peninsula", "Lake And Peninsula"),
         county = str_replace(county, " of ", " Of "),
         county = str_replace(county, "ñ", "n")) %>% # swap/remove parts to enable matching
  mutate(state = recode(state, !!!state_abbreviations)) # swap full state names to abbreviations

# join county geo ids/geometries to the us fire depts
us_fire_stations_clean_w_geo_ids <- county_geo_ids_clean %>%
  left_join(us_fire_stations_clean, by = c("county", "state")) %>%
  mutate(num_vol_fire_stations = replace_na(num_vol_fire_stations, 0),
         prop_vol_fire_stations = num_vol_fire_stations/population#,
         #prop_vol_fire_stations_rescaled = scales::rescale(prop_vol_fire_stations, to = c(0, 1))
         ) %>%
  select(-county) %>% # get rid of this column because the names don't exactly match with the county column in the study area file
  left_join(study_area_lvl_2, by = c("geo_id" = "stco_fipsc")) %>%
  st_set_geometry("geometry") %>%
  select(prop_vol_fire_stations) %>%
  st_transform(., st_crs(study_area_raster))
  

# double check all geometries are valid before rasterizing
us_fire_stations_clean_w_geo_ids_valid_check <- st_is_valid(us_fire_stations_clean_w_geo_ids)
unique(us_fire_stations_clean_w_geo_ids_valid_check)
# [1] TRUE
# since no FALSE, all are valid

# visually check out the data
plot(us_fire_stations_clean_w_geo_ids)

# # rasterize vector fire departments to 100 m study area raster
# us_fire_stations_rast <- terra::rasterize(us_fire_stations_clean_w_geo_ids,
#                                    study_area_raster,
#                                    field = "num_vol_fire_stations",
#                                    background = NA,
#                                    fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# # plot the raster
# plot(us_fire_stations_rast)


# Canada
# read in BC fire department data: https://www2.gov.bc.ca/assets/gov/public-safety-and-emergency-services/public-safety/fire-safety/fire_dept_listing.pdf

# get BC fire data
# https://catalogue.data.gov.bc.ca/dataset/first-responders
bc_fire_stations <- st_read(file.path(wri_project_root, "data", "infrastructure", "raw", "fire_stations", "2024", "BCGW_02001F02_1748559278610_16688", "GSR_FIRST_RESPONDERS_SVW", "FRST_RSPND_point.shp"))

# clean up the BC fire department df
bc_fire_stations_clean <- bc_fire_stations %>%
  janitor::clean_names() %>%
  filter(grepl("Vol", fclty_nm, ignore.case = TRUE)) #%>% # we just want volunteer departments
  #mutate(across(everything(), ~ na_if(., ""))) # fill in NA values instead of blanks/empties

# bc_fire_stations_clean_sf <- bc_fire_stations_clean %>%
#   st_drop_geometry() %>%
#   st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) # convert to sf object with lat/long coords

# set up regions and generic variable of interest (we just care about the geometries)
regions_of_interest <- c("59", "60")
variables_of_interest <- c("v_CA21_1")

# get BC and YK (for later) divisions shapefiles for spatial joining
bc_yk_div_sf <- get_census(dataset = 'CA21', 
                           regions = list(PR = regions_of_interest),
                           vectors = variables_of_interest, 
                           level = 'CD')

# filter to just BC divs for use now
bc_div_sf <- bc_yk_div_sf %>%
  filter(PR_UID == "59") %>%
  select(name = "Region Name", geo_id = GeoUID, population = "v_CA21_1: Population, 2021") %>%
  left_join(study_area_lvl_2, by = c("geo_id" = "cduid")) %>%
  select(name, geo_id, geometry, population) %>%
  st_set_geometry("geometry") %>%
  st_transform(., st_crs(study_area_raster_moll))
  

# get BC and YK (for later) province geometries for use in a visual check
bc_yk_prov_sf <- get_census(dataset = 'CA21', 
                            regions = list(PR = regions_of_interest),
                            vectors = variables_of_interest, 
                            level = 'PR', 
                            geo_format = "sf") # keep geometry for plotting

# filter to just BC prov for use now
bc_prov_sf <- bc_yk_prov_sf %>%
  filter(GeoUID == "59") %>%
  select(name, geometry) %>%
  st_transform(., st_crs(study_area_raster_moll))

# convert fire station points to match the CRS of interest
bc_fire_stations_sf <- bc_fire_stations_clean %>%
  st_transform(., st_crs(study_area_raster_moll)) # transform to match the study area raster

# visually check the geocoded BC points fall within the BC area, to ensure projecting didn't mess with anything
plot(bc_prov_sf$geometry)
plot(bc_fire_stations_sf$geometry, col = "pink", add = TRUE)
# close enough

# read in alt divisions data source
#divs_alt_source <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "canada_census_divisions", "canada-census-divisions.shp")) %>%
  #st_transform(., st_crs(study_area_raster_moll)) # transform to match the study area raster

# join the points with the divisions polygons to see which fall in each polygon
bc_fire_stations_sf_with_divs <- st_join(bc_fire_stations_sf, bc_div_sf, join = st_intersects) # st_within caused many to not get filled, but is the more ideal function. using st_nearest_feature gave the same results as st_within, but with the NAs filled, because it is more general. switched to st_intersects so that points if they are on a boundary get included
# for now, we are hoping this is accurate. need to manually check.
# if some didn't get filled, manually look to see which division they are in
# using cancensus boundaries causes points to fall off (check other projections?)
bc_fire_stations_sf_with_divs_nas <- bc_fire_stations_sf_with_divs %>%
  filter(is.na(name)) %>%
  select(-name, -geo_id, -population)

# replot to see where the NA points are if needed
plot(bc_div_sf$geometry)
plot(bc_fire_stations_sf_with_divs_nas$geometry, col = "pink", add = TRUE)

# deal with these ones with st_nearest_feature and check that it worked correctly
bc_fire_stations_sf_with_divs_nas_filled <- st_join(bc_fire_stations_sf_with_divs_nas, bc_div_sf, join = st_nearest_feature)

# summarize volunteer fire department counts for each BC division
bc_fire_stations_sf_with_divs_sums <- bc_fire_stations_sf_with_divs %>%
  group_by(name, geo_id, population) %>%
  summarize(num_vol_fire_stations = n()) %>%
  mutate(prop_vol_fire_stations = num_vol_fire_stations/population) %>%
  st_drop_geometry() %>%
  right_join(bc_div_sf, by = c("name", "geo_id")) %>%
  st_set_geometry("geometry") %>%
  select(prop_vol_fire_stations) %>%
  mutate(prop_vol_fire_stations = replace_na(prop_vol_fire_stations, 0)) %>%
  st_transform(., st_crs(study_area_raster))

# double check all geometries are valid before rasterizing
bc_fire_stations_sf_with_divs_sums_valid_check <- st_is_valid(bc_fire_stations_sf_with_divs_sums)
unique(bc_fire_stations_sf_with_divs_sums_valid_check)
# [1] TRUE
# since no FALSE, all are valid

# visually check out the data
plot(bc_fire_stations_sf_with_divs_sums)

# # rasterize vector fire departments to 100 m study area raster
# bc_fire_stations_rast <- terra::rasterize(bc_fire_stations_sf_with_divs_sums,
#                                       study_area_raster,
#                                       field = "num_vol_fire_stations",
#                                       background = NA,
#                                       fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# # plot the raster
# plot(bc_fire_stations_rast)
  
  

# Yukon
# https://yukon.ca/en/employment/find-volunteer-opportunity/become-volunteer-firefighter
# read in YK volunteer fire department names
# SWITCH TO LUC'S DATA SOURCE
# The processed version of YK fire stations is created in:
# infrastructure/resistance/4-fire_resource_density/fire_station_proximity.ipynb
yk_fire_stations <- st_read(file.path(wri_project_root, "data", "infrastructure", "raw", "fire_stations", "2023", "yukon-fire-resources", "processed", "yukon_fire_departments.shp"))

# filter to only volunteer
yk_fire_stations_sf <- yk_fire_stations %>%
  filter(volunteer_ == 1) %>%
  st_transform(., st_crs(study_area_raster_moll)) # transform to match the study area raster

# grab just the YK div data from the cancensus data we grabbed earlier
yk_div_sf <- bc_yk_div_sf %>%
  filter(PR_UID == "60") %>%
  select(name = "Region Name", geo_id = GeoUID, population = "v_CA21_1: Population, 2021") %>%
  left_join(study_area_lvl_2, by = c("geo_id" = "cduid")) %>% # use this instead of the census API data for consistency with other domains
  select(name, geo_id, geometry, population) %>%
  st_set_geometry("geometry") %>%
  st_transform(., st_crs(study_area_raster_moll))

# grab just the YK prov data from the cancensus data we grabbed earlier
yk_prov_sf <- bc_yk_prov_sf %>%
  filter(GeoUID == "60") %>%
  select(name, geometry) %>%
  st_transform(., st_crs(study_area_raster_moll))

# check the YK fire department points fall within the YK prov area
plot(yk_prov_sf$geometry)
plot(yk_fire_stations_sf$geometry, col = "pink", add = TRUE)
# close enough

# join the points with the divisions polygons to see which fall in each polygon
yk_fire_stations_sf_with_divs <- st_join(yk_fire_stations_sf, yk_div_sf, join = st_within) # yk only has one division, but following this process for consistency and can more easily translate it to another geography if needed

# summarize volunteer fire department counts for each YK division (only one)
yk_fire_stations_sf_with_divs_sums <- yk_fire_stations_sf_with_divs %>%
  group_by(name, geo_id, population) %>%
  summarize(num_vol_fire_stations = n()) %>%
  mutate(prop_vol_fire_stations = num_vol_fire_stations/population) %>%
  st_drop_geometry() %>%
  right_join(yk_div_sf, by = c("name", "geo_id")) %>%
  st_set_geometry("geometry") %>%
  select(prop_vol_fire_stations) %>%
  mutate(prop_vol_fire_stations = replace_na(prop_vol_fire_stations, 0)) %>%
  st_transform(., st_crs(study_area_raster))

# double check all geometries are valid before rasterizing
yk_fire_stations_sf_with_divs_sums_valid_check <- st_is_valid(yk_fire_stations_sf_with_divs_sums)
unique(yk_fire_stations_sf_with_divs_sums_valid_check)
# [1] TRUE
# since no FALSE, all are valid

# visually check out the data
plot(yk_fire_stations_sf_with_divs_sums)

# # rasterize vector fire departments to 100 m study area raster
# yk_fire_stations_rast <- terra::rasterize(yk_fire_stations_sf_with_divs_sums,
#                                       study_area_raster,
#                                       field = "num_vol_fire_stations",
#                                       background = NA,
#                                       fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# # plot the raster
# plot(yk_fire_stations_rast)

# combine csvs into one for rescaling with both countries together
fire_stations_full <- rbind(us_fire_stations_clean_w_geo_ids, bc_fire_stations_sf_with_divs_sums, yk_fire_stations_sf_with_divs_sums)
fire_stations_full_rescaled <- fire_stations_full %>%
  mutate(prop_vol_fire_stations_rescaled = scales::rescale(prop_vol_fire_stations, to = c(0, 1)))

# rasterize vector fire departments to 100 m study area raster
fire_stations_rast <- terra::rasterize(fire_stations_full_rescaled,
                                      study_area_raster,
                                      field = "prop_vol_fire_stations_rescaled",
                                      background = NA,
                                      fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

# plot the raster
plot(fire_stations_rast)

# align to template
fire_stations_rast_aligned <- align_raster_to_template(study_area_raster, fire_stations_rast, input_type = "continuous")

# write the full raster
#merged_fire_dpt_raster <- merge(us_fire_stations_rast, bc_fire_stations_rast, yk_fire_stations_rast)
writeRaster(fire_stations_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_vol_fire_stations.tif"), overwrite = TRUE) # this is currently in 5070

# merged_fire_dpt_raster_4269 <- fire_stations_rast %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(merged_fire_dpt_raster_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "vol_fire_depts_4269.tif"), overwrite = TRUE)