library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(sf)
library(terra)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))
census_api_key <- Sys.getenv('CENSUS_API_KEY') # retrieve key stored in Renv file
study_area_lvl_2 <- st_read("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_2.shp") # for the vector shapes for the data
study_area_raster <- rast("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif") # study area raster to rasterize to

# US
# read in the data
us_firewise <- read_csv("/home/shares/wwri-wildfire/data/communities/raw/us_firewise_communities/us_firewise_communities.csv", col_names = "community_name")

# filter by states of interest and add state name info
state_names <- c("New Mexico", "Arizona", "California", "Nevada", "Utah", "Colorado", "Montana", "Idaho", "Wyoming", "Washington", "Oregon", "Alaska")
us_firewise_states_added <- us_firewise %>%
  mutate(state = if_else(!str_detect(community_name, ","), community_name, NA_character_)) %>% # begin the state column
  fill(state) %>% # fill in the states for every row
  filter(state %in% state_names) %>% # get only states of interest
  mutate(community_name = if_else(community_name == state, NA, community_name)) %>% # make NA the community names that are just the state name (this is from how the data was organized)
  drop_na(community_name) %>% # get rid of those NAs just created
  mutate(community_name = paste(community_name, state, sep = ", ")) # add the state into the community name column too for geododing

# get just counties (needed for current process of summarizing by county)
us_firewise_counties <- us_firewise_states_added %>%
  mutate(county = str_replace(community_name, "^(.+,\\s)*([^,]+)(,\\s[^,]+)$", "\\2"))

# attach the geo_ids for each county of interest without geocoding first
interested_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")
county_geo_ids <- map_df(interested_states, ~get_acs(geography = "county",
                                                     variables = "S0101_C01_001", # arbitrary variable (chose one of the most basic ones)
                                                     state = .x,
                                                     geometry = TRUE,
                                                     year = 2022,
                                                     survey = "acs5",
                                                     key = census_api_key))

# clean the county geo ids
county_geo_ids_clean <- county_geo_ids %>%
  st_drop_geometry() %>%
  select(geo_id = GEOID, county = NAME, population = estimate) %>%
  separate(county, into = c("county", "state"), sep = ", ", remove = FALSE) # need county and state because some states have counties by the same name

# add firewise communities to the geo ids based on county and state
us_firewise_clean_w_geo_ids <- county_geo_ids_clean %>%
  left_join(us_firewise_counties, by = c("county", "state"))

# these communities did not get assigned to a geo id
us_firewise_no_match <- us_firewise_counties %>%
  left_join(county_geo_ids_clean, by = c("county", "state")) %>%
  filter(is.na(geo_id))
# 7 of them

# take care of the no matches geographically
# clean up the names so they are geocoded properly
us_firewise_no_match_clean <- us_firewise_no_match %>%
  mutate(address = str_trim(str_extract(community_name, "(?<=, )[^,]+, [^,]+$")),
         address = ifelse(is.na(address),
                          sapply(strsplit(community_name, ","), function(x) {
                            # get rid of empty parts due to double commas
                            x <- x[x != ""]
                            if (length(x) >= 2) {
                              return(paste(trimws(x[(length(x) - 1)]), trimws(x[length(x)]), sep = ", "))
                            } else {
                              return(NA) # return NA if not enough parts
                            }
                          }),
                          address))
  
# run the geocoding
geocoded_us_firewise_no_match_osm <- us_firewise_no_match_clean %>%
  
  geocode(address, method = "osm")
sum(is.na(geocoded_us_firewise_no_match_osm$lat))
# [1] 0

# add second round here in case down the line in subsequent years it's needed?

# match up the communities to the county geo ids geographically
us_firewise_no_match_sf <- st_as_sf(geocoded_us_firewise_no_match_osm, coords = c("long", "lat"), crs = 4326) %>%
  select(-geo_id, -population)

county_geo_ids_for_join <- county_geo_ids %>%
  st_transform(crs = 4326) %>%
  select(geo_id = GEOID, name = NAME, population = estimate)


# check the points fall within the area
plot(county_geo_ids_for_join$geometry)
plot(us_firewise_no_match_sf$geometry, col = "pink", add = TRUE)
# they do

# join the points with the divisions polygons to see which fall in each polygon
us_firewise_no_match_sf_with_counties <- st_join(us_firewise_no_match_sf, county_geo_ids_for_join, join = st_within) # can do st_nearest_feature if this doesn't work
# park city is in two counties. currently counting it in its majority county only. googled the specific place and it is in that county.

# going to manually check these counties are correct because communities can potentially span multiple counties
# all seem good so can drop the extra info now
us_firewise_no_match_sf_with_counties <- us_firewise_no_match_sf_with_counties %>%
  st_drop_geometry() %>%
  select(-address, -name)

# if a county does not have any community, make that count 0
us_firewise_clean_w_geo_ids_nas <- us_firewise_clean_w_geo_ids %>%
  filter(is.na(community_name)) %>%
  mutate(count = 0) %>%
  select(geo_id, count)
length(unique(us_firewise_clean_w_geo_ids_nas$geo_id)) # should match length of df. 248. and it does

# get the rest of the data where there are no NAs
us_firewise_clean_w_geo_ids_no_nas <- us_firewise_clean_w_geo_ids %>%
  filter(!is.na(community_name))

# combine the non-NA data with the fixed no matches
us_firewise_geo_ids_full <- rbind(us_firewise_clean_w_geo_ids_no_nas, us_firewise_no_match_sf_with_counties)

# combine that with the NA converted to 0 count data and find the rest of the counts
us_firewise_geo_ids_full_counts <- us_firewise_geo_ids_full %>%
  group_by(geo_id, population) %>%
  summarize(count = n()) %>%
  rbind(us_firewise_clean_w_geo_ids_nas) # add in the 0s

# there may be duplicate geo_ids, ie. one county may have been NA, but actually has one or more once the no match is counted
# let's account for that
us_firewise_geo_ids_full_counts_no_dupes <- us_firewise_geo_ids_full_counts %>%
  group_by(geo_id) %>%
  filter(count == max(count)) %>%
  mutate(count_binary = if_else(count >= 1, 1, 0)) %>%
  ungroup() %>%
  left_join(study_area_lvl_2, by = c("geo_id" = "stco_fipsc")) %>%
  st_set_geometry("geometry") %>%
  select(count_binary) %>%
  st_transform(., st_crs(study_area_raster))


# double check all geometries are valid before rasterizing
us_firewise_geo_ids_full_counts_no_dupes_valid_check <- st_is_valid(us_firewise_geo_ids_full_counts_no_dupes)
unique(us_firewise_geo_ids_full_counts_no_dupes_valid_check)
# [1] TRUE
# since no FALSE, all are valid

# visually check out the data
plot(us_firewise_geo_ids_full_counts_no_dupes)

# rasterize vector fire departments to 100 m study area raster
us_firewise_comms_rast <- terra::rasterize(us_firewise_geo_ids_full_counts_no_dupes,
                                      study_area_raster,
                                      field = "count_binary",
                                      #background = NA,
                                      fun = "mean")
#touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case

# plot the raster
plot(us_firewise_comms_rast)

# align the raster to the template
us_firewise_comms_rast_aligned <- align_raster_to_template(study_area_raster, us_firewise_comms_rast, input_type = "categorical")

# write out the full raster
writeRaster(us_firewise_comms_rast, "/home/shares/wwri-wildfire/final_layers/2024/communities/indicators/communities_resistance_firewise_comms.tif", overwrite = TRUE) # this is currently in 5070

# us_firewise_comms_rast_4269 <- us_firewise_comms_rast %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(us_firewise_comms_rast_4269, "/home/shares/wwri-wildfire/domains/sense-of-place/people/firewise_comms_4269.tif", overwrite = TRUE)

# Canada
# as of right now, we don't have access to a Canada equivalent. FireSmart Canada may be similar enough but the list of neighbourhoods is not available on the website and we were told via email that we would not be able to get access.