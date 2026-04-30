wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# The purpose of this script is to download and prepare all American Community Survey (US Census data) variables at the tract and county level (county level used for gapfilling tracts). 
# This includes variables: age 65+, disability, no vehicle, renter-occupied housing units, poverty, and households earning 200k+ annually (the last two getting combined later to form the income indicator). The first three variables are used in the SVI (CDC Social Vulnerability Index) and the last three are used in the SoVI (University of South Carolina Social Vulnerability Index). This informed our decision to use them, but we chose our final variables based on relation to wildfire resilience as well.
# The output will be a csv file with all variables of interest for the study area states, which will later be combined with Canada's equivalents (if available) and then rasterized to 90 m.

#### Read in packages ####
library(tidycensus)
library(tidyverse)
library(sf)
library(terra)
# Set up census API key (replace 'Your.census.key.here' with your actual key)
# census_api_key('Your.census.key.here', install = TRUE) # can overwrite if needed; only run once
census_api_key <- Sys.getenv('CENSUS_API_KEY') # Retrieve key stored in .Renv file

# The below objects can be used for preliminary inspection of variable options
vars_acs_2023_detailed_b_c <- load_variables(2023, "acs5")
vars_acs_2023_s <- load_variables(2023, "acs5/subject")
vars_acs_2023_dp <- load_variables(2023, "acs5/profile")

# from SVI we want: aged 65 or older (pct_population_65_plus = S0101_C02_030), civilian with a disability (5+ years old) (pct_civilian_noninstitutionalized_population_with_disability = DP02_0072P), no vehicle (pct_housing_units_with_no_vehicle = DP04_0058P)
# from SoVI we want: poverty (pct_population_with_income_below_poverty_lvl = S0601_C01_049), renters (pct_renter_occupied_housing_units = S2501_C05_001/S2501_C01_001), households earning 200k+ annually (pct_households_with_200k_or_more_income = S1901_C01_011)
# we do not know for sure SoVI used those exact variable codes, but the SVI variable codes were obtained from the table at the bottom of: https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2022.html
# general use: S0101_C01_001 - total population. if population is 0, all values should be NA and domain score NA. will eventually want this to match mona and carlo's population data.


#### CENSUS API PREP ####



# create list of states of interest in US
interested_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK")


variables_of_interest <- c("S0101_C02_030", "DP02_0072P", "DP04_0058P", "S0601_C01_049", "S2501_C03_001", "S2501_C01_001", "S1901_C01_011", "S0101_C01_001")


#### GETTING TRACT DATA ####
# get 2018-2022 5 year ACS data (most recent) for tracts (can easily gapfill with county)
us_acs_variables_people <- map_df(interested_states, ~get_acs(geography = "tract",
                                                           variables = variables_of_interest,
                                                           state = .x,
                                                           geometry = TRUE,
                                                           year = 2023,
                                                           survey = "acs5",
                                                           key = census_api_key)) %>%
  rename(geo_id = GEOID)
sum(is.na(us_acs_variables_people$estimate))
# [1] 19192
# [1] 847 with new var for renters

# block_group_test <- get_acs(geography = "block group",
#         variables = c(paste0("B01001_", sprintf("%03d", 1:49))),
#         state = interested_states,
#         geometry = TRUE,
#         year = 2022,
#         survey = "acs5",
#         key = census_api_key)
# sum(is.na(test$estimate))

# for use in combining script
st_write(us_acs_variables_people, file.path(wri_project_root, "data", "communities", "raw", "2024", "acs", "us_acs_2019_2023_variables_communities.gpkg"))


# clean the tract data
us_acs_variables_people_cleaned <- us_acs_variables_people %>%
  select(-NAME, -moe) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  group_by(geo_id) %>%
  summarize(poverty = S0601_C01_049,
            owner = (S2501_C03_001/S2501_C01_001)*100, # seemed to be no percent version of this number. the one labelled percent is still an estimate, but all other values nested under that are percents.
            greater_than_200k = S1901_C01_011,
            age_65_plus = S0101_C02_030,
            disability = DP02_0072P,
            no_vehicle = DP04_0058P,
            population = S0101_C01_001
            )


#### GAPFILLING ####
# get county level for gapfilling too
us_acs_variables_people_gf <- map_df(interested_states, ~get_acs(geography = "county",
                                                                 variables = variables_of_interest,
                                                                 state = .x,
                                                                 geometry = TRUE,
                                                                 year = 2023,
                                                                 survey = "acs5",
                                                                 key = census_api_key)) %>%
  rename(geo_id = GEOID)

# saving to ensure we have the data
st_write(us_acs_variables_people_gf, file.path(wri_project_root, "data", "communities", "raw", "2024", "acs", "us_acs_2019_2023_variables_people_gf.gpkg"))

sum(is.na(us_acs_variables_people_gf$estimate))
# [1] 342
# [1] 0 with new var for renters, yay

na_summary <- us_acs_variables_people_gf %>%
  st_drop_geometry() %>%
  filter(is.na(estimate)) %>%
  group_by(variable) %>% 
  summarize(na_count = n()) 
# it all comes from S0501_C01_135, so find an alternative
# try S2501_C05_001
# this var works

# clean the county data that will be used for gapfilling
us_acs_variables_people_cleaned_gf <- us_acs_variables_people_gf %>%
  select(-NAME, -moe) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(geo_id_gf = geo_id) %>%
  group_by(geo_id_gf) %>%
  summarize(poverty_gf = S0601_C01_049,
            owner_gf = (S2501_C03_001/S2501_C01_001)*100, # seemed to be no percent version of this number. the variable labelled percent is still an estimate, but all other values nested under that are percents
            greater_than_200k_gf = S1901_C01_011,
            age_65_plus_gf = S0101_C02_030,
            disability_gf = DP02_0072P,
            no_vehicle_gf = DP04_0058P,
            population_gf = S0101_C01_001
  )

# set up which columns to gapfill
cols_to_fill <- c("poverty", "owner", "greater_than_200k", "age_65_plus", "disability", "no_vehicle") # can add more if desired

# for these columns, if tracts is NA, take value from county level (ONLY if pop > 0 -- 124 cases where pop = 0)
# geoid length 5 = county
# geoid length 11 = tract
us_acs_variables_people_full <- us_acs_variables_people_cleaned %>%
  mutate(geo_id_gf = substr(geo_id, 1, 5)) %>%
  left_join(us_acs_variables_people_cleaned_gf,
            by = c("geo_id_gf")
            ) %>% # add gf (county) data to the tract data
  mutate(across(all_of(cols_to_fill), 
                ~ ifelse(is.na(.) & population > 0, get(paste0(cur_column(), "_gf")), .) / 100)
         ) %>% # fill NAs when population > 0 with values from corresponding county level and get everything as proportions rather than percents; can also use case_when if there are more cases
  select(-ends_with("_gf")) # get rid of the gf columns that are no longer needed; before removing, check that things filled properly

# # reverse direction of all indicators except 200k household
# us_acs_variables_people_full_correct_direction <- us_acs_variables_people_full %>%
#   mutate(poverty_inverted = scales::rescale(poverty, to = c(1, 0)),
#          renter_inverted = scales::rescale(renter, to = c(1, 0)),
#          age_65_plus_inverted = scales::rescale(age_65_plus, to = c(1, 0)),
#          disability_inverted = scales::rescale(disability, to = c(1, 0)),
#          no_vehicle_inverted = scales::rescale(no_vehicle, to = c(1, 0))
#          ) %>%
#   select(-poverty, -renter, -age_65_plus, -disability, -no_vehicle)

# write the US tract ACS data out
write_csv(us_acs_variables_people_full, file.path(wri_project_root, "data", "communities", "int", "2024", "acs", "us_acs_2019_2023_variables_communities_full.gpkg")) # write out correct direction instead?
# this can just be a csv, there's no spatial info


# # add back in the spatial data
# # clean the tract data for only spatial info
# us_acs_variables_people_cleaned_spatial_only <- us_acs_variables_people %>%
#   select(-NAME, -moe) %>%
#   filter(variable == "S0101_C01_001") %>%
#   select(GEOID) %>%
#   distinct() # may not be needed, but just in case
# 
# # add in that spatial info
# us_acs_variables_people_full_spatial <- us_acs_variables_people_full_correct_direction %>%
#   left_join(us_acs_variables_people_cleaned_spatial_only, by = "GEOID") %>%
#   st_set_geometry("geometry") %>%
#   st_transform(5070)
# 
# 
# # double check all geometries are valid before rasterizing
# us_acs_variables_people_full_spatial_valid_check <- st_is_valid(us_acs_variables_people_full_spatial)
# unique(us_acs_variables_people_full_spatial_valid_check)
# # [1] TRUE
# # since no FALSE, all are valid
# 
# # visually check out the data
# # plot(us_acs_variables_people_full_spatial$geometry)
# # may cause a crash because it's a lot of shapes
# 
# # rasterize vector ACS data to 100 m study area raster
# acs_data_poverty_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                            study_area_raster_100m,
#                                            field = "poverty_inverted",
#                                            background = NA,
#                                            fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_renter_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                           study_area_raster_100m,
#                                           field = "renter_inverted",
#                                           background = NA,
#                                           fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_greater_than_200k_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                           study_area_raster_100m,
#                                           field = "greater_than_200k",
#                                           background = NA,
#                                           fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_age_65_plus_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                                     study_area_raster_100m,
#                                                     field = "age_65_plus_inverted",
#                                                     background = NA,
#                                                     fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_disability_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                                     study_area_raster_100m,
#                                                     field = "disability_inverted",
#                                                     background = NA,
#                                                     fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_no_vehicle_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                                     study_area_raster_100m,
#                                                     field = "no_vehicle_inverted",
#                                                     background = NA,
#                                                     fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# acs_data_population_rast <- terra::rasterize(us_acs_variables_people_full_spatial,
#                                                     study_area_raster_100m,
#                                                     field = "population",
#                                                     background = NA,
#                                                     fun = "mean")
# #touches = TRUE) # the function shouldn't be needed because we don't expect overlap, but just in case
# 
# # plot the raster
# plot(acs_data_poverty_rast)
# plot(acs_data_renter_rast)
# plot(acs_data_greater_than_200k_rast)
# plot(acs_data_age_65_plus_rast)
# plot(acs_data_disability_rast)
# plot(acs_data_no_vehicle_rast)
# plot(acs_data_population_rast)
# 
# # make raster writing function since needing to do it many times
# write_rasters <- function(raster_obj, base_filename) {
#   # write out current raster
#   writeRaster(raster_obj, paste0(base_filename, ".tif"), overwrite = TRUE)
#   
#   # create EPSG:4269 version
#   raster_obj_4269 <- raster_obj %>%
#     project(x = ., y = "EPSG:4269")
#   
#   # write it out
#   writeRaster(raster_obj_4269, paste0(base_filename, "_4269.tif"), overwrite = TRUE)
# }
# 
# # write out my rasters
# write_rasters(acs_data_poverty_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_poverty_data_rast"))
# write_rasters(acs_data_renter_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_renter_data_rast"))
# write_rasters(acs_data_greater_than_200k_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_data_greater_than_200k_rast"))
# write_rasters(acs_data_age_65_plus_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_data_age_65_plus_rast"))
# write_rasters(acs_data_disability_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_data_disability_rast"))
# write_rasters(acs_data_no_vehicle_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_data_no_vehicle_rast"))
# write_rasters(acs_data_population_rast, file.path(wri_project_root, "domains", "sense-of-place", "people", "acs_data_population_rast"))





# # gapfill option if we want to add the rows rather than attach the gf dataframe to the side
# # this wasn't working accurately, so not using this method. for example, 02090000700's values fill in 06071012300, 06071980200, 06071012202, and 06071980100 and i haven't been able to figure out why those get matched
# # for these columns, if tracts is NA, take value from county level
# us_acs_variables_people_cleaned_gf <- us_acs_variables_people_gf %>%
#   select(-NAME, -moe) %>%
#   st_drop_geometry() %>%
#   pivot_wider(names_from = variable, values_from = estimate) %>%
#   group_by(GEOID) %>%
#   summarize(poverty = S0601_C01_049,
#             renter = (S2501_C05_001/S2501_C01_001)*100, # seemed to be no percent version of this number. the variable labelled percent is still an estimate, but all other values nested under that are percents.
#             greater_than_200k = S1901_C01_011,
#             age_65_plus = S0101_C02_030,
#             disability = DP02_0072P,
#             no_vehicle = DP04_0058P,
#             population = S0101_C01_001
#   )
# 
# # add in the county data for use in gapfilling
# us_acs_variables_people_full <- bind_rows(us_acs_variables_people_cleaned, us_acs_variables_people_cleaned_gf)
#
# us_acs_variables_people_full_gapfilled <- us_acs_variables_people_full %>%
#   mutate(across(all_of(cols_to_fill), ~ case_when(
#     nchar(GEOID) == 11 & is.na(.) & population > 0 ~ .[match(substr(GEOID, 1, 5), substr(us_acs_variables_people_full$GEOID[nchar(us_acs_variables_people_full$GEOID) == 5], 1, 5))],
#     TRUE ~ .
#   ))) %>%
#   filter(nchar(GEOID) == 11) %>%
#   mutate(
#     across(all_of(cols_to_fill),
#            ~ . / 100) # get everything as proportions rather than percents
#   ) 



# # code to check out NAs using some detailed variables instead of all summary variables
# # try detailed to see if it has less NAs
# test <- map_df(interested_states, ~get_acs(geography = "tract",
#                                                                  variables = c("B25003_003", "B25003_001", "B19001_017", "B19001_001", "B17001_002", "B17001_001", "S0101_C02_030", "DP02_0072P", "DP04_0058P"),
#                                                                  state = .x,
#                                                                  geometry = TRUE,
#                                                                  year = 2022,
#                                                                  survey = "acs5",
#                                                                  key = census_api_key))
# sum(is.na(test$estimate))
# # [1] 482
#
# test_cleaned <- test %>%
#   select(-NAME, -moe) %>%
#   st_drop_geometry() %>%
#   pivot_wider(names_from = variable, values_from = estimate) %>%
#   group_by(GEOID) %>%
#   summarize(poverty = B17001_002/B17001_001,
#             renter = B25003_003/B25003_001,
#             greater_than_200k = B19001_017/B19001_001,
#             age_65_plus = S0101_C02_030,
#             disability = DP02_0072P,
#             no_vehicle = DP04_0058P
#             )
#
# sum(is.na(test_cleaned))
# # [1] 1036
#
# # then try with all detailed variables instead? could do down the line