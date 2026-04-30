wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyhydat)
library(tidyverse)
library(sf)
# download_hydat() # necessary for first time; done most recently on 5/22/2025; version is stated to be from 2025-04-15

# start out by grabbing all stations (any could be used to fill hydrobasins)
all_stations <- hy_stations() %>%
  janitor::clean_names() #%>%
#filter(prov_terr_state_loc %in% c("ME", "MN", "MI", "MT", "ND", "AK", "WA", "ID", "YT", "BC"))

# get just the numbers
all_stations_list <- all_stations$station_number

# check out agencies
agency_list <- hy_agency_list() # if curious

# pull monthly streamflow data for all stations
monthly_data <- hy_monthly_flows(station_number = all_stations_list) %>%
  janitor::clean_names()

# add the station information to the monthly data
monthly_data_w_station_info <- left_join(monthly_data, all_stations, by = "station_number") %>%
  drop_na(value)

# write out unfiltered dataset
write_csv(monthly_data_w_station_info, file.path(wri_project_root, "data", "water", "int", "canadian-streamflow-data-full_2024.csv"))



# get canada stations with mean values from 1990–2009
monthly_data_w_station_info_filtered <- monthly_data_w_station_info %>%
  filter(sum_stat == "MEAN" & prov_terr_state_loc %in% c("QC", "NB", "PE", "NS", "ON", "NL", "MB", "AB", "SK", "NU", "NT", "YT", "BC"))

# get full data station numbers
# complete_1990_2009_stations <- monthly_data_1990_2009 %>%
#   distinct(station_number, year, month) %>%
#   count(station_number, name = "n_months") %>% # get num of unique year, month observations by station
#   filter(n_months == 240) %>%  # we want only stations with full data -- 12 * 20 = 240
#   pull(station_number)

complete_1991_2020_stations <- monthly_data_w_station_info_filtered %>%
  filter(year %in% 1991:2020) %>%
  count(station_number, year, month, name = "n_obs") %>%           # Count obs per month
  # filter(n_obs >= 1) %>% # this adds 17 sites that meet all reqs, but we don't know exactly why they have more than 1 obs in at least one month
  filter(n_obs == 1) %>%                                          # Keep only months with exactly 1 obs
  count(station_number, year, name = "n_valid_months") %>%            # Count valid months per year
  filter(n_valid_months == 12) %>%                                # Keep only full years
  count(station_number, name = "n_full_years") %>%                       # Count full years per site
  filter(n_full_years >= 20) %>%                                  # Sites with ≥ 20 good years
  pull(station_number)

# filter to only canada stations, mean stat, and years in our 30 year range (at least 1 obs/yr) and get those station numbers
# monthly_data_w_station_info_of_interest_30_yr <- monthly_data_w_station_info %>%
#   filter(year %in% 1991:2020 & sum_stat == "MEAN" & prov_terr_state_loc %in% c("QC", "NB", "PE", "NS", "ON", "NL", "MB", "AB", "SK", "NU", "NT", "YT", "BC") & station_number %in% complete_1990_2009_stations) %>%
#   group_by(station_number) %>%
#   summarize(n_years = n_distinct(year)) %>%
#   filter(n_years == 30) %>% # grab station only if have >=1 obs for each of all 30 years
#   ungroup() %>%
#   pull(station_number)

# filter to only canada stations, mean stat, and year for 2023 (recent year of interest) and get those station numbers
# monthly_data_w_station_info_of_interest_recent <- monthly_data_w_station_info %>%
#   filter(year %in% 2024 & sum_stat == "MEAN" & prov_terr_state_loc %in% c("QC", "NB", "PE", "NS", "ON", "NL", "MB", "AB", "SK", "NU", "NT", "YT", "BC") & station_number %in% complete_1990_2009_stations) %>%
#   pull(station_number)

monthly_data_w_station_info_of_interest_recent <- monthly_data_w_station_info_filtered %>%
  filter(year == 2024 & station_number %in% complete_1991_2020_stations) %>%
  count(station_number, month, name = "n_obs") %>%                    # Count obs per month
  filter(n_obs == 1) %>%                                          # Keep only those with exactly 1 obs
  count(station_number, name = "n_valid_months") %>%                     # Count valid months per site
  filter(n_valid_months == 12) %>%
  pull(station_number)

# which stations fulfill requirements for our 30 yr period and recent year?
# both_reqs_stations <- intersect(monthly_data_w_station_info_of_interest_recent, monthly_data_w_station_info_of_interest_30_yr) # 213 for BC and YK; 1041 for all Canada

# filter the monthly data for those stations
# monthly_data_w_station_info_of_interest_full <- monthly_data_w_station_info %>%
#   filter(station_number %in% both_reqs_stations) %>%
#   filter(year %in% c(1991:2020, 2024) & sum_stat == "MEAN" & prov_terr_state_loc %in% c("QC", "NB", "PE", "NS", "ON", "NL", "MB", "AB", "SK", "NU", "NT", "YT", "BC")) %>% # & year %in% 1991:2020
#   mutate(value_us_units = value * 35.3147) # convert (cubic meters per second) to match US units (cubic feet per second) for discharge

canada_stream_data_filtered <- monthly_data_w_station_info_filtered %>%
  filter(station_number %in% monthly_data_w_station_info_of_interest_recent) %>%
  filter(year %in% c(1991:2020, 2024)) %>%
  mutate(value_us_units = value * 35.3147) %>%
  group_by(station_number, year) %>%
  mutate(full_year = if_else(n_distinct(month) == 12, 1, 0)) %>%
  ungroup()


# write out data for use elsewhere
write_csv(canada_stream_data_filtered, file.path(wri_project_root, "data", "water", "int", "canadian-streamflow-data-30-yr-and-recent_2024.csv"))



# check cross-matching between the US sites and the stations in the US within the Canada dataset
# this code below supports our conclusion that we cannot be certain the canadian dataset US stations 
# are the same or not as what is included in the USGS NWIS stations, so we should exclude them from our analysis 
# to ensure we do not double count

# get US data of interest
hcdn_2009_designations_conus <- read_csv(file.path(wri_project_root, "data", "water-domain-data", "raw", "GAGES_II_Geospa", "basinchar_and_report_sept_2011", "spreadsheets-in-csv-format", "conterm_basinid.txt")) %>%
  filter(!is.na(`HCDN-2009`)) %>%
  select(STAID, LAT_GAGE, LNG_GAGE)

hcdn_2009_designations_ak <- read_csv(file.path(wri_project_root, "data", "water-domain-data", "raw", "GAGES_II_Geospa", "basinchar_and_report_sept_2011", "spreadsheets-in-csv-format", "AKHIPR_basinid.txt")) %>%
  filter(!is.na(`HCDN-2009`)) %>%
  select(STAID, LAT_GAGE, LNG_GAGE)

all_hcdn_2009_sites <- rbind(hcdn_2009_designations_conus, hcdn_2009_designations_ak)

nwis_data <- read_csv(file.path(wri_project_root, "data", "water-domain-data", "int", "study-area-usgs-nwis-data-with-coords-new-source-entire-us.csv"))
nwis_data_hcdn_2009 <- nwis_data %>%
  filter(site_no %in% c(all_hcdn_2009_sites$STAID))
length(unique(nwis_data_hcdn_2009$site_no)) # 717
nwis_data_hcdn_2009_for_compare <- nwis_data %>% # nwis_data_hcdn_2009
  filter(state_code %in% c("ME", "MN", "MI", "MT", "ND", "AK", "WA", "ID")) %>%
  select(site_no, latitude = dec_lat_va, longitude = dec_long_va) %>%
  distinct()

monthly_data_us <- monthly_data_w_station_info %>%
  filter(sum_stat == "MEAN" & prov_terr_state_loc %in% c("ME", "MN", "MI", "MT", "ND", "AK", "WA", "ID")) %>% # year %in% 1991:2020 & 
  select(station_number, latitude, longitude) %>%
  distinct()

# US data points
us_sf <- nwis_data_hcdn_2009_for_compare %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) # says in the data that it is NAD83

# canada US data points
can_us_sf <- monthly_data_us %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# check datum codes
datums <- hy_datum_list()
# seems like these are mostly/all vertical datums? so going to assume 4326 for horizontal

# within what distance do we assume a point is the same one in both datasets?
tolerance <- units::set_units(10000, "m") # had to mess around with this to get desired results; can also set to 1000 and see that 95% are within 1000

# transform to our crs for calcs
us_proj <- st_transform(us_sf, 5070)
can_us_proj <- st_transform(can_us_sf, 5070)

# check out distribution
plot(us_proj$geometry, cex = .1)
plot(can_us_proj$geometry, cex = .1, col = "red", add = TRUE)

# peform spatial join within tolerance to see matching station numbers
us_with_matches <- st_join(us_proj, can_us_proj, join = st_is_within_distance, dist = tolerance)
length(unique(us_with_matches$station_number))

# so all of the stations in the US that are within the Canadian streamwater database are within ~10,000 m of a station in the US streamwater database, with 95% being within ~1,000 m. i'm not sure if that's enough proof that USGS covers them? my instinct here is to just exclude any US stations in the Canadian database to ensure there is definitely no double counting. thoughts?
#   
# Cat agrees