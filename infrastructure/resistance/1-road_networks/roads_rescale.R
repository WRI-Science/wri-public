wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggridges)
library(tidyr)
library(terra)
library(sf)
library(here)

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "infrastructure")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2023", "infrastructure")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>% 
  st_transform(5070)
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))
states <- c("Alaska", "Washington", "Oregon", "California", "Idaho", "Nevada", "Utah", "Arizona", "Montana", "Wyoming", "Colorado", "New Mexico")

#### Data Layers ####

# US and Canada road network statistics
us_network_statistics <- read.csv(file.path(wri_project_root, "papers", "networks_paper", "output_csvs", "combined_csv", "combined_data.csv")) %>%
  rename(
    state = state_x) %>%
  filter(state %in% states) %>% 
  dplyr::select(
    state, STATEFP, PLACEFP, PLACENS, GEOID, GEOIDFQ, NAME, NAMELSAD, LSAD, CLASSFP,
    PCICBSA, MTFCC, FUNCSTAT, Name_1, Total.Popu,
    boundary_crossing_edges_motorway, boundary_crossing_lanes_motorway,
    boundary_crossing_edges_trunk, boundary_crossing_lanes_trunk,
    boundary_crossing_edges_primary, boundary_crossing_lanes_primary,
    boundary_crossing_edges_secondary, boundary_crossing_lanes_secondary,
    boundary_crossing_edges_tertiary, boundary_crossing_lanes_tertiary
  ) %>% 
  mutate(GEOID = as.character(GEOID))

# 329
canada_network_statistics <- read.csv(file.path(wri_project_root, "data", "infrastructure", "intermediate", "road_network", "2023", "canada_network_statistics.csv")) %>%
  mutate(DPLUID = as.character(DPLUID), PRUID = as.character(PRUID)) %>%
  dplyr::select(
    DPLUID, DGUID, DPLNAME, DPLTYPE, LANDAREA, PRUID, boundary_crossing_edges_motorway,
    boundary_crossing_lanes_motorway, boundary_crossing_edges_trunk,
    boundary_crossing_lanes_trunk, boundary_crossing_edges_primary,
    boundary_crossing_lanes_primary, boundary_crossing_edges_secondary,
    boundary_crossing_lanes_secondary, boundary_crossing_edges_tertiary,
    boundary_crossing_lanes_tertiary)

# US CDP's in study area 
us_cdp_path <- file.path(multi_domain_data_file_path, "raw/boundary_layers/us_census_designated_places/2024") 

# CAN CDP with <50k population
#332
can_cdp_equivalent <- st_read(file.path(data_file_path, "intermediate/road_network/2023/canada_designated_places_equivalent/canada-cdp-equivelant.shp")) %>% 
  st_transform(5070) %>% 
  st_make_valid() 

#### code to identify CAN population areas with >50k population ####

can_sbd_shapefile <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "canada_census_subdivisions", "canada_census_subdivisions.shp")) %>% 
  st_transform(5070)
shapefile_path_dpls = st_read(file.path(wri_project_root, "data", "multi_domain_data", "raw", "boundary_layers", "canada_designated_places", "ldpl000b21a_e.shp")) %>% 
  st_transform(5070) %>% 
  filter(PRUID %in% c(59, 60))
shapefile_path_pcenters = st_read(file.path(wri_project_root, "data", "multi_domain_data", "raw", "boundary_layers", "canada_population_centers", "lpc_000b21a_e.shp")) %>% 
  st_transform(5070) %>% 
  filter(PRUID %in% c(59, 60))

# bind rows and then filter for those not 
shapefile_pcenters_dpls <- bind_rows(shapefile_path_pcenters, shapefile_path_dpls)

# CSV file paths with population
csv_path_population_centers = read_csv(file.path(wri_project_root, "data", "infrastructure", "canada-population-centers-population", "98100011.csv"))
# Canada subdivision population data
can_subdivision_population <- read_csv(file.path(wri_project_root, "data", "infrastructure", "canada-subdivision-population", "98100004.csv"))

# add population to subdivisions and filter for places >50k
# 20 
pop_col <- "Population and dwelling counts (7): Population, 2021 [1]"
subdivision_population <- left_join(can_sbd_shapefile, can_subdivision_population) %>%
  filter(.data[[pop_col]] > 50000)

# Anti-join to identify areas that are not in the CDP < 50k
not_in_cbd_equivalent <- anti_join(shapefile_pcenters_dpls, st_drop_geometry(can_cdp_equivalent), by = "DGUID")

# add population to the canada designated places and population centers to identify places >50k population
# 10 
pop_col <- "Population and dwelling counts (7): Population, 2021 [1]"
not_in_cbd_equivalent_pop <- inner_join(not_in_cbd_equivalent, csv_path_population_centers) %>%
  filter(.data[[pop_col]] > 50000)

# Now bind subdivision population with census and DPL population rows
# 30
can_population_places_over_50k <- bind_rows(not_in_cbd_equivalent_pop, subdivision_population)

# add a new column called exits_divided_by_2_rescaled and apply a value of 1
can_population_places_over_50k <- can_population_places_over_50k %>% 
  mutate(exits_divided_by_2_rescaled = 1)


#### Count US and Canada exits for population places under 50k and divide by 2 #### 

#count the number of access points (called exits)
us_network_statistics$exits <- with(us_network_statistics, 
                    rowSums(cbind(boundary_crossing_edges_motorway, boundary_crossing_edges_trunk,
                                  boundary_crossing_edges_primary, boundary_crossing_edges_secondary,
                                  boundary_crossing_edges_tertiary), na.rm = TRUE)
)

#count the number of lanes 
us_network_statistics$lanes <- with(us_network_statistics, 
                    rowSums(cbind(boundary_crossing_lanes_motorway, boundary_crossing_lanes_trunk,
                                  boundary_crossing_lanes_primary, boundary_crossing_lanes_secondary,
                                  boundary_crossing_lanes_tertiary), na.rm = TRUE)
)

#divide the number of roads by 2 to account for 2-way traffic
us_network_statistics$exits_divided_by_2 <- us_network_statistics$exits/2 #divide by 2 because two way roads come in 2s

canada_network_statistics$exits <- with(canada_network_statistics, 
                                        rowSums(cbind(boundary_crossing_edges_motorway, boundary_crossing_edges_trunk,
                                                      boundary_crossing_edges_primary, boundary_crossing_edges_secondary,
                                                      boundary_crossing_edges_tertiary), na.rm = TRUE)
)

canada_network_statistics$lanes <- with(canada_network_statistics, 
                                        rowSums(cbind(boundary_crossing_lanes_motorway, boundary_crossing_lanes_trunk,
                                                      boundary_crossing_lanes_primary, boundary_crossing_lanes_secondary,
                                                      boundary_crossing_lanes_tertiary), na.rm = TRUE)
)

# Divide exits by 2 to account for two-way roads
canada_network_statistics$exits_divided_by_2 <- canada_network_statistics$exits/2

#### Identify US CDP's > 50,000 people and assign an exits_divided_by_2_rescaled value of 1 ####

# List all state subfolders
state_folders <- list.dirs(us_cdp_path, recursive = FALSE)

# Initialize an empty list to store shapefiles
cdp_list <- list()

# Loop through each folder and read the shapefile
for (folder in state_folders) {
  # List all shapefiles in the folder
  shp_files <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  
  # Only proceed if a shapefile is found
  if (length(shp_files) > 0) {
    # If there are multiple shapefiles, you can adjust logic here
    shp <- shp_files[1]  # Just read the first one for now
    # Read and store, using folder name as list key
    state_name <- basename(folder)
    cdp_list[[state_name]] <- st_read(shp, quiet = TRUE)
  }
}

# Combine all into one big sf object
us_cdp <- bind_rows(cdp_list, .id = "state")

# transform to 5070 and make valid
us_cdp_5070 <- us_cdp %>% 
  st_transform(5070) %>% 
  st_make_valid() 

# remove the leading zero from GEOID and make character
us_cdp_5070 <- us_cdp_5070 %>%
  mutate(GEOID = str_remove(as.character(GEOID), "^0+"))

# identify the GEOID's that are different between us_cdp_5070 (geometry + all US cdp) and us_network_statistics (only CDP <50k population) to get the CDP's with greater than 50k
cdp_greater_than_50k <- anti_join(us_cdp_5070, us_network_statistics, by = "GEOID")

# add a new column called exits_divided_by_2_rescaled and apply a value of 1
cdp_greater_than_50k <- cdp_greater_than_50k %>% 
  mutate(exits_divided_by_2_rescaled = 1)


#### Combine US and CAN network statistics and RESCALE road exits_divided_by_2 for CDP less than 50k ####

# bind US and CAN network statistics
wwri_network_stats <- bind_rows(us_network_statistics, canada_network_statistics)

# RESCALE: 0 exits_divided_by_2 = 0 and 33 exits_divided_by_2 = 1 with linear scaling in between
wwri_network_stats <- wwri_network_stats %>%
  mutate(
    exits_divided_by_2_rescaled = pmin(1, (exits_divided_by_2 - 0) / (33 - 0))
  )

ggplot(wwri_network_stats, aes(x = exits_divided_by_2_rescaled)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "US+CAN Distribution of exits_divided_by_2 rescaled at 33",
       x = "Number of exits_divided_by_2",
       y = "Count")

# Join rescaled spatial data to the wwri network statistics and also cdp_greater_than_50k
wwri_network_stats_spatial_canada <- inner_join(can_cdp_equivalent, wwri_network_stats)
wwri_network_stats_spatial_us <- inner_join(us_cdp_5070, wwri_network_stats, by = "GEOID")

wwri_network_stats_spatial_canada <- wwri_network_stats_spatial_canada %>%
  mutate(STATEFP = as.character(STATEFP), 
         PLACEFP = as.character(PLACEFP), 
         PLACENS = as.character(PLACENS))

cdp_greater_than_50k <- cdp_greater_than_50k %>%
  mutate(STATEFP = as.character(STATEFP),
         PLACEFP = as.character(PLACEFP),
         PLACENS = as.character(PLACENS))

wwri_network_stats_spatial <- bind_rows(
  wwri_network_stats_spatial_canada,
  wwri_network_stats_spatial_us,
  cdp_greater_than_50k, 
  can_population_places_over_50k
)

#### Identify US and Canada non-CDP rural areas and rescale to 0 ####

# Combine all CDP polygons into a single (multi)polygon
wwri_rescaled_roads_union <- st_union(wwri_network_stats_spatial)

# Calculate difference: study area minus all CDPs
wwri_study_area_gap <- st_difference(study_area_admin0_shape_5070, wwri_rescaled_roads_union)

wwri_study_area_gap <- wwri_study_area_gap %>%
  mutate(exits_divided_by_2_rescaled = 0)

wwri_rescaled_roads <- bind_rows(wwri_network_stats_spatial, wwri_study_area_gap)

ggplot() +
  geom_sf(data = wwri_rescaled_roads, aes(fill = exits_divided_by_2_rescaled), color = NA) +
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "viridis", name = "Score", na.value = "grey90") +
  labs(title = "CDP exit roads rescaled and no CDP") +
  theme_minimal() 


#### Rasterize Egress Indicator ####

# Rasterize indicator or score
infrastructure_resistance_egress <- terra::rasterize(
  wwri_rescaled_roads,
  study_area_90m_5070,
  field = "exits_divided_by_2_rescaled", 
  fun = "mean")

plot(infrastructure_resistance_egress)

# mask on status human settlement layer 
infrastructure_resistance_egress_masked <- mask(infrastructure_resistance_egress, human_settlement_layer)

# Align both indicators with study_area_90m_template raster
infrastructure_resistance_egress <- align_raster_to_template(study_area_90m_5070, infrastructure_resistance_egress, input_type = "continuous")
infrastructure_resistance_egress_masked <- align_raster_to_template(study_area_90m_5070, infrastructure_resistance_egress_masked, input_type = "continuous")

# Save to aurora
writeRaster(infrastructure_resistance_egress, 
            filename = file.path(final_layers_file_path, "indicators/infrastructure_resistance_egress.tif"),
            overwrite = TRUE)
writeRaster(infrastructure_resistance_egress_masked, 
            filename = file.path(final_layers_file_path, "indicators/infrastructure_resistance_egress_masked.tif"),
            overwrite = TRUE)

plot(infrastructure_resistance_egress, main = "Egress")
plot(infrastructure_resistance_egress_masked, main = "Egress masked")


#### US Network Stats ####

us_summary_stats_exits_divided_by_2 <- us_network_statistics %>%
  summarise(
    n = n(),
    min = min(exits_divided_by_2, na.rm = TRUE),
    q1 = quantile(exits_divided_by_2, 0.25, na.rm = TRUE),
    median = median(exits_divided_by_2, na.rm = TRUE),
    mean = mean(exits_divided_by_2, na.rm = TRUE),
    q3 = quantile(exits_divided_by_2, 0.75, na.rm = TRUE),
    max = max(exits_divided_by_2, na.rm = TRUE),
    sd = sd(exits_divided_by_2, na.rm = TRUE),
    sd2 = 2 * sd(exits_divided_by_2, na.rm = TRUE),
    sd3 = 3 * sd(exits_divided_by_2, na.rm = TRUE),
    p1 = quantile(exits_divided_by_2, 0.01, na.rm = TRUE),
    p5 = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p95 = quantile(exits_divided_by_2, 0.95, na.rm = TRUE),
    p99 = quantile(exits_divided_by_2, 0.99, na.rm = TRUE)
  )
print(us_summary_stats_exits_divided_by_2)

ggplot(us_network_statistics, aes(x = exits_divided_by_2)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of exits_divided_by_2 (Adjusted for Two-way Traffic)",
       x = "Number of exits_divided_by_2",
       y = "Count")

summary_stats_exits_divided_by_2_by_state <- us_network_statistics %>%
  group_by(state) %>%
  summarise(
    n = n(),
    min = min(exits_divided_by_2, na.rm = TRUE),
    q1 = quantile(exits_divided_by_2, 0.25, na.rm = TRUE),
    median = median(exits_divided_by_2, na.rm = TRUE),
    mean = mean(exits_divided_by_2, na.rm = TRUE),
    q3 = quantile(exits_divided_by_2, 0.75, na.rm = TRUE),
    max = max(exits_divided_by_2, na.rm = TRUE),
    sd = sd(exits_divided_by_2, na.rm = TRUE),
    sd2 = 2 * sd(exits_divided_by_2, na.rm = TRUE),
    sd3 = 3 * sd(exits_divided_by_2, na.rm = TRUE),
    p1 = quantile(exits_divided_by_2, 0.01, na.rm = TRUE),
    p5 = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p95 = quantile(exits_divided_by_2, 0.95, na.rm = TRUE),
    p99 = quantile(exits_divided_by_2, 0.99, na.rm = TRUE)
  ) %>%
  arrange(desc(mean))
print(summary_stats_exits_divided_by_2_by_state)

ggplot(us_network_statistics, aes(x = exits_divided_by_2)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black", alpha = 0.7) +
  facet_wrap(~ state, scales = "free_y") +
  theme_minimal() +
  labs(title = "exits_divided_by_2/2 Distribution by State", x = "Number of exits_divided_by_2", y = "Count")

ggplot(us_network_statistics, aes(x = exits_divided_by_2)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black", alpha = 0.7) +
  facet_wrap(~ state, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution of Unique exits_divided_by_2 by State (exits_divided_by_2/2)",
    x = "Number of Unique exits_divided_by_2",
    y = "Count"
  )

percentiles_by_geoid <- us_network_statistics %>%
  group_by(GEOID) %>%
  summarise(
    p5  = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p10 = quantile(exits_divided_by_2, 0.10, na.rm = TRUE),
    p15 = quantile(exits_divided_by_2, 0.15, na.rm = TRUE)  # This is the median
  )

percentiles_long <- percentiles_by_geoid %>%
  pivot_longer(cols = c(p5, p10, p15), 
               names_to = "percentile", 
               values_to = "value")

ggplot(percentiles_long, aes(x = value, fill = percentile)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~percentile, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Distribution of exits_divided_by_2 Percentiles per GEOID",
    x = "exits_divided_by_2",
    y = "Number of GEOIDs"
  )


#### CAN Network Stats ####

# Check the number of zeroes
zero_roads_canada <- subset(canada_network_statistics, exits_divided_by_2 == 0) # 54

can_summary_stats_exits_divided_by_2_canada <- canada_network_statistics %>%
  summarise(
    n = n(),
    min = min(exits_divided_by_2, na.rm = TRUE),
    q1 = quantile(exits_divided_by_2, 0.25, na.rm = TRUE),
    median = median(exits_divided_by_2, na.rm = TRUE),
    mean = mean(exits_divided_by_2, na.rm = TRUE),
    q3 = quantile(exits_divided_by_2, 0.75, na.rm = TRUE),
    max = max(exits_divided_by_2, na.rm = TRUE),
    sd = sd(exits_divided_by_2, na.rm = TRUE),
    sd2 = 2 * sd(exits_divided_by_2, na.rm = TRUE),
    sd3 = 3 * sd(exits_divided_by_2, na.rm = TRUE),
    p1 = quantile(exits_divided_by_2, 0.01, na.rm = TRUE),
    p5 = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p95 = quantile(exits_divided_by_2, 0.95, na.rm = TRUE),
    p99 = quantile(exits_divided_by_2, 0.99, na.rm = TRUE)
  )
print(can_summary_stats_exits_divided_by_2_canada)

ggplot(canada_network_statistics, aes(x = exits_divided_by_2)) +
  geom_histogram(bins = 30, fill = "forestgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of exits_divided_by_2 (Canada, Adjusted for Two-way Traffic)",
       x = "Number of exits_divided_by_2",
       y = "Count")

summary_stats_exits_divided_by_2_by_province <- canada_network_statistics %>%
  group_by(PRUID) %>%
  summarise(
    n = n(),
    min = min(exits_divided_by_2, na.rm = TRUE),
    q1 = quantile(exits_divided_by_2, 0.25, na.rm = TRUE),
    median = median(exits_divided_by_2, na.rm = TRUE),
    mean = mean(exits_divided_by_2, na.rm = TRUE),
    q3 = quantile(exits_divided_by_2, 0.75, na.rm = TRUE),
    max = max(exits_divided_by_2, na.rm = TRUE),
    sd = sd(exits_divided_by_2, na.rm = TRUE),
    sd2 = 2 * sd(exits_divided_by_2, na.rm = TRUE),
    sd3 = 3 * sd(exits_divided_by_2, na.rm = TRUE),
    p1 = quantile(exits_divided_by_2, 0.01, na.rm = TRUE),
    p5 = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p95 = quantile(exits_divided_by_2, 0.95, na.rm = TRUE),
    p99 = quantile(exits_divided_by_2, 0.99, na.rm = TRUE)
  ) %>%
  arrange(desc(mean))

print(summary_stats_exits_divided_by_2_by_province)

# rename PRUID to state and change 59 to British Columbia and 60 to Yukon
summary_stats_exits_divided_by_2_by_province <- summary_stats_exits_divided_by_2_by_province %>%
  rename(state = PRUID) %>%
  mutate(state = recode(state,
                        `59` = "British Columbia",
                        `60` = "Yukon"))

ggplot(canada_network_statistics, aes(x = exits_divided_by_2)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black", alpha = 0.7) +
  facet_wrap(~ PRUID, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "exits_divided_by_2/2 Distribution by Province (Canada)",
    x = "Number of exits_divided_by_2",
    y = "Count"
  )

percentiles_by_dguid_canada <- canada_network_statistics %>%
  group_by(DGUID) %>%
  summarise(
    p5  = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p10 = quantile(exits_divided_by_2, 0.10, na.rm = TRUE),
    p50 = quantile(exits_divided_by_2, 0.50, na.rm = TRUE)  # Median
  )

percentiles_long_canada <- percentiles_by_dguid_canada %>%
  pivot_longer(cols = c(p5, p10, p50), 
               names_to = "percentile", 
               values_to = "value")

ggplot(percentiles_long_canada, aes(x = value, fill = percentile)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~percentile, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Distribution of exits_divided_by_2 Percentiles per DGUID (Canada)",
    x = "exits_divided_by_2",
    y = "Number of DGUIDs"
  )


#### Combined US CAN summary stats ####

all_summary_stats <- bind_rows(us_network_statistics, canada_network_statistics) %>% 
  summarise(
    n = n(),
    min = min(exits_divided_by_2, na.rm = TRUE),
    q1 = quantile(exits_divided_by_2, 0.25, na.rm = TRUE),
    median = median(exits_divided_by_2, na.rm = TRUE),
    mean = mean(exits_divided_by_2, na.rm = TRUE),
    q3 = quantile(exits_divided_by_2, 0.75, na.rm = TRUE),
    max = max(exits_divided_by_2, na.rm = TRUE),
    sd = sd(exits_divided_by_2, na.rm = TRUE),
    sd2 = 2 * sd(exits_divided_by_2, na.rm = TRUE),
    sd3 = 3 * sd(exits_divided_by_2, na.rm = TRUE),
    p1 = quantile(exits_divided_by_2, 0.01, na.rm = TRUE),
    p5 = quantile(exits_divided_by_2, 0.05, na.rm = TRUE),
    p50 = quantile(exits_divided_by_2, 0.50, na.rm = TRUE),
    p95 = quantile(exits_divided_by_2, 0.95, na.rm = TRUE),
    p99 = quantile(exits_divided_by_2, 0.99, na.rm = TRUE)
  )
print(all_summary_stats)



all_summary_stats_by_state_province_country <- bind_rows(
  summary_stats_exits_divided_by_2_by_state %>% mutate(country = "US"),
  us_summary_stats_exits_divided_by_2 %>% mutate(country = "US", state = "US"), 
  summary_stats_exits_divided_by_2_by_province %>% mutate(country = "CAN"), 
  can_summary_stats_exits_divided_by_2_canada %>% mutate(country = "CAN", state = "CAN"),
  all_summary_stats %>% mutate(country = "all", state = "all"))

print(all_summary_stats_by_state_province_country)

# Save the summary statistics to a CSV file
write.csv(all_summary_stats_by_state_province, 
          file.path(wri_project_root, "data", "infrastructure", "intermediate", "resistance", "road-network", "all_summary_stats_by_state_province.csv"), 
          row.names = FALSE)

