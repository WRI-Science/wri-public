# script for occupational hazard (jobs) affected by poor Air Quality 
# NAICS codes for (11) Agriculture, Forestry, Fishing and Hunting and (23) Construction

# Identifies key industries likely impacted by wildfires using NAICS codes and computes the proportion of total employment these industries represent within each census tract or subdivision.
# The data for Canadian regions is fetched from Statistics Canada, while U.S. data is retrieved using the Census API.
# Download Canadian NAICS Codes: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810059201

# Load required packages
library(cancensus)
library(tidycensus)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(viridis)
library(purrr)
library(raster)
library(scales)  
library(terra)
library(here)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/air_quality"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/air_quality/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/air_quality"

#### Boundary layers ####
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_admin2_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_2.shp"))
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
canadian_NAICS_path <- read_csv(file.path(multi_domain_data_file_path, "canadian_naics_codes/98100592.csv"))
qcew <- read_csv(file.path(multi_domain_data_file_path, "BLS_QCEW/2023.annual.singlefile.csv")) # Bureau of Labor Statistics Quarterly Census of Employment & Wages
us_total_farm_h2a_workers <- st_read(file.path(intermediate_data_file_path, "ncfh/us_total_farm_h2a_workers.geojson")) %>% # from ncfh script
  dplyr::select(-NAME, -total_populationE, -total_populationM)

### Cansim package: (doesnt quite work) for Canadian NAICS occupational workers ####
# labor <- search_cansim_cubes("worker by industry")
# labor2 <- search_cansim_cubes("labour force")
# 
# data1 <- get_cansim("98-10-0592", refresh = TRUE) # current data in use, but for some reason cannot access this df through the package
# data2 <- get_cansim("14-10-0023") # more recent release (2025) but does not have YT

#### Get Canadian NAICS 11 & 23 occupational workers ####
canada_counties <- study_area_admin2_shape_5070 %>%
  dplyr::mutate(DGUID = dguid) %>%
  filter(country %in% "Canada") %>%
  dplyr::select(DGUID, county, country, cduid, province, pruid)

canada_naics <- canadian_NAICS_path %>%
  filter(`Statistics (3)` == "Count",
         `Gender (3)` == "Total - Gender",
         `Age (15A)` == "Total - Age",
         `Labour force status (3)` == "Total - Labour force status",
         nchar(DGUID) == 13,
         DGUID %in% canada_counties$DGUID) %>%
  dplyr::select(DGUID, 
                naics = `Industry - Groups - North American Industry Classification System (NAICS) 2017 (428A)`, 
                count = `Class of worker (7A):Total - Class of worker[1]`) %>%
  filter(grepl("^\\d{4} ", naics), count > 0) %>%
  mutate(naics_code = substr(naics, 1, 4),
         naics_description = trimws(substr(naics, 6, nchar(naics)))) %>%
  filter(substr(naics_code, 1, 2) %in% c("11", "23")) %>%
  dplyr::select(DGUID, naics_code, naics_description, count)


#### Cancensus: Get Canadian Population Census Division data ####

# follow instructions here to get key: https://mountainmath.github.io/cancensus/
#set_cancensus_cache_path("/home/farnisa/cache_cancensus", install = TRUE)
# 
# # some search stuff
# View(list_census_vectors("CA21")) # To view available Census variables for the 2021 Census
# find_census_vectors('population', dataset = 'CA21', type = 'total', query_type = 'keyword', interactive = T)
# list_census_vectors("CA21") %>%
#   filter(vector == "v_CA21_68" | vector == "v_CA21_1" | vector == "v_CA21_8")

# 59 and 60 are BC and YT regions
regions_of_interest <- c("59", "60")

# Total Working age population for 15 to 64 years
variables_of_interest <- c("v_CA21_68", "v_CA21_1")

# Get cancensus population data at the census division level as that is the level we have NAICS data at
census_data_canada_cd <- get_census(dataset='CA21', regions=list(PR = regions_of_interest), 
                                    vectors=variables_of_interest, level='CD', geo_format = "sf") 
census_data_canada_cd_5070 <- st_transform(census_data_canada_cd, 5070)

#### Canada working population ####

# Canada: Clean DGUID to match GeoUID
canada_naics_selected <- canada_naics %>%
  mutate(GeoUID = gsub("^2021A0003", "", DGUID))

# Join NAICS employment to census population data by GeoUID
can_selected_naics_total_pop <- left_join(
  census_data_canada_cd_5070,
  canada_naics_selected,
  by = "GeoUID"
)

# Summarize employment and calculate percent employed
can_naics_grouped <- can_selected_naics_total_pop %>%
  st_drop_geometry() %>%
  group_by(GeoUID, name, PR_UID, `Region Name`) %>%
  summarise(
    total_naics_count = sum(count, na.rm = TRUE),
    working_age_population = first(`v_CA21_68: 15 to 64 years`)
  ) %>%
  mutate(
    percent_naics_tot_pop = (total_naics_count / working_age_population) * 100
  )

#### Get BLS Quarterly Census Employment & Wages - US NAICS 11 & 23 data ####

# filter for only study area fips code and naics 4 digit codes that start with 11 and 23
# annual_avg_emplvl = Annual average of monthly employment levels for a given year
# 7588
qcew_naics_filtered <- qcew %>%
  mutate(industry_code = as.character(industry_code)) %>%
  filter(
    (str_starts(industry_code, "11") | str_starts(industry_code, "23")) &
      str_length(industry_code) == 4,
    area_fips %in% study_area_admin2_shape_5070$stco_fipsc
  ) %>%
  rename(GEOID = area_fips) %>%
  dplyr::select(
    GEOID, own_code, industry_code, agglvl_code, size_code, year, qtr, disclosure_code, annual_avg_estabs, annual_avg_emplvl
  )

#### Get county working populations from ACS survey API ####

# Read in the wwri_study_area_admin2.shp and filter for only US
us_counties <- study_area_admin2_shape_5070 %>%
  filter(country %in% "United States") %>%
  dplyr::select(county, county_fip, state_name, state_fips, country, GEOID = stco_fipsc)

# Define state FIPS codes for fetching NAICS data
stateFipsCodes <- unique(us_counties$state_fips)

variables <- c(
  working_age_population = "B23006_001" # Civilian working-age population (16 to 64 years)
)

# Function to fetch data for a single state including working-age variables
fetch_data_for_county_working_age <- function(state) {
  get_acs(
    geography = "county",
    variables = variables,
    year = 2023,
    survey = "acs5",
    output = "wide",
    state = state
  )
}

# Fetch data for each state and combine results
census_data_list <- purrr::map(stateFipsCodes, fetch_data_for_county_working_age)
census_data <- bind_rows(census_data_list)

# Merge NAICS county employed workers data to county total working population 
us_selected_naics_total_pop <- left_join(census_data, qcew_naics_filtered)

# Group by GEOID, sum the annual_avg_emplvl for each GEOID, and keep the remaining columns and then calculate the % of employed to working_total_population
# 444 rows
us_selected_naics_total_pop_grouped <- us_selected_naics_total_pop %>%
  group_by(GEOID) %>%
  summarise(
    NAME = paste(unique(NAME), collapse = " | "),
    working_age_total_populationE = first(working_age_populationE),
    naics_total = sum(annual_avg_emplvl, na.rm = TRUE), # Sum the EMP counts for each GEOID
    prcnt_employed_not_farm = (naics_total / working_age_total_populationE) * 100
  )

#### Add farm workers and h2a worker count to US NAICS EMP count because it is not captured in the NAICS codes ####

# remove the NAME, total_populationE and total_populationM columns and the dependent columns 
us_total_farm_h2a_workers <- us_total_farm_h2a_workers %>% 
  dplyr::select(-Total.Dependents, -Crop.Production.Dependents, -Animal.Production.Dependents) %>% 
  rename(sum_total_ag_workers_and_h2a = sum_total_workers_and_h2a)

# Join the farm+h2a workers df and the US naics df
# Summarize NAICS (11+23) and NCFH agricultural workers
us_naics_grouped_sf <- 
  full_join(us_selected_naics_total_pop_grouped, us_total_farm_h2a_workers, by = "GEOID") %>%
  group_by(GEOID) %>%
  mutate(
    naics_clean = replace_na(naics_total, 0),
    ag_clean = replace_na(sum_total_ag_workers_and_h2a, 0),
    total = naics_clean + ag_clean,
    percent_naics_tot_pop = (total / working_age_total_populationE) * 100
  ) %>%
  dplyr::select(-geometry, -naics_clean, -ag_clean) %>%
  ungroup()


#### Attach US and CAN occupational worker % to geometry ####

# Filter the study area for US and Canada separately
study_area_us <- study_area_admin2_shape_5070 %>% filter(country == "United States")
study_area_can <- study_area_admin2_shape_5070 %>% filter(country == "Canada")

# Join US NAICS data to US shapefile
us_naics_selected_sf <- study_area_us %>%
  left_join(us_naics_grouped_sf, by = c("stco_fipsc" = "GEOID")) %>%
  mutate(country = "United States")

# Join Canada NAICS data to Canadian shapefile
canada_naics_selected_sf <- study_area_can %>%
  left_join(can_naics_grouped, by = c("cduid" = "GeoUID")) %>%
  mutate(country = "Canada")

# Combine the two back into one final dataset
wwri_vulnerable_workers <- bind_rows(us_naics_selected_sf, canada_naics_selected_sf) %>% 
  st_transform(5070)

#### Data Visualization ####

ggplot(data = wwri_vulnerable_workers) +
  geom_sf(aes(fill = percent_naics_tot_pop)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Vulnerable Workers",
       #subtitle = "2023 Vulnerable Workers in NAICS 11 & 23 (plus US codes 111 & 112 from NCFH)",
       fill = "% vulnerable of working population",
       caption = "BLS QCEW: 2023; US Census Bureau ACS 2022; Can Census: 2021"
  ) +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

#### Summary Stats ####
wwri_vulnerable_workers_stats <- wwri_vulnerable_workers %>%
  summarise(
    n = n(),
    min = min(percent_naics_tot_pop, na.rm = TRUE),
    q1 = quantile(percent_naics_tot_pop, 0.25, na.rm = TRUE),
    median = median(percent_naics_tot_pop, na.rm = TRUE),
    mean = mean(percent_naics_tot_pop, na.rm = TRUE),
    q3 = quantile(percent_naics_tot_pop, 0.75, na.rm = TRUE),
    max = max(percent_naics_tot_pop, na.rm = TRUE),
    sd = sd(percent_naics_tot_pop, na.rm = TRUE),
    sd2 = 2 * sd(percent_naics_tot_pop, na.rm = TRUE),
    sd3 = 3 * sd(percent_naics_tot_pop, na.rm = TRUE),
    p1 = quantile(percent_naics_tot_pop, 0.01, na.rm = TRUE),
    p5 = quantile(percent_naics_tot_pop, 0.05, na.rm = TRUE),
    p95 = quantile(percent_naics_tot_pop, 0.95, na.rm = TRUE),
    p99 = quantile(percent_naics_tot_pop, 0.99, na.rm = TRUE)
  )
print(wwri_vulnerable_workers_stats)


#### Rescale values between 0-1 ####

# Inverse rescale the values and cap the upper threshold at p99 = 87.25722
naics_vulnerable_workers_rescaled <- wwri_vulnerable_workers %>%
  mutate(
    percent_naics_tot_pop_capped = pmin(87.25722, pmax(0, percent_naics_tot_pop)),    # Cap to 0–87.25722
    percent_naics_tot_pop_rescaled = (87.25722 - percent_naics_tot_pop_capped) / 87.25722  # Rescale: 0 → 1, 87.25722 → 0
  )

# plot rescaled values 
ggplot(data = naics_vulnerable_workers_rescaled) +
  geom_sf(aes(fill = percent_naics_tot_pop_rescaled)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis", na.value = "grey", limits = c(0, NA), breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "% Vulnerable Workers of Working Population",
       fill = "Resistance",
       #caption = "Source: U.S. Census Bureau ACS 2021"
  ) +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))


#### Create Raster ####

# Rasterize the vector data onto the study area raster
vulnerable_workers_rescaled_90m_5070 <- terra::rasterize(naics_vulnerable_workers_rescaled, 
                                                         study_area_90m_5070, 
                                                         field = "percent_naics_tot_pop_rescaled", 
                                                         fun = "mean")

# Align indicator with study_area_90m_template raster
air_quality_resistance_vulnerable_workers <- align_raster_to_template(study_area_90m_5070, vulnerable_workers_rescaled_90m_5070)

# Plot the final raster
plot(air_quality_resistance_vulnerable_workers, main = "Resistance: Vulnerable Workers")

# Save to aurora
writeRaster(air_quality_resistance_vulnerable_workers, 
            filename = file.path(final_layers_file_path, "indicators/air_quality_resistance_vulnerable_workers.tif"),
            overwrite = TRUE)

plot(rast("/home/shares/wwri-wildfire/final_layers/2024/air_quality/indicators/air_quality_resistance_vulnerable_workers.tif"))

#### Data Visualization with CRS 5070 for newsletter ####

# Transforming the CRS of the sf dataframe to EPSG:5070
wwri_naics_selected_sf_5070 <- sf::st_transform(wwri_naics_selected_sf, crs = 5070)

# Cap values at 50 for visualization purposes
wwri_naics_selected_sf_5070$percent_naics_tot_pop_rescaled <- pmin(wwri_naics_selected_sf_5070$percent_naics_tot_pop, 50)

# Confirm the rescaling
summary(wwri_naics_selected_sf_5070$percent_naics_tot_pop_rescaled)

vuln_workers_5070 <- ggplot(data = wwri_naics_selected_sf_5070) +
  geom_sf(aes(fill = percent_naics_tot_pop_rescaled), alpha = 0.7, color = "grey", linewidth = 0.1) +  # Add thinner grey outlines
  theme_void() +
  scale_fill_viridis_c(
    option = "magma", 
    limits = c(0, 50),  # Set the limits explicitly to match the top threshold
    breaks = c(0, 10, 20, 30, 40, 50),  # Define the break points
    labels = c("0", "10", "20", "30", "40", "≥ 50")  # Add the "≥" sign for the top value
  ) +
  labs(title = "Vulnerable Workers (NAICS Codes 11 and 23)",
       fill = "% of total working population") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12)   # Increase legend text size
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10))

vuln_workers_5070 
