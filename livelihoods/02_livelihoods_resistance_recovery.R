wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf) 
library(tidyverse) 
library(here)
library(ggplot2) 
library(dplyr)
library(readr)
library(purrr)
library(censusapi)
library(terra)

#### Overview #### 

# Resilience in the Economies Domain is computed through a combination of Resistance and Recovery metrics applied to Canadian and U.S. economic data, specifically focusing on industries 
# impacted by wildfires. Resistance is calculated by analyzing the proportion of jobs in specific industries prone to wildfire disruptions, normalized between 0 and 1 for both countries.
# Recovery is determined by calculating the Shannon diversity index of job types within each region, which measures economic diversity and potential for recovery, normalized within each country.
# The final resilience score is a composite measure combining resistance and recovery, indicating the area's ability to withstand and recover from economic disruptions caused by wildfires.

## Resistance Calculation
# Identifies key industries likely impacted by wildfires using NAICS codes and computes the proportion of total employment these industries represent within each census tract or subdivision.
# The data for Canadian regions is fetched from Statistics Canada, while U.S. data is retrieved using the Census API.

## Recovery Calculation
# Uses job distribution across various NAICS categories to compute the Shannon diversity index for each area, providing a measure of economic diversity and resilience potential.

# Download Canadian NAICS Codes: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810059201
# Data handling is done using a combination of tidyverse for data manipulation and sf package for spatial data integration.

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "livelihoods")
raw_data_file_path <- file.path(wri_project_root, "data", "livelihoodss", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "livelihoods", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "livelihoods")

#### Boundary Layers ####
study_area_admin2_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_2.shp")) %>% 
  st_transform(5070)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
human_settlement_layer <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

process_naics <- function(df) {
  
  # Calculate total workforce
  country_naics_wf <- df %>%
    group_by(geo_id) %>%
    summarise(wf = sum(count))
  
  # Calculate wildfire exposed workforce
  country_naics_wf_exp <- df %>%
    filter(naics %in% c(
      "4871", "4872", "4879", "5322", "5323", "5615",
      "7112", "7113", "7115", "7131", "7132", "7139",
      "7211", "7212", "7213", "7223", "7224", "7225"
    )) %>%
    group_by(geo_id) %>%
    summarise(wf_exp = sum(count))
  
  # Calculate proportions of workers affected by wildfires: resistance
  country_resistance_job_vulnerability <- country_naics_wf %>%
    left_join(country_naics_wf_exp, by = "geo_id") %>%
    mutate(wf_exp = replace_na(wf_exp, 0)) %>%
    mutate(prop_affected = wf_exp / wf) %>%
    mutate(resistance_job_vulnerability = 1 - (prop_affected - min(prop_affected)) / (max(prop_affected) - min(prop_affected)))
  
  # Calculate Shannon diversity index for NAICS data
  country_recovery_job_diversity <- df %>%
    group_by(geo_id) %>%
    mutate(wf = sum(count), prop = if_else(wf > 0, count / wf, 0)) %>%
    filter(prop > 0) %>%
    summarise(
      shannon_index = -sum(prop * log(prop)), 
      .groups = 'drop') %>%
    mutate(recovery_job_diversity = (shannon_index - min(shannon_index)) / (max(shannon_index) - min(shannon_index))) %>%
    dplyr::select(-shannon_index)
  
  country <- country_resistance_job_vulnerability %>%
    left_join(country_recovery_job_diversity, by = "geo_id")
  
  return(country)

}

#### Data Layers ####
canada_naics_raw <- read_csv(file.path(multi_domain_data_file_path, "canadian_naics_codes/98100592.csv"))
qcew <- read_csv(file.path(multi_domain_data_file_path, "BLS_QCEW/2023.annual.singlefile.csv")) 

# Define state FIPS codes for identifying correct states
stateFipsCodes <- c("02", "04", "06", "08", "16", "30", "32", "35", "41", "49", "53", "56")

#### Canadian NAICS Cleaning ####

# Clean and format data
canada_naics <- canada_naics_raw %>%
  filter(`Statistics (3)` == "Count",
         `Gender (3)` == "Total - Gender",
         `Age (15A)` == "Total - Age",
         `Labour force status (3)` == "Total - Labour force status") %>%
  filter(nchar(DGUID) == 13) %>%
  dplyr::select(geo_id = DGUID, naics = `Industry - Groups - North American Industry Classification System (NAICS) 2017 (428A)`, count = `Class of worker (7A):Total - Class of worker[1]`) %>%
  filter(grepl("^\\d{4} ", naics)) %>%
  filter(count > 0) %>% 
  mutate(naics = substr(naics, 1, 4))

#### US NAICS Cleaning #### 

us_naics <- qcew %>%
  as_tibble() %>%
  filter(
    nchar(industry_code) == 4,
    str_sub(area_fips, 1, 2) %in% stateFipsCodes # or alternatively filter by stco_fipsc in study_area_admin2_shape
  ) %>%
  mutate(geo_id = area_fips) %>%
  dplyr::select(geo_id, naics = industry_code, count = annual_avg_emplvl)


#### Indicator Calculation #### 

canada_df <- process_naics(canada_naics)
us_df <- process_naics(us_naics)

#### Attach geometry to datasets and bind Canada and US #### 

us_vulnerability_diversity <- study_area_admin2_shape_5070 %>%
  left_join(us_df, by = c("stco_fipsc" = "geo_id")) %>%
  filter(country %in% "United States") 

canada_vulnerability_diversity <- study_area_admin2_shape_5070 %>%
  left_join(canada_df, by = c("dguid" = "geo_id")) %>%
  filter(country %in% "Canada") 

# Combine the two indicators into one final dataset
livelihoods_vulnerability_diversity <- bind_rows(us_vulnerability_diversity, canada_vulnerability_diversity)

#### Data visualization #### 

# Plot job vulnerability
ggplot(livelihoods_vulnerability_diversity) +
  geom_sf(aes(fill = resistance_job_vulnerability), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: Resistance", fill = "Jobs Vulnerability") +
  theme_minimal()

# Plotting diversity of jobs
ggplot(livelihoods_vulnerability_diversity) +
  geom_sf(aes(fill = recovery_job_diversity), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Livelihoods: Recovery", fill = "Job Diversity") +
  theme_minimal()

#### Create Rasters ####

# Resistance: Job Vulnerability
livelihoods_job_vulnerability_rescaled_90m_5070 <- terra::rasterize(livelihoods_vulnerability_diversity, 
                                                             study_area_90m_5070, 
                                                             field = "resistance_job_vulnerability", 
                                                             fun = "mean")
# Align indicator with study_area_90m_template raster
livelihoods_job_vulnerability_rescaled_90m_5070 <- align_raster_to_template(study_area_90m_5070, livelihoods_job_vulnerability_rescaled_90m_5070, input_type = "continuous")

# Mask on status human settlement layer 
livelihoods_job_vulnerability_rescaled_90m_5070 <- mask(livelihoods_job_vulnerability_rescaled_90m_5070, human_settlement_layer)

writeRaster(livelihoods_job_vulnerability_rescaled_90m_5070, 
            file.path(final_layers_file_path, "indicators/livelihoods_resistance_job_vulnerability.tif"), 
            overwrite = TRUE)

# Recovery: Diversity of Jobs
livelihoods_diversity_of_jobs_rescaled_90m_5070 <- terra::rasterize(livelihoods_vulnerability_diversity, 
                                                             study_area_90m_5070, 
                                                             field = "recovery_job_diversity", 
                                                             fun = "mean")
# Align indicator with study_area_90m_template raster
livelihoods_diversity_of_jobs_rescaled_90m_5070 <- align_raster_to_template(study_area_90m_5070, livelihoods_diversity_of_jobs_rescaled_90m_5070, input_type = "continuous")

# Mask on status human settlement layer 
livelihoods_diversity_of_jobs_rescaled_90m_5070 <- mask(livelihoods_diversity_of_jobs_rescaled_90m_5070, human_settlement_layer)

writeRaster(livelihoods_diversity_of_jobs_rescaled_90m_5070, 
            file.path(final_layers_file_path, "indicators/livelihoods_recovery_diversity_of_jobs.tif"), 
            overwrite = TRUE)

plot(livelihoods_job_vulnerability_rescaled_90m_5070, main = "Livelihoods: Resistance: Job Vulnerability")
plot(livelihoods_diversity_of_jobs_rescaled_90m_5070, main = "Livelihoods: Recovery: Diversity of Jobs")
