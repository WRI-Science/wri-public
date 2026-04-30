wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(readxl) # For reading Excel files cleanly
library(tidyverse) # For data manipulation
library(sf) # For spatial data operations
library(tidycensus) # For accessing US census data
library(terra) # For raster operations
library(here) # To assemble file paths within project


#### Script Overview ####
# This script reads in water treatment data from the EPA, cleans it, and processes it to create a water treatment score for each US county in the study area. The scores are based on the rescaled number of sources (community water systems) and rescaled total violations reported multiplied together. The resulting data is then rasterized to a 90 m resolution, aligned to a template raster, and written out.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "water") # Base directory for water
final_layers_file_path <- file.path(wri_project_root, "final_layers")
multi_domain_data_path <- file.path(wri_project_root, "data", "multi_domain_data") # Base directory for multi-domain data
path_year <- "2024" # Update this if needed for the year of the data
raw_data_file_path <- file.path(data_file_path, "raw", path_year) # Directory for raw data
final_layers_output_path <- file.path(final_layers_file_path, path_year, "water") # output path for final layers


#### Boundary Layers ####
# Read in official county names/shapes
study_area_admin_2 <- st_read(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_2.shp"))

# Read in study area raster to rasterize to
study_area_rast <- rast(file.path(multi_domain_data_path, "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))


#### Functions ####
# For aligning fucntion
source(here("templates_and_functions", "align_raster_to_template.R"))

# Function to get year, quarter, and letter from file paths to create 2 new columns with time info
extract_year_quarter <- function(filepath) {
  
  # Get the parts of the file path needed for extraction
  file_parts <- str_split(filepath, "/", simplify = TRUE)
  year_letter <- file_parts[length(file_parts) - 2]
  
  # Extract year
  year <- str_extract(year_letter, "\\d{4}")
  #letter <- str_extract(year_letter, "[A-Z]$")
  
  # Extract quarter
  quarter <- str_replace(file_parts[length(file_parts) - 1], "q", "")
  
  # Return a data frame with the extracted information
  data.frame(year = as.integer(year), quarter = as.integer(quarter))
  
}


#### Data Layers ####
# List all files in epa folder
files <- list.files(file.path(raw_data_file_path, "epa"), pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)

# Read in and bind raw water treatment data files (for Active status only)
water_treatment_data <- bind_rows(lapply(files, function(file) {
  meta <- extract_year_quarter(file)
  df <- read_excel(file, skip = 4) %>% 
    mutate(year = meta$year, quarter = meta$quarter)
}))

#### Data Processing ####

# Set up list of states we are interested in for filtering
interested_states <- c("New Mexico", "Arizona", "California", "Nevada", "Utah", "Colorado", "Montana", "Idaho", "Wyoming", "Washington", "Oregon", "Alaska")

# Set up mapping between county names in water treatment data vs. our county shapes (all Alaska)
county_name_mapping <- c(
  "Kenai Peninsula Borough" = "Kenai Peninsula",
  "Anchorage Municipality" = "Anchorage",
  "Valdez-Cordova Census Area" = "Copper River",  # Split into Copper River & Chugach in 2019; only assigning Copper since Chugach is represented below
  "Ketchikan Gateway Borough" = "Ketchikan Gateway",
  "Matanuska-Susitna Borough" = "Matanuska-Susitna",
  "Fairbanks North Star Borough" = "Fairbanks North Star",
  "Bristol Bay Borough" = "Bristol Bay",
  "Bethel Census Area" = "Bethel",
  "Kodiak Island Borough" = "Kodiak Island",
  "Dillingham Census Area" = "Dillingham",
  "Aleutians East Borough" = "Aleutians East",
  "Denali Borough" = "Denali",
  "Yukon-Koyukuk Census Area" = "Yukon-Koyukuk",
  "Southeast Fairbanks Census Area" = "Southeast Fairbanks",
  "Juneau City and Borough" = "Juneau",
  "Wade Hampton Census Area" = "Kusilvak",  # Renamed to Kusilvak in 2015
  "Nome Census Area" = "Nome",
  "North Slope Borough" = "North Slope",
  "Northwest Arctic Borough" = "Northwest Arctic",
  "Skagway Municipality" = "Skagway",
  "Lake and Peninsula Borough" = "Lake and Peninsula",
  "Haines Borough" = "Haines",
  "Sitka City and Borough" = "Sitka",
  "Yakutat City and Borough" = "Yakutat",
  "Aleutians West Census Area" = "Aleutians West",
  "Wrangell City and Borough" = "Wrangell",
  "Hoonah-Angoon Census Area" = "Hoonah-Angoon",
  "Prince of Wales-Hyder Census Area" = "Prince of Wales-Hyder",
  "Chugach Census Area" = "Chugach"
)

# Do initial data cleaning
# Filter the data to:
# 1. Only counties in study area
# 2. Keep only rows with > 0 population served
# 3. Remove facility types we aren't interested in
water_treatment_data_cleaned <- water_treatment_data %>%
  janitor::clean_names() %>% # Clean names before working with filters, etc
  filter(primacy_agency %in% interested_states & population_br_served_count > 0 & pws_type == "Community water system") %>% # Agency is in interested state and pop served > 0 and is a Community water system; filtering to state and county combinations are necessary because county names can be duplicated across states; cannot just use county name for filtering without filtering states first
  mutate(counties_served = ifelse(is.na(counties_served) & cities_served == "CARSON CITY" & primacy_agency == "Nevada", "Carson City", counties_served),
         counties_served = ifelse(cities_served == "HYDER" & primacy_agency == "Alaska", "Prince of Wales-Hyder", counties_served),
         counties_served = ifelse(cities_served == "PETERSBURG" & primacy_agency == "Alaska", "Petersburg", counties_served),
         counties_served = ifelse(cities_served == "ANGOON" & primacy_agency == "Alaska", "Hoonah-Angoon", counties_served),
         counties_served = ifelse(cities_served == "HOONAH" & primacy_agency == "Alaska", "Hoonah-Angoon", counties_served)) %>% # Convert Carson City to county name if it is missing (it is a city-county); change some cities served to matching county (see notes on counties with no data below)
  drop_na(counties_served) %>% # If now no county name, don't include
  separate_wider_delim(counties_served, names_sep = "_", delim = ", ", too_few = "align_start") %>% # Make counties multiple columns instead of one
  mutate(across(starts_with("counties_served_"), ~ recode(.x, !!!county_name_mapping))) %>% # Alter names to match names in county (admin level 2) shapes
  select(pws_id, primacy_agency, counties_served_1, counties_served_2, population_br_served_count, number_of_violations, year, quarter) %>% # Select columns of interest going forward
  pivot_longer(cols = starts_with("counties_served"), names_to = "county_col", values_to = "county") %>% # Pivot longer to have one county per row
  select(-county_col) %>% # Remove county_col column
  drop_na(county) %>% # Drop rows with no county (ie. had only one county served but there were two county served columns)
  distinct() %>% # Make sure there are no duplicates (there shouldn't be; may not be needed)
  group_by(primacy_agency, county) %>% # Group by state and county
  summarize(total_pop_served = sum(population_br_served_count, na.rm = TRUE), # Sum population served across all systems in county
            num_sources = n(), # Count number of sources (ie. systems) in county
            total_violations = sum(number_of_violations, na.rm = TRUE), # Sum total violations across all systems in county
            .groups = "drop") # Drop grouping after summarizing

# Read in official county shapes and names
county_df <- study_area_admin_2 %>%
  filter(!is.na(state_name)) %>% # Filter out Canada divisions since we only have data for the US
  select(county, primacy_agency = state_name) # Select only relevant columns and rename state_name to primacy_agency for joining

# Join county shapes to water treatment data
water_treatment_data_cleaned_geom <- county_df %>%
  full_join(water_treatment_data_cleaned, by = c("county", "primacy_agency")) # Full join instead of left join should make any remaining name mismatches apparent; this df will have more counties than the county df if that is the case

# See which counties have no data and be sure that is accurate
water_treatment_data_cleaned_geom %>%
  filter(is.na(total_pop_served) & is.na(num_sources) & is.na(total_violations)) %>%
  select(county, primacy_agency)
# Prairie: has no community systems but has non-community systems, so this seems fine.
# Prince of Wales-Hyder: PRINCE OF WALES ISLAND is not mentioned in the data currently (just a different WALES city in Nome). HYDER is a city served which has Ketchikan Gateway Borough as corresponding county served for a system in the data. Prince of Wales-Hyder Census Area was previously part of Prince of Wales–Outer Ketchikan Census Area. It is now separate. We correct the county name. This county still drops out because it has no community systems.
# Petersburg: PETERSBURG is a city served which has Wrangell City and Borough as corresponding county served for a system in the data. Petersburg Census Area was previously part of Wrangell-Petersburg Census Area. These are now separate. We correct the county name.
# Hoonah-Angoon: HOONAH is a city served which has Skagway Municipality as corresponding county served for a system. ANGOON is a city served which has Skagway Municipality as corresponding county served for a system in the data. These both were previously part of Skagway–Yakutat–Angoon Census Area which then became Skagway–Hoonah–Angoon Census Area. Skagway and Yakutat are now separate. We correct the county name.
# Chugach: mentioned in a PWS name for a non-community system. Otherwise, nothing in current data, so this seems fine.
# Carson City: the county served column for Carson City, Nevada (found in the cities served column) seems to be NA, so we just fill it in with Carson City.

# If accurate, give remaining NAs for num_sources or total_violations 0s
water_treatment_data_cleaned_filled <- water_treatment_data_cleaned_geom %>%
  
  mutate(num_sources = if_else(is.na(num_sources) & !is.na(primacy_agency), 0, num_sources), # If num_sources is NA, give it a 0; we want to add it here so it is included in rescaling
         total_violations = if_else(is.na(total_violations), 0, total_violations), # If total_violations is NA, give it a 0; we want to add it here so it is included in rescaling
         num_sources_rescaled = scales::rescale(num_sources, to = c(0, 1)), # Rescale num_sources to 0-1
         total_violations_rescaled = scales::rescale(total_violations, to = c(1, 0)), # Rescale total_violations to 1-0 (so higher violations = lower score)
         county_score = num_sources_rescaled * total_violations_rescaled) %>% # Calculate county score as product of rescaled num_sources and total_violations
  select(primacy_agency, county, county_score, geometry) # Keep only relevant columns


# Rasterize layer to 90 m resolution raster
# Note: Could intersect to the US only before this?
water_treatment_rast <- terra::rasterize(water_treatment_data_cleaned_filled,
                                          study_area_rast,
                                          field = "county_score",
                                          fun = "mean")

# Plot unaligned raster
plot(water_treatment_rast)

# Align raster to template
water_treatment_rast <- align_raster_to_template(study_area_rast, water_treatment_rast)

# Plot aligned raster
plot(water_treatment_rast, main = "Water Treatment Scores 2024")

# Write out raster to indicators folder
writeRaster(water_treatment_rast, file.path(wri_project_root, "final_layers", "2024", "water", "indicators", "water_resistance_water_treatment.tif"), overwrite = TRUE)