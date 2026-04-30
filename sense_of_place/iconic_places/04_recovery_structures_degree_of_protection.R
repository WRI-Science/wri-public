wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# script to assign degree of protection recovery score to historic structures 

library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_places")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp"))
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

# Define the states of interest
states_of_interest <- c("WASHINGTON", "CALIFORNIA", "OREGON", "IDAHO", "ARIZONA", 
                        "NEVADA", "WYOMING", "COLORADO", "UTAH", "MONTANA", 
                        "NEW MEXICO", "ALASKA")
#### Data Layers ####
# 20096 entries
historic_structures <- st_read(file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg")) %>% 
  rename(geometry = geom)
# US National Register of Historic Places spreadsheet with information about Level of Significance
nrhp_spreadsheet <- read_excel(file.path(raw_data_file_path, "national-register-listed-20240710.xlsx"), sheet = 1) %>% 
  filter(State %in% states_of_interest)

#### Assign US Degree of Protection ####

# Select only US structures
us_historic_structures <- historic_structures %>%
  filter(!is.na(State_us))

# Handling duplicate Property IDs by selecting the most recent 'Last Action Date'
# removed 51 rows
nrhp_filtered <- nrhp_spreadsheet %>%
  group_by(`Property ID`) %>%
  filter(`Last Action Date` == max(`Last Action Date`)) %>%
  ungroup()

# Check to see there are no International or Not Indicated levels of significance
# all False = good
table(nrhp_filtered$`Level of Significance - Not Indicated`) # none
table(nrhp_filtered$`Level of Significance - International`) # none
table(nrhp_filtered$`Level of Significance - National`) # 1892
table(nrhp_filtered$`Level of Significance - State`) # 4288
table(nrhp_filtered$`Level of Significance - Local`) # 11140

# Align property ID numbers in the nris points and nrhp levle of significance df's 
# 14744
# 2139 structures don't have a similar property ID (seems many of these have restricted addresses / sensitive locations)
us_nris_spatial_points_5070_wwri_states_full <- left_join(us_historic_structures, nrhp_filtered, 
                                                          by = c("NR_PROPERTYID" = "Property ID"), 
                                                          relationship = "many-to-many")

# Score Level of Significance - National = 1, Level of Significance - State = 0.5, Level of Significance - Local = 0
us_nris_spatial_points_5070_wwri_states_full_rescaled <- us_nris_spatial_points_5070_wwri_states_full %>%
  mutate(degree_of_protection = case_when(
    `Level of Significance - National` == "True" ~ 1,
    `Level of Significance - State` == "True" ~ 0.5,
    `Level of Significance - Local` == "True" ~ 0,
    TRUE ~ NA_real_  # Assigns NA to rows where none of the conditions match
  ))

table(us_nris_spatial_points_5070_wwri_states_full_rescaled$degree_of_protection, useNA = "ifany")
# Score Counts
# 0  0.5    1 <NA> 
# 9957 3438 1229   120

# View 120 NA rows
us_nris_spatial_points_5070_wwri_states_full_rescaled %>%
  filter(is.na(degree_of_protection)) %>%
  View()

#### BC ####

# Select only BC structures
# 5658
bc_historic_structures <- historic_structures %>%
  filter(!is.na(Province_bc))

# Assign rescaled extent of value score 
bc_historic_rescaled <- bc_historic_structures %>%
  mutate(degree_of_protection = case_when(
    RCGNTN_GVL == "Federal" ~ 1,
    RCGNTN_GVL == "Provincial" ~ 0.5,
    RCGNTN_GVL == "Municipal" ~ 0,
    TRUE ~ NA_real_  # Assigns NA to rows where none of the conditions match
  ))

# check counts
table(bc_historic_rescaled$degree_of_protection, useNA = "ifany")
# 0  0.5    1 
# 5054  210   17


#### YT ####

# Select only YT structures
# 42
yt_historic_structures <- historic_structures %>%
  filter(!is.na(YHSI_ID))

# Assign rescaled degree of protection scores 
# Score a score of 1 for anything that has 'National Historic Site of Canada' in the name and 0 for other 
yt_historic_rescaled <- yt_historic_structures %>%
  mutate(degree_of_protection = ifelse(grepl("National Historic Site of Canada", SITE_NAME), 1, 0))

# check counts
table(yt_historic_rescaled$degree_of_protection, useNA = "ifany")
# 0  1 
# 33  9 

#### Join the rescaled degree of protection US, BC, YT df's #### 

# join all the rescaled extent of value dfs from the US, BC and YT into one dataframe 
# 20067 rows 
recovery_rescaled_degree_of_protection <- bind_rows(
  us_nris_spatial_points_5070_wwri_states_full_rescaled,
  bc_historic_rescaled,
  yt_historic_rescaled)

# check counts
table(recovery_rescaled_degree_of_protection$degree_of_protection, useNA = "ifany")
# 0   0.5     1  <NA> 
# 15044  3648  1255   120 

# Recovery: Extent of Value plot with polygons 
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070) +
  geom_sf(
    data = recovery_rescaled_degree_of_protection,
    aes(
      colour = degree_of_protection,
      fill   = degree_of_protection),
    size = 0.1) +
  scale_colour_viridis_c(option = "plasma", guide = "none") +
  scale_fill_viridis_c(option = "plasma", name = "Extent of Significance") +
  theme_minimal() +
  labs(title = "Recovery: Structures: Extent of Significance")

# Check geometry types and counts
# 20067
geometry_types <- st_geometry_type(recovery_rescaled_degree_of_protection) %>% 
  table()
print(geometry_types)

# Convert to terra vector
recovery_rescaled_degree_of_protection_vect <- vect(recovery_rescaled_degree_of_protection)

#### Rasterize indicator or score #### 
wwri_sense_of_place_iconic_places_degree_of_protection <- terra::rasterize(
  recovery_rescaled_degree_of_protection_vect,
  study_area_90m_5070,
  field = "degree_of_protection", 
  fun = "mean")

# Align indicator with study_area_90m_template raster
wwri_sense_of_place_iconic_places_degree_of_protection <- align_raster_to_template(study_area_90m_5070, wwri_sense_of_place_iconic_places_degree_of_protection, input_type = "categorical")

plot(wwri_sense_of_place_iconic_places_degree_of_protection, main = "Recovery: Structures: Degree of Protection")

# Save raster to aurora
writeRaster(wwri_sense_of_place_iconic_places_degree_of_protection, 
            filename = file.path(final_layers_file_path, "indicators/sense_of_place_iconic_places_recovery_degree_of_protection.tif"),
            overwrite = TRUE)





