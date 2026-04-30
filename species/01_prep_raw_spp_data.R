wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf) # For working with spatial (vector/shapefile) data
library(tidyverse) # For data manipulation
library(here) # For sourcing the functions script cleanly later


#### Script Overview #### 
# The purpose of this script is to get the species data cleaned and prepared with just our potential species of interest.
# We use primarily IUCN spatial data and BirdLife spatial data, with AVONET data supplementing for filtering out marine bird habitats (we are not interested in marine birds for our purposes).


#### Base directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024" # Update this if needed for the year of the data
raw_data_file_path <- file.path(data_file_path, "raw", path_year)
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path <- file.path(int_data_file_path, "species_data_with_initial_processing")


#### Data Layers ####
# Get all IUCN shapefile paths and read in the shapefiles
iucn_shp_filepaths <- list.files(path = file.path(raw_data_file_path, "iucn"), 
                                 pattern = "\\.shp$", 
                                 recursive = TRUE, 
                                 full.names = TRUE)
iucn_shps <- lapply(iucn_shp_filepaths, st_read)

# Read in the BirdLife spatial layers
# Check formatting hasn't changed and edit if needed
# bird_lyr_opts <- st_layers(file.path(raw_data_file_path, "birdlife", "species", "BOTW_2024_2.gpkg")) # Inspect layer options if needed
bird_spp <- st_read(file.path(raw_data_file_path, "birdlife", "species", "BOTW_2024_2.gpkg"), 
                    layer = "all_species") # Layer with geometries; warnings are normal
bird_spp_checklist <- st_read(file.path(raw_data_file_path, "birdlife", "species", "BOTW_2024_2.gpkg"), 
                              layer = "main_BL_HBW_Checklist_V9") # Layer with more needed info; warnings are normal

# Read in AVONET to get marine vs. not info with some light cleaning
# Species that don't match AVONET but are extinct, so it is ok they don't have matches: Ectopistes migratorius, Conuropsis carolinensis
# We may not have perfect matches and you may need to add some, but we won't know until we see what is in the study area. Do not bother making sure species not in study area match, as it will not be used.
# Note: This has not been redownloaded in 2024/2025, but should be static.
avonet_traits <- read_csv(file.path(raw_data_file_path, "avonet", "ELEData", "TraitData", "AVONET1_BirdLife.csv")) %>%
  select(avonet_sci_name = Species1, habitat = Habitat) %>%
  mutate(sci_name = case_when(
    avonet_sci_name == "Amazilia violiceps" ~ "Leucolia violiceps",
    avonet_sci_name == "Antigone canadensis" ~ "Grus canadensis",
    avonet_sci_name == "Nannopterum brasilianus" ~ "Nannopterum brasilianum",
    avonet_sci_name == "Nannopterum auritus" ~ "Nannopterum auritum",
    avonet_sci_name == "Cyanecula svecica" ~ "Luscinia svecica",
    avonet_sci_name == "Regulus calendula" ~ "Corthylio calendula",
    avonet_sci_name == "Falcipennis canadensis" ~ "Canachites canadensis",
    avonet_sci_name == "Falcipennis franklinii" ~ "Canachites franklinii",
    TRUE ~ avonet_sci_name
  )) # Note: Could also check out the synonyms csv from IUCN instead to see if this could be done more automatically than manually listing the synonyms needed


#### Functions ####
source(here("biodiversity", "00_biodiversity_custom_functions.R")) # For the prepare_birdlife and prepare_iucn functions to clean the data


#### Data Processing ####
# Prepare IUCN and BirdLife data for general usage
# Use custom function to update IUCN column names and BirdLife column names and join the BirdLife Tables, and filter to the species of potential interest
# Check formatting hasn't changed for the data -- you may need to edit stuff in the functions if so
bird_spp_prepped <- prepare_birdlife(bird_spp = bird_spp, 
                                     bird_spp_checklist = bird_spp_checklist, 
                                     avonet_traits = avonet_traits, 
                                     output_path = output_path)
iucn_shps_prepped <- prepare_iucn(iucn_shps)

# Save these as R data and read them back in so you don't need to repeat these steps as much (prepped is for use in resistance/recovery because it has different filters)
saveRDS(bird_spp_prepped, file = file.path(output_path, "bird_spp_prepped.rds"))
saveRDS(iucn_shps_prepped, file = file.path(output_path, "iucn_shps_prepped.rds"))

# Save as geopackages too which may be more stable
# Note: May want to transition to just using this method at some point
st_write(bird_spp_prepped, file.path(output_path, "bird_spp_prepped.gpkg"), delete_dsn = TRUE)
for (i in seq_along(iucn_shps_prepped)) {
  filename <- sprintf("iucn_shps_prepped_%d.gpkg", i)
  st_write(iucn_shps_prepped[[i]], file.path(output_path, filename), delete_dsn = TRUE)
}