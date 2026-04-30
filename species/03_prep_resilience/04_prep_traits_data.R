library(tidyverse) # For data manipulation


#### Script Overview ####
# This script prepares the traits data, joins it with the species range area data, and calculates the resistance (resistance traits only) and recovery scores (mean of recovery traits score and range area rescaled) for each species.


#### Base Directories ####
data_file_path <- "/home/shares/wwri-wildfire/data/biodiversity"
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_dfs_status") # output_path from previous steps of status processing
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files


#### Data Layers ####
# Read in the traits data that contains our species (year-over-year new species may need to be added)
traits <- read_csv("/home/shares/wwri-wildfire/data/multi_domain_data/traits/final_trait_lists/animal_traits_iucn_species.csv") # Manually created/coded by Rachel King

# Read in the IUCN Red List data from the API that was saved in the previous step
iucn_red_list_from_api <- read.csv(file.path(output_path_prev, "iucn_red_list_from_api.csv")) %>%
  select(id_no = iucn_sid, sci_name = sciname) # Change some column names for the join

# Read in the species range sizes that were calculated in the previous step
spp_with_landscape_metric <- read_csv(file.path(output_path, "spp_range_sizes.csv")) %>%
  left_join(iucn_red_list_from_api, by = "id_no")


#### Data Processing ####
# Clean traits data to have scores following this scheme:
# Semel/itero (RECOVERY) - 0 for semel, 1 for itero
# Bipart life (RESISTANCE) - 1 for yes, 0 for no
# Longevity (RECOVERY) - 0-1 based on max value. Long lived organisms get lower resilience
# Reproductive output (RECOVERY) - 0 to 1, Higher repro output gets higher score
# Age to first repro (RECOVERY) - 0-1 based on max value. Long lived organisms get lower resilience
# Asex (RECOVERY), gills (RESISTANCE), wings (RESISTANCE and RECOVERY) - 0 for no, 1 for yes
# Mass (RESISTANCE AND RECOVERY) - 0-1 based on max value. Larger organisms get lower resilience
# Cell wall (NOTHING) - delete column.

# Join traits data to the species range area data
spp_with_traits <- spp_with_landscape_metric %>%
  left_join(traits, by = "sci_name")

# Check to make sure no NAs (ie. that all species have traits) -- look into any that exist
col_na_counts <- colSums(is.na(spp_with_traits))
print(col_na_counts) # All good!

# Check the column values are as expected to ensure the following section works correctly
unique(spp_with_traits$semel_itero) # Should be "semelparous" or "iteroparous"
unique(spp_with_traits$bipart_lifecycle) # Should specify the parts or say "no"
is.numeric(spp_with_traits$longevity_y) # Should be TRUE
is.numeric(spp_with_traits$annual_repro_young_per_y) # Should be TRUE
is.numeric(spp_with_traits$age_first_repro_y) # Should be TRUE
unique(spp_with_traits$asexual_repro) # Should be "yes" or "no"
unique(spp_with_traits$gills) # Should be "yes" or "no"
unique(spp_with_traits$wings) # Should be "yes" or "no"
is.numeric(spp_with_traits$body_mass_g) # Should be TRUE

# Clean the traits data according to our scheme
spp_with_traits_cleaned <- spp_with_traits %>%
  mutate(
    semel_itero = ifelse(semel_itero == "semelparous", 0, 1), # 0 for semel, 1 for itero
    bipart_lifecycle = ifelse(bipart_lifecycle == "no", 0, 1), # 1 for yes, 0 for no
    longevity_y = scales::rescale(longevity_y, to = c(1, 0)), # 0-1 based on max value, Long lived organisms get lower resilience
    annual_repro_young_per_y = scales::rescale(annual_repro_young_per_y, to = c(0, 1)), # 0 to 1, Higher repro output gets higher score
    age_first_repro_y = scales::rescale(age_first_repro_y, to = c(1, 0)), # 0-1 based on max value, Long lived organisms get lower resilience
    asexual_repro = ifelse(asexual_repro == "yes", 1, 0), # 1 for yes, 0 for no
    gills = ifelse(gills == "yes", 1, 0), # 1 for yes, 0 for no
    wings = ifelse(wings == "yes", 1, 0), # 1 for yes, 0 for no
    body_mass_g = scales::rescale(body_mass_g, to = c(1, 0)) # 0-1 based on max value, Larger organisms get lower resilience
  ) %>%
  select(-cell_wall) # Remove the cell wall column as it is not used in the resilience calculations

# Calculate the resistence and recovery scores at the species level based on the cleaned traits data
spp_with_traits_summarized <- spp_with_traits_cleaned %>%
  rowwise() %>% # Ensure calculations are done row-wise (by species, each has one row)
  mutate(
    traits_recovery = mean(c(semel_itero, longevity_y, annual_repro_young_per_y, 
                          age_first_repro_y, body_mass_g, 
                          asexual_repro, wings), na.rm = TRUE),
    traits_resistance = mean(c(bipart_lifecycle, gills, wings, body_mass_g), na.rm = TRUE)
  ) %>%
  select(id_no, sci_name, traits_recovery, traits_resistance, geom_area_rescaled) %>% # Select relevant columns
  ungroup() # Remove any row-wise grouping

# Write the summarized species data with resistance and recovery indicator scores to a CSV file
write_csv(spp_with_traits_summarized, file.path(output_path, "spp_resistance_recovery_indicator_scores.csv"))