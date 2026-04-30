#### Goal ####
# The goal of this script is to take the dataframes generated in step 3 and 
# calculate the historical percent natural values for 2005 and 2015. The output 
# for this script will be combined with the present day percent natural calculation
# to determine how things have changed over time.

#### Packages ####
library(terra)
library(tidyverse)

# Set memory options for terra
terraOptions(memfrac=0.8)  # Use up to 80% of available memory

#### File Paths ####
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"
data_file_path <- "/home/shares/wwri-wildfire/data/"
raw_data_file_path <- file.path(data_file_path, "natural_habitats/raw/")
int_data_file_path <- file.path(data_file_path, "natural_habitats/int/")
historical_landcover_int_data_file_path <- file.path(int_data_file_path, "historical_landcover")

# Land cover dataframe paths
landcover_count_by_ecoregion_2005_path <- file.path(historical_landcover_int_data_file_path, paste0("landcover_by_ecoregion_2005.csv"))
landcover_count_by_ecoregion_2015_path <- file.path(historical_landcover_int_data_file_path, paste0("landcover_by_ecoregion_2015.csv"))

# Save paths for output dataframes
percent_natural_by_ecoregion_2005_save_path <- file.path(historical_landcover_int_data_file_path, paste0("percent_natural_ecoregion_2005.csv"))
percent_natural_by_ecoregion_2015_save_path <- file.path(historical_landcover_int_data_file_path, paste0("percent_natural_ecoregion_2015.csv"))


#### Process Land Cover Data ####
# Read in the land cover data
landcover_counts_2005 <- read.csv(landcover_count_by_ecoregion_2005_path)
landcover_counts_2015 <- read.csv(landcover_count_by_ecoregion_2015_path)

# Filter to remove land cover classes we are excluding
landcover_counts_2005 <- landcover_counts_2005 %>%
  filter(!lc_id %in% c(210, # water body
                       0, # filled value
                       250 # filled value
  )) %>%
  # make a column to determine what cells are natural and unnatural
  mutate(designation = ifelse(lc_id %in% c(10, 11, 12, 20, # agriculture
                                           190), # built
                              "unnatural", "natural")) %>%
  # group by the ecoregion and calculate the total number of cells that are natural
  # unnatural, and the total cells
  group_by(eco_id, eco_name) %>%
  # calculate the total number of natural and unnatural cells
  summarise(natural_cells = sum(count[designation == "natural"]),
            unnatural_cells = sum(count[designation == "unnatural"])) %>%
  # calculate the total number of cell
  mutate(total_cells = sum(natural_cells, unnatural_cells)) %>% 
  # calculate the percent of natural and unnatural cells
  ungroup() %>% 
  mutate(percent_natural = (natural_cells / total_cells) * 100,
         percent_unnatural = (unnatural_cells / total_cells)* 100) 

# do the same for 2015
landcover_counts_2015 <- landcover_counts_2015 %>%
  filter(!lc_id %in% c(210, # water body
                       0, # filled value
                       250 # filled value
  )) %>%
  # make a column to determine what cells are natural and unnatural
  mutate(designation = ifelse(lc_id %in% c(10, 11, 12, 20, # agriculture
                                           190), # built
                              "unnatural", "natural")) %>%
  # group by the ecoregion and calculate the total number of cells that are natural
  # unnatural, and the total cells
  group_by(eco_id, eco_name) %>%
  # calculate the total number of natural and unnatural cells
  summarise(natural_cells = sum(count[designation == "natural"]),
            unnatural_cells = sum(count[designation == "unnatural"])) %>%
  # calculate the total number of cell
  mutate(total_cells = sum(natural_cells, unnatural_cells)) %>% 
  # calculate the percent of natural and unnatural cells
  ungroup() %>% 
  mutate(percent_natural = (natural_cells / total_cells) * 100,
         percent_unnatural = (unnatural_cells / total_cells)* 100)

# Save the dataframes
write.csv(landcover_counts_2005, 
          file = percent_natural_by_ecoregion_2005_save_path, 
          row.names = FALSE)

write.csv(landcover_counts_2015,
          file = percent_natural_by_ecoregion_2015_save_path, 
          row.names = FALSE)