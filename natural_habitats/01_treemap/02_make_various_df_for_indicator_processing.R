wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to generate the tm_id dfs to be joined with the step 1
# output that will be used to calculate stand density, diversity, and tree trait indicators.
# The outputs of these dfs will feed into the indicator processing scripts.
# Think takes about 20 minutes to run as a background job from top to bottom.

#### Packages ####
library(tidyverse)
library(data.table)
library(terra)

#### Setup and File Paths ####
multi_domain_data_base_path <- file.path(wri_project_root, "data", "multi_domain_data")
natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")

study_area_df_path <- paste0(multi_domain_data_base_path, "int/treemap/study_area_treemap_2016_all_layers.csv")
tree_table_df_path <- paste0(multi_domain_data_base_path, "raw/treemap/from_publication_zip/Data/TreeMap2016_tree_table.csv")

# Save Paths
tree_count_save_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_tm_id_w_tree_count.csv")
species_count_save_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_tm_id_w_unique_species_count.csv")  
study_region_coords_w_tm_id_individual_species_count_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_coords_w_tm_id_individual_species_count.csv")

#### Make Tree Count (density) DF to join later ####
# read in the study area df and select the relevant columns
study_area_tm_id_df <- fread(study_area_df_path) %>% 
  select(X, Y, tm_id)

# make a list of the unique tm_id in the study area
unique_tm_id_list <- study_area_tm_id_df %>% 
  select(tm_id) %>% 
  unique()

# remove the big df to save memory, we only need the tm_ids
rm(study_area_tm_id_df)

# read in the tree table df and select the relevant columns
tree_table_raw <- fread(tree_table_df_path)

tree_count_per_tm_id <- tree_table_raw %>% 
  # filter only for alive trees
  filter(STATUSCD == 1,
         # filter also for only those im_id in our study region
         tm_id %in% unique(unique_tm_id_list$tm_id)) %>%
  # now group by and count the number of rows as each row is equal to 1 tree
  group_by(tm_id) %>% 
  summarise(tree_count = n()) %>% 
  ungroup()

write_csv(x = tree_count_per_tm_id,
          file = tree_count_save_path)

#### Make Number of Species (Diversity) DF to join later #### 
tm_id_unique_species_count <- tree_table_raw %>% 
  # filter only for alive trees
  filter(STATUSCD == 1,
         # filter also for only those im_id in our study region
         tm_id %in% unique(unique_tm_id_list$tm_id)) %>%
  # select the only tm_id and species ids to get unique combos
  select(tm_id, SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  unique() %>% 
  # now group by and count the number of rows as each row is equal to 1 tree
  group_by(tm_id) %>% 
  summarise(unique_tree_species_count = n()) %>% 
  ungroup()

write_csv(x = tm_id_unique_species_count,
          file = species_count_save_path)

#### Make full species table for tree trait matrix ####
tree_table_tm_id_species_count <- tree_table_raw %>% 
  # filter only for alive trees
  filter(STATUSCD == 1,
         # filter also for only those im_id in our study region
         tm_id %in% unique(unique_tm_id_list$tm_id)) %>% 
  group_by(tm_id, SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  summarise(species_count = n()) %>% 
  ungroup()

# now we need to make this a wide df
tree_table_tm_id_species_count_wide <- tree_table_tm_id_species_count %>% 
  select(tm_id, SPCD, species_count) %>% 
  pivot_wider(names_from = "SPCD",
              values_from = "species_count")

# make the NAs zero 
tree_table_tm_id_species_count_wide[is.na(tree_table_tm_id_species_count_wide)] <- 0

study_area_tm_id_df <- fread(study_area_df_path) %>% 
  select(X, Y, tm_id)

# now join with the tm_id xy data
coords_w_tm_id_species_count <- left_join(x = study_area_tm_id_df,
                                          y = tree_table_tm_id_species_count_wide,
                                          by = "tm_id")

write_csv(x = coords_w_tm_id_species_count,
          file = study_region_coords_w_tm_id_individual_species_count_path)
