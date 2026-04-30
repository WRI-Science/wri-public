#### Goal ####
# the goal of this script is to recale the treemap diversity data and 
# prepare it to be mosaiced with the scanfi data. Run script 1-3 before this. 
# The step 4 scripts can be run in any order. The output of this script will be
# further processed in the diversity folder.

# if redo_all is true it will take about an hour to run.

#### Packages ####
library(tidyverse)
library(terra)
library(data.table)
library(arrow)

#### File paths and setup ####
redo_all <- TRUE # set to TRUE to redo all steps

natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

multi_domain_data_file_path = "/home/shares/wwri-wildfire/data/multi_domain_data/"
raw_treemap_data_base = paste0(multi_domain_data_file_path, "raw/treemap/from_publication_zip/Data/")
treemap_template_raster_path <- paste0(raw_treemap_data_base, "TreeMap2016.tif")

species_count_tm_id_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_tm_id_w_unique_species_count.csv")

ecoregion_xy_assignment_path <- paste0(multi_domain_data_file_path, "/int/treemap/treemap_xy_with_ecoregion.csv")

tm_id_xy_path <- paste0(multi_domain_data_file_path, "int/treemap/study_area_treemap_2016_all_layers.csv")


# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))


# save paths
rescaled_csv_path <- paste0(natural_habitats_base_path, "int/treemap/rescaled_treemap_diversity.csv")
rescaled_save_path <- paste0(natural_habitats_base_path, "int/treemap/rescaled_treemap_diversity_90m.tif")
 
#### Main Processing ####
# now we need to calculate the stand density max per ecoregion
if (redo_all || !file.exists(rescaled_csv_path)) {
  print("Rescaling the diversity counts by ecoregion")
  # read in the ecoregions xy
  diversity_with_ecoregions <- fread(ecoregion_xy_assignment_path) %>% 
    rename(ecoregion = NA_L3KEY, X = x, Y = y)
  
  # read in the diversity df
  diversity_counts_tm_id <- fread(species_count_tm_id_path)
  
  # read in the tm_id_xy data
  tm_id_xy <- fread(tm_id_xy_path)
  
  # merge tm_id_xy with the diversity counts
  diversity_counts_xy <- merge(tm_id_xy, diversity_counts_tm_id,
                               by = "tm_id", all.x = TRUE)
  rm(diversity_counts_tm_id, tm_id_xy)
  
  # merge the ecoregions with the diversity counts
  diversity_with_ecoregions <- merge(diversity_counts_xy,
                                     diversity_with_ecoregions, 
                                     by = c("X", "Y"), 
                                     all.x = TRUE)
  rm(diversity_counts_xy)
  
  # there are about 0 rows that do not have an ecoregion assigned!
  # explore_NAs <- diversity_with_ecoregions %>%
  #   filter(is.na(ecoregion))
  
  # Calculate max tree diversity for each ecoregion
  diversity_with_ecoregions[, max_diversity := max(unique_tree_species_count, na.rm = TRUE), by = ecoregion]
  
  # Normalize between zero and one
  diversity_with_ecoregions[, rescaled_diversity := unique_tree_species_count / max_diversity]
  
  # save!
  fwrite(x = diversity_with_ecoregions,
         file = rescaled_csv_path)
  rm(diversity_with_ecoregions)
}

print("Starting rasterization process")
# reload (or load) the CSV
diversity_ecoregions <- data.table::fread(rescaled_csv_path)

# read in the treemap template raster
treemap_template_raster <- terra::rast(treemap_template_raster_path)

# turn your table into a SpatVector in that CRS
pts <- terra::vect(
  diversity_ecoregions,
  geom = c("X", "Y"),
  crs  = terra::crs(treemap_template_raster)
)

# rasterize onto the treemap grid
rescaled_tm_rast <- terra::rasterize(
  x          = pts,
  y          = treemap_template_raster,
  field      = "rescaled_diversity",
  background = NA
)
rm(pts, diversity_ecoregions)
gc()

template_raster <- terra::rast(template_raster_path)

# align the raster to the template
tree_rast <- align_raster_to_template(
  input_raster = rescaled_tm_rast,
  template_raster = template_raster,
  input_type = "continuous"
)

# name raster layer
names(tree_rast) <- "treemap_rescaled_diversity"

# save the raster
terra::writeRaster(
  tree_rast,
  rescaled_save_path,
  overwrite = TRUE
)
# clean up
rm(rescaled_tm_rast, tree_rast, template_raster)
gc()