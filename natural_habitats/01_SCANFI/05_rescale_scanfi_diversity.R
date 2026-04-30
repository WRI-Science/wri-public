#### Goal ####

# the goal of this script is to rescale the diversity data for SCANFI and 
# prepare it to be mosaiced with the treemap data. There are a number of 
# processes that need to be done before this but those will need to be 
# documented once steps are finalized.
# If redo_all is FALSE then this takes about an hour to run.

#### Packages ####
library(tidyverse)
library(data.table)
library(terra)

#### Setup and File Paths ####
redo_all <- FALSE # set to TRUE to redo all steps

natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

merged_cover_raw_path <-  paste0(natural_habitats_base_path, "int/scanfi/scanfi_merged_closure_all_species.csv")

scanfi_with_ecoregions_path <- paste0(natural_habitats_base_path, "int/scanfi/scanfi_xy_with_ecoregion.csv")

# scanfi template raster
scanfi_template_raster <- rast(paste0(natural_habitats_base_path, "raw/scanfi/SCANFI_att_closure_SW_2020_v1.2.tif"))

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# save paths
diversity_df_save_path <- paste0(natural_habitats_base_path, "int/scanfi/diversity_counts_xy.csv")
rescaled_csv_path <- paste0(natural_habitats_base_path, "int/scanfi/rescaled_scanfi_diversity.csv")
rescaled_tif_save_path <- paste0(natural_habitats_base_path, "int/scanfi/rescaled_scanfi_diversity_90m.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))

#### Main Processing ####
# read in the merged df with all species

if (redo_all || !file.exists(diversity_df_save_path)) {
  print("Coputing the species count xy dataframe")
  merged_cover_raw <- fread(merged_cover_raw_path)
  
  merged_cover_filtered <- merged_cover_raw %>% 
    filter(closure > 0)
  rm(merged_cover_raw)
  
  merged_cover_filtered$species_count <- rowSums(merged_cover_filtered[, (which(names(merged_cover_filtered) == "closure") + 1):ncol(merged_cover_filtered)] > 0)
  
  merged_species_count <- merged_cover_filtered %>% 
    select(x, y, species_count)
  
  # save the intermediate diversity counts csv
  fwrite(x = merged_species_count,
         file = diversity_df_save_path)
  
  rm(merged_cover_filtered, merged_species_count)
}

# now we need to calculate the stand density max per ecoregion
if (redo_all || !file.exists(rescaled_csv_path)) {
  print("Rescaling the diversity counts by ecoregion")
  # read in the ecoregions xy
  diversity_with_ecoregions <- fread(scanfi_with_ecoregions_path) %>% 
    rename(ecoregion = NA_L3KEY)
  
  # read in the merged_species_count
  diversity_counts_xy <- fread(diversity_df_save_path)
  
  # merge the ecoregions with the diversity counts
  diversity_with_ecoregions <- merge(diversity_counts_xy,
                                     diversity_with_ecoregions, 
                                     by = c("x", "y"), 
                                     all.x = TRUE)
  rm(diversity_counts_xy)
  
  
  # Calculate max tree diversity for each ecoregion
  diversity_with_ecoregions[, max_diversity := max(species_count, na.rm = TRUE), by = ecoregion]
  
  # Normalize between zero and one
  diversity_with_ecoregions[, rescaled_diversity := species_count / max_diversity]
  
  # save!
  fwrite(x = diversity_with_ecoregions,
         file = rescaled_csv_path)
  rm(diversity_with_ecoregions)
}

print("Rescaled diversity data is ready for rasterization and alignment. Starting process now...")
# now make rescaled data into a raster
# reload (or load) the CSV
count_ecoregion <- data.table::fread(rescaled_csv_path)

# turn your table into a SpatVector in that CRS
pts <- terra::vect(
  count_ecoregion,
  geom = c("x", "y"),
  crs  = terra::crs(scanfi_template_raster)
)

# rasterize onto the scanfi grid
rescaled_tm_rast <- terra::rasterize(
  x          = pts,
  y          = scanfi_template_raster,
  field      = "rescaled_diversity",
  background = NA
)
rm(pts, count_ecoregion, scanfi_template_raster)
gc()
print("Rasterization to scanfi template complete. Now aligning to the template raster...")

template_raster <- terra::rast(template_raster_path)

# align the raster to the template
tree_rast <- align_raster_to_template(
  input_raster = rescaled_tm_rast,
  template_raster = template_raster,
  input_type = "continuous"
)

names(tree_rast) <- "rescaled_diversity"

print("Alignment complete. Now saving...")

# save the raster
terra::writeRaster(
  tree_rast,
  rescaled_tif_save_path,
  overwrite = TRUE
)
# clean up
rm(rescaled_tm_rast, tree_rast, template_raster)
gc()
print("Rescaled SCANFI diversity raster saved successfully.")