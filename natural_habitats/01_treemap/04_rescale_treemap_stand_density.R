#### Goal ####
# the goal of this script is to recale the treemap stand density data and 
# prepare it to be mosaiced with the scanfi data. Run script 1-3 before this. 
# The step 4 scripts can be run in any order. The output of this script will be
# further processed in the diversity folder.

#### Packages ####
library(tidyverse)
library(terra)
library(data.table)
library(arrow)

#### File paths and setup ####
redo_all <- TRUE # set to TRUE to redo all steps
rescale_method <- "median"

natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

multi_domain_data_file_path = "/home/shares/wwri-wildfire/data/multi_domain_data/"
raw_treemap_data_base = paste0(multi_domain_data_file_path, "raw/treemap/from_publication_zip/Data/")
treemap_template_raster_path <- paste0(raw_treemap_data_base, "TreeMap2016.tif")

tm_id_tree_count_df_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_tm_id_w_tree_count.csv")

ecoregion_xy_assignment_path <- paste0(multi_domain_data_file_path, "/int/treemap/treemap_xy_with_ecoregion.csv")

tm_id_xy_path <- paste0(multi_domain_data_file_path, "int/treemap/study_area_treemap_2016_all_layers.csv")


# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))


# save paths
rescaled_csv_path <- paste0(natural_habitats_base_path, "int/treemap/rescaled_treemap_stand_density.csv")
rescaled_save_path <- paste0(natural_habitats_base_path, "int/treemap/rescaled_treemap_stand_density_90m.tif")

#### Main Processing ####
if (redo_all || !file.exists(rescaled_csv_path)) {
  # read in the data
  tm_id_tree_count_df <- fread(tm_id_tree_count_df_path) 
  
  ecoregion_xy_assignment <- fread(ecoregion_xy_assignment_path) %>% 
    rename(X = x, Y = y)
  
  tm_id_xy <- fread(tm_id_xy_path)
  
  ecoregion_xy_assignment <- ecoregion_xy_assignment[, .(X, Y, NA_L3KEY)]
  
  # merge ecoregion_xy_assignment with the xy_tm_id data w/ data.table syntax
  ecoregion_tm_id_xy <- merge(ecoregion_xy_assignment, tm_id_xy, by = c("X", "Y"), all.x = TRUE)
  rm(ecoregion_xy_assignment, tm_id_xy)
  
  # Perform the left join
  density_ecoregions <- merge(ecoregion_tm_id_xy, tm_id_tree_count_df, by = c("tm_id"), all.x = TRUE)
  rm(ecoregion_tm_id_xy)
    
  # there are about 0 rows that do not have an ecoregion assigned!
  # explore_NAs <- density_ecoregions %>%
  #   filter(is.na(NA_L3KEY))
  
  # density_ecoregions <- density_ecoregions %>% 
  #   filter(!is.na(NA_L3KEY))
  
  # groupby ecoregion and calculate median or mean tree density for each using data.table syntax depending on rescale_method
  # use ifelse to determine whether to use median or mean for rescaling
  if (rescale_method == "median") {
    ecoregion_denstiy_for_rescale <- density_ecoregions[, .(avg_density = median(tree_count, na.rm = TRUE)), by = NA_L3KEY]
  } else if (rescale_method == "mean") {
    ecoregion_denstiy_for_rescale <- density_ecoregions[, .(avg_density = mean(tree_count, na.rm = TRUE)), by = NA_L3KEY]
  } else {
    stop("Invalid rescale method. Please choose 'median' or 'mean'.")
  }
  
  # now with the avg_density of each ecoregion we need to also calculate the p99 for each ecoregion
  ecoregion_denstiy_p99 <- density_ecoregions[, .(p99_density = quantile(tree_count, probs = 0.99, na.rm = TRUE)), by = NA_L3KEY]
  
  # now we can merge the two dataframes together
  ecoregion_denstiy_for_rescale <- merge(ecoregion_denstiy_for_rescale, ecoregion_denstiy_p99, by = "NA_L3KEY")
  
  # calculate a slope up down and intercept down for rescaling
  ecoregion_denstiy_for_rescale <- ecoregion_denstiy_for_rescale %>%
    mutate(slope_up = 1 / avg_density,
           intercept_up = 0,
           slope_down = -1 / (p99_density - avg_density),
           intercept_down = p99_density / (p99_density - avg_density))
  
  # now merge back wit the original density_ecoregions dataframe based on NA_L3KEY
  density_ecoregions <- merge(density_ecoregions, ecoregion_denstiy_for_rescale, by = "NA_L3KEY")
  
  # now we can rescale the density data
  density_ecoregions <- density_ecoregions %>%
    # when tree_count is greater than p99_density we will set it to p99_density
    mutate(tree_count = ifelse(tree_count > p99_density, p99_density, tree_count)) %>%
    mutate(rescaled_density = case_when(
      tree_count == 0 ~ 0,
      tree_count >= p99_density ~ 0,
      tree_count == avg_density ~ 1,
      tree_count < avg_density ~ (slope_up * tree_count) + intercept_up,
      tree_count > avg_density ~ (slope_down * tree_count) + intercept_down
    ))
  
  # now we can save the rescaled data to a csv file
  fwrite(density_ecoregions, rescaled_csv_path)
}

# reload (or load) the CSV
density_ecoregions <- data.table::fread(rescaled_csv_path)

# read in the treemap template raster
treemap_template_raster <- terra::rast(treemap_template_raster_path)

# turn your table into a SpatVector in that CRS
pts <- terra::vect(
  density_ecoregions,
  geom = c("X", "Y"),
  crs  = terra::crs(treemap_template_raster)
)

# rasterize onto the treemap grid
# this part of the script makes the memory usage huge! Just warning
rescaled_tm_rast <- terra::rasterize(
  x          = pts,
  y          = treemap_template_raster,
  field      = "rescaled_density",
  background = NA
)
rm(pts, density_ecoregions)
gc()

template_raster <- terra::rast(template_raster_path)

# align the raster to the template
tree_rast <- align_raster_to_template(
  input_raster = rescaled_tm_rast,
  template_raster = template_raster,
  input_type = "continuous"
)

# set the names of the raster layers
names(tree_rast) <- "treemap_rescaled_density"

# save the raster
terra::writeRaster(
  tree_rast,
  rescaled_save_path,
  overwrite = TRUE
)
# clean up
rm(rescaled_tm_rast, tree_rast, template_raster)
gc()