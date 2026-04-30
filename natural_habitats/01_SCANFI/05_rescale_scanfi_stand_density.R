wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####

# the goal of this script is to rescale the stand density data for SCANFI and 
# prepare it to be mosaiced with the treemap data. There are a number of 
# processes that need to be done before this but those will need to be 
# documented once steps are finalized.

#### Packages ####
library(tidyverse)
library(data.table)
library(terra)

#### Setup and File Paths ####
redo_all <- TRUE # set to TRUE to redo all steps
rescale_method <- "median"

natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

# These files paths will need to be fixed once the data and prior processes and cleaned
merged_cover_raw_path <-  paste0(natural_habitats_base_path, "int/scanfi/scanfi_merged_closure_all_species.csv")

diversity_with_ecoregions_path <- paste0(natural_habitats_base_path, "int/scanfi/scanfi_xy_with_ecoregion.csv")

# scanfi template raster
# this path will need to be updated
scanfi_template_raster <- rast(paste0(natural_habitats_base_path, "raw/scanfi/SCANFI_att_closure_SW_2020_v1.2.tif"))

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))


# Save Paths
rescaled_csv_path <- paste0(natural_habitats_base_path, "int/scanfi/rescaled_scanfi_stand_density.csv")
rescaled_tif_save_path <- paste0(natural_habitats_base_path, "int/scanfi/rescaled_scanfi_stand_density_90m.tif")

#### Main Processing ####
# SCANFI does not have species count like treemap so we will need to use the 
# closure % as a proxy to rescale stand density

merged_closure <- fread(merged_cover_raw_path) %>% 
  select(x, y, closure) 

#### Main Processing ####
if (redo_all || !file.exists(rescaled_csv_path)) {
  # read in the merged closure data and get the x, y and closure columns
  merged_closure <- fread(merged_cover_raw_path) %>% 
    select(x, y, closure) 
  
  # read in the ecoregion data and get the x, y and ecoregion columns 
  ecoregion_assignment <- fread(diversity_with_ecoregions_path) %>% 
    select(x, y, ecoregion = NA_L3KEY)
  
  # Perform the join
  closure_ecoregion <- merge(merged_closure, ecoregion_assignment, by = c("x", "y"), all.x = TRUE)
  # remove mreged_closure and ecoregion_assignment from memory
  rm(merged_closure, ecoregion_assignment)
  
  # there are about 0 rows that do not have an ecoregion assigned
  explore_NAs <- closure_ecoregion %>% 
     filter(is.na(ecoregion))
  
  # keep this just in case
  closure_ecoregion <- closure_ecoregion %>% 
    filter(!is.na(ecoregion))
  
  # groupby ecoregion and calculate median or mean tree density for each using data.table syntax depending on rescale_method
  # use ifelse to determine whether to use median or mean for rescaling
  if (rescale_method == "median") {
    ecoregion_density_for_rescale <- closure_ecoregion[, .(avg_density = median(closure, na.rm = TRUE)), by = ecoregion]
  } else if (rescale_method == "mean") {
    ecoregion_density_for_rescale <- closure_ecoregion[, .(avg_density = mean(closure, na.rm = TRUE)), by = ecoregion]
  } else {
    stop("Invalid rescale method. Please choose 'median' or 'mean'.")
  }
  
  # now with the avg_density of each ecoregion we need to also calculate the p99 for each ecoregion
  ecoregion_denstiy_p99 <- closure_ecoregion[, .(p99_density = quantile(closure, probs = 0.99, na.rm = TRUE)), by = ecoregion]
  
  # now we can merge the two dataframes together
  ecoregion_density_for_rescale <- full_join(ecoregion_density_for_rescale, ecoregion_denstiy_p99, by = "ecoregion")
  
  # calculate a slope up down and intercept down for rescaling
  ecoregion_density_for_rescale <- ecoregion_density_for_rescale %>%
    mutate(slope_up = 1 / avg_density,
           intercept_up = 0,
           slope_down = -1 / (p99_density - avg_density),
           intercept_down = p99_density / (p99_density - avg_density))
  
  # now merge back with the original closure_ecoregion dataframe based on ecoregion
  closure_ecoregion <- merge(closure_ecoregion, ecoregion_density_for_rescale, by = "ecoregion")
  
  # now we can rescale the density data
  closure_ecoregion <- closure_ecoregion %>%
    # when tree_count is greater than p99_density we will set it to p99_density
    mutate(closure = ifelse(closure > p99_density, p99_density, closure)) %>%
    mutate(rescaled_density = case_when(
      closure == 0 ~ 0,
      closure >= p99_density ~ 0,
      closure == avg_density ~ 1,
      closure < avg_density ~ (slope_up * closure) + intercept_up,
      closure > avg_density ~ (slope_down * closure) + intercept_down
    ))
  
  # now we can save the rescaled data to a csv file
  fwrite(closure_ecoregion, rescaled_csv_path)
}

# reload (or load) the CSV
closure_ecoregion <- data.table::fread(rescaled_csv_path)

# turn your table into a SpatVector in that CRS
pts <- terra::vect(
  closure_ecoregion,
  geom = c("x", "y"),
  crs  = terra::crs(scanfi_template_raster)
)

# rasterize onto the scanfi grid
rescaled_tm_rast <- terra::rasterize(
  x          = pts,
  y          = scanfi_template_raster,
  field      = "rescaled_density",
  background = NA
)
rm(pts, closure_ecoregion, scanfi_template_raster)
gc()

template_raster <- terra::rast(template_raster_path)

# align the raster to the template
tree_rast <- align_raster_to_template(
  input_raster = rescaled_tm_rast,
  template_raster = template_raster,
  input_type = "continuous"
)

names(tree_rast) <- "rescaled_density"

# save the raster
terra::writeRaster(
  tree_rast,
  rescaled_tif_save_path,
  overwrite = TRUE
)
# clean up
rm(rescaled_tm_rast, tree_rast, template_raster)
gc()