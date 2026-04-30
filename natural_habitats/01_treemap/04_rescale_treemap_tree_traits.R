wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal ####
# the goal of this script is to generate the tree trait matrix for the treemap 
# data and then to rescale it based on ecoregions. Once this is output is 
# generated it can be joined with the treemap tree trait data.
# This takes about 1 hour and 35 minutes to run when redo_all is set to TRUE.

#### Packages ####
library(data.table)
library(terra)
library(future)
library(furrr)
library(progress)
library(ggplot2)
library(tidyverse)
library(pbapply)

#### File paths and setup ####
redo_all <- FALSE # set to TRUE to redo all steps

natural_habitats_base_path <- file.path(wri_project_root, "data", "natural_habitats")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

treemap_tree_trait_path <- paste0(natural_habitats_base_path, "int/tree_traits/traits_95_west_conus.csv")

study_region_coords_w_tm_id_individual_species_count_path <- paste0(natural_habitats_base_path, "int/treemap/study_region_coords_w_tm_id_individual_species_count.csv")

# treemap template raster
raw_treemap_data_base = paste0(multi_domain_data_file_path, "raw/treemap/from_publication_zip/Data/")
treemap_template_raster_path <- paste0(raw_treemap_data_base, "TreeMap2016.tif")

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# Save Paths
unadjusted_tree_traits_path <- paste0(natural_habitats_base_path, "int/treemap/unadjusted_tree_traits_matrix.csv")
adjusted_tree_traits_path <- paste0(natural_habitats_base_path, "int/treemap/adjusted_tree_traits_matrix.csv")
treemap_tree_traits_resistance_recovery_path <- paste0(natural_habitats_base_path, "int/treemap/treemap_tree_traits_resistance_recovery.csv")
resistance_tif_save_path <- paste0(natural_habitats_base_path, "int/treemap/treemap_tree_traits_resistance_90m.tif")
recovery_tif_save_path <- paste0(natural_habitats_base_path, "int/treemap/treemap_tree_traits_recovery_90m.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))

#### Main Proccesing ####
# Part 1: Generate a traits table for all species that is unadjusted
# Check if the unadjusted tree traits file already exists
if (redo_all || !file.exists(unadjusted_tree_traits_path)) {
  print("Generating unadjusted tree trait matrix...")
  
  #read in and format traits
  print("Reading tree trait CSV...")
  traits<-read.csv(treemap_tree_trait_path)
  
  #make sci name row name
  rownames(traits) <- traits[, "FIA_CODE"]
  #remove sci name
  traits <- traits[, -c(1:2)]  # Remove the tm_id column after setting row names
  
  print("Rescaling traits...")
  traits_rescaled <- traits %>%
    mutate(across(everything(), ~ ( . - min(.) ) / ( max(.) - min(.) )))
  traits_rescaled<-as.matrix(traits_rescaled)
  
  # Get the list of FIA_CODEs from the traits data
  valid_species <- rownames(traits_rescaled)
  
  print("Reading study region species count header...")
  header <- fread(study_region_coords_w_tm_id_individual_species_count_path, nrows = 1)
  
  valid_columns <- which(names(header) %in% valid_species)
  
  print("Reading filtered treemap species count data...")
  treemap <- fread(study_region_coords_w_tm_id_individual_species_count_path, 
                   select = c(1, 2, 3, valid_columns))
  
  treemap_id<-treemap[,1:3]
  treemap <- treemap[,-c(1:3) ]
  treemap<-as.matrix(treemap)
  
  print("Matching trait matrix to treemap species...")
  traits_rescaled_ordered <- traits_rescaled[match(colnames(treemap), rownames(traits_rescaled)), ]
  
  print("Multiplying species matrix with traits matrix...")
  result <- treemap %*% traits_rescaled_ordered
  plot_species_count <- rowSums(treemap)
  mean_trait_matrix <- result / plot_species_count
  
  print("Creating output dataframe...")
  df<-as.data.frame(mean_trait_matrix)
  rm(result, mean_trait_matrix)
  df<-cbind(treemap_id, df)
  
  print("Saving unadjusted tree trait matrix to CSV...")
  fwrite(df, unadjusted_tree_traits_path)
  rm(df, traits_rescaled_ordered, treemap, traits, treemap_id, plot_species_count)
  gc()
  
} else {
  print("Unadjusted tree traits file already exists, skipping generation step.")
}

# The next part is adjusting the longevity and seed mass traits
if (redo_all || !file.exists(adjusted_tree_traits_path)) {
  print("Adjusting the longevity and seed mass traits...")
  
  print("Reading unadjusted trait CSV...")
  df <- fread(unadjusted_tree_traits_path)
  
  dt_adjusted_long_mass <- copy(df)[, `:=`(
    longevity_yrs_adj = 1 - longevity_yrs,
    seed_mass_adj = 1 - seed_mass_mg
  )][, c("longevity_yrs", "seed_mass_mg") := NULL]
  
  rm(df)
  
  print("Saving adjusted traits to CSV...")
  fwrite(dt_adjusted_long_mass, adjusted_tree_traits_path)
  rm(dt_adjusted_long_mass)
  print("Adjusted tree traits saved successfully.")
} else {
  print("Adjusted tree traits file already exists, skipping adjustment step.")
}

# Next, we will generate the resistance and recovery traits
if (redo_all || !file.exists(treemap_tree_traits_resistance_recovery_path)) {
  print("Generating resistance and recovery traits...")
  
  print("Reading adjusted trait matrix CSV...")
  resistance_recovery <- fread(adjusted_tree_traits_path)
  
  print("Calculating resistance metric...")
  resistance_recovery[, resistance := rowMeans(.SD), 
                      .SDcols = c("avg_bark_percent", "height_m", "pruning")]
  
  print("Calculating recovery metric...")
  resistance_recovery[, recovery := rowMeans(.SD), 
                      .SDcols = c("longevity_yrs_adj", "resprout", "seed_mass_adj", "serotiny")]
  
  resistance_recovery <- resistance_recovery[, .(X, Y, resistance, recovery)]
  
  print("Saving resistance and recovery metrics to CSV...")
  fwrite(resistance_recovery, treemap_tree_traits_resistance_recovery_path)
  rm(resistance_recovery)
  
  print("Resistance and recovery traits saved successfully.")
} else {
  print("Resistance and recovery traits file already exists, skipping generation step.")
}

# Now lets generate the final raster!
print("Rasterizing data into the treemap template raster and then the final raster...")
resistance_recovery <- data.table::fread(treemap_tree_traits_resistance_recovery_path)

terraOptions(threads = 8)

treemap_template_raster <- terra::rast(treemap_template_raster_path)

print("Converting resistance and recovery data to SpatVector...")
pts <- terra::vect(
  resistance_recovery,
  geom = c("X", "Y"),
  crs  = terra::crs(treemap_template_raster)
)
rm(resistance_recovery)

if (redo_all || !file.exists(resistance_tif_save_path)) {
  print("Rasterizing resistance traits...")
  resistance_rast <- terra::rasterize(
    x = pts,
    y = treemap_template_raster, 
    field = "resistance", 
    background = NA
  )
  
  print("Reading alignment template...")
  template_raster <- terra::rast(template_raster_path)
  
  print("Aligning resistance raster to the template raster...")
  resistance_rast <- align_raster_to_template(
    input_raster = resistance_rast,
    template_raster = template_raster,
    input_type = "continuous"
  )
  
  names(resistance_rast) <- "treemap_traits_resistance"
  
  print("Saving resistance raster to disk...")
  terra::writeRaster(
    resistance_rast,
    resistance_tif_save_path,
    overwrite = TRUE
  )
  rm(resistance_rast)
  gc()
} else {
  print("Resistance raster already exists, skipping generation step.")
}

#if (redo_all || !file.exists(recovery_tif_save_path)) {
  print("Rasterizing recovery traits...")
  recovery_rast <- terra::rasterize(
    x = pts,
    y = treemap_template_raster, 
    field = "recovery", 
    background = NA
  )
  
  print("Reading alignment template...")
  template_raster <- terra::rast(template_raster_path)
  
  print("Aligning recovery raster to the template raster...")
  recovery_rast <- align_raster_to_template(
    input_raster = recovery_rast,
    template_raster = template_raster,
    input_type = "continuous"
  )
  
  names(recovery_rast) <- "treemap_traits_recovery"
  
  print("Saving recovery raster to disk...")
  terra::writeRaster(
    recovery_rast,
    recovery_tif_save_path,
    overwrite = TRUE
  )
  rm(recovery_rast, pts, treemap_template_raster, template_raster)
  gc()
# } else {
#   print("Recovery raster already exists, skipping generation step.")
# }

print("All treemap tree trait rasters generated and saved successfully or already exist.")
