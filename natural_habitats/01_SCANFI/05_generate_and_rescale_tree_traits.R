#### Goal ####
# The goal of this script is to generate the SCANFI tree trait data and then to
# rescale it based on ecoregions. Once this is output is generated it can be 
# joined with the treemap tree trait data. If redo all = TRUE this will take 
# about 2.5 hours to complete.

#### Packages ####
library(data.table)
library(terra)
library(future)
library(furrr)
library(progress)
library(ggplot2)
library(tidyverse)
library(pbapply)

#### Setup and File Paths ####
redo_all <- FALSE # set to TRUE to redo all steps

natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

scanfi_tree_trait_path <- paste0(natural_habitats_base_path, "int/tree_traits/tree_sp_can_95_for_scanfi.csv")

merged_cover_raw_path <-  paste0(natural_habitats_base_path, "int/scanfi/scanfi_merged_closure_all_species.csv")

# scanfi template raster
scanfi_template_raster <- rast(paste0(natural_habitats_base_path, "raw/scanfi/SCANFI_att_closure_SW_2020_v1.2.tif"))

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# Save Paths
unadjusted_tree_traits_path <- paste0(natural_habitats_base_path, "int/scanfi/unadjusted_tree_traits_matrix.csv")
adjusted_tree_traits_path <- paste0(natural_habitats_base_path, "int/scanfi/adjusted_tree_traits_matrix.csv")
scanfi_tree_traits_resistance_recovery_path <- paste0(natural_habitats_base_path, "int/scanfi/scanfi_tree_traits_resistance_recovery.csv")
resistance_tif_save_path <- paste0(natural_habitats_base_path, "int/scanfi/scanfi_tree_traits_resistance_90m.tif")
recovery_tif_save_path <- paste0(natural_habitats_base_path, "int/scanfi/scanfi_tree_traits_recovery_90m.tif")

source(here::here("templates_and_functions", "align_raster_to_template.R"))


#### Main Processing ####
# Part 1: Generate a traits table for all species that is unadjusted
# Check if the unadjusted tree traits file already exists
if (redo_all || !file.exists(unadjusted_tree_traits_path)) {
  # if this process executes it will take about 8 minutes on aurora
  print("First assigning tree codes to the SCANFI tree traits and closure data...")
  # read in the trait and closure data
  scanfi_tree_traits <- fread(scanfi_tree_trait_path)
  scanfi_closure <- fread(merged_cover_raw_path)
  
  # make a tree code column for all out species t1-t10
  scanfi_tree_traits$tree_code <- paste0(seq_len(nrow(scanfi_tree_traits)))
  
  # multiply the closure by the tree columns (not x y)
  # Identify the tree columns (assuming all except x, y, and closure)
  tree_cols <- setdiff(names(scanfi_closure), c("x", "y", "closure"))
  
  # Multiply in-place and divide by 10000 to get proportion from 0-1
  # we divide by 10000 the result of the percentage multiplication into a proportion
  scanfi_closure[, (tree_cols) := lapply(.SD, function(col) (col * closure) / 10000), .SDcols = tree_cols]
  
  # now need to make a match table between the tree codes and the tree columns
  # Create a data frame with tree codes and corresponding column names
  tree_code_mapping <- scanfi_tree_traits %>% 
    select(SCIENTIFIC_NAME, tree_code) %>% 
    # based on names from the scanfi data, make a key using casewhen
    mutate(scanfi_name = case_when(
      tree_code == "1" ~ "balsam_fir",
      tree_code == "2" ~ "tamarack",
      tree_code == "3" ~ "black_spruce",
      tree_code == "4" ~ "jack_pine",
      tree_code == "5" ~ "lodge_pole",
      tree_code == "6" ~ "ponderosa_pine",
      tree_code == "7" ~ "douglas_fir",
      tree_code == "8" ~ "white_red_pine",
      tree_code == "9" ~ "broadleaf_tree_prcB",
      tree_code == "10" ~ "other_coniferous_prcC",
      TRUE ~ NA_character_  # Default case for any unmatched tree codes
    ))
  
  # replace scanfi_closure column names with the tree codes
  # Create a named vector for renaming
  rename_vector <- setNames(tree_code_mapping$tree_code, tree_code_mapping$scanfi_name)
  
  # this is a check to make sure no tree species are missing
  missing <- setdiff(tree_cols, names(rename_vector))
  if (length(missing)) {
    stop("These scanfi_closure columns have no mapping: ", paste(missing, collapse = ", "))
  }
  
  # Rename the columns in scanfi_closure to match the tree codes
  setnames(
    scanfi_closure,
    old = tree_cols,
    new = rename_vector[tree_cols]
  )
  
  print("Rescaling the trait data...")
  # matrix math setup and execution
  # 1. Rescale traits and rename the matrix rows to tree code
  traits_rescaled <- scanfi_tree_traits %>%
    column_to_rownames(var = "tree_code") %>%   # makes tree_code the rownames
    select(-c(SCIENTIFIC_NAME, type)) %>%       # drop other metadata
    mutate(across(everything(), ~ (.-min(.))/(max(.)-min(.)))) %>%
    as.matrix()
  
  print("Matching column names and orders for math...")
  # 2. Identify your tree number columns (already done this with names above but now with numbers)
  tree_cols <- setdiff(names(scanfi_closure), c("x", "y", "closure"))
  
  # 3. Reorder the trait matrix to exactly match the closure columns:
  trait_mat_ordered <- traits_rescaled[tree_cols, ]
  
  print("Applying matrix multiplication to get mean traits for each closure cell...")
  # 4. Turn closure data into a matrix and multiply:
  closure_mat <- as.matrix(scanfi_closure[, ..tree_cols])
  mean_traits  <- closure_mat %*% trait_mat_ordered
  
  # 5. Build output data.frame
  df_out <- as.data.frame(mean_traits)
  df_out <- cbind(scanfi_closure[, .(x, y)], df_out)
  
  print("Saving intermediate file with unadjusted tree traits...")
  # 6. Save intermediate file
  fwrite(df_out, unadjusted_tree_traits_path)
  rm(closure_mat, mean_traits, scanfi_closure, scanfi_tree_traits, traits_rescaled, df_out, tree_cols, trait_mat_ordered, rename_vector, tree_code_mapping)
} else {
  print("Unadjusted tree traits file already exists, skipping generation step.")
}

# The next part is adjusting the longevity and seed mass traits
if (redo_all || !file.exists(adjusted_tree_traits_path)) {
  # if this part runs it should take about 4 minutes on aurora
  print("Adjusting the longevity and seed mass traits...")
  unadjusted_tree_traits <- fread(unadjusted_tree_traits_path)
  
  adjusted_tree_traits <- copy(unadjusted_tree_traits)[, `:=`(
    longevity_yrs_adj = 1 - longevity_yrs,
    seed_mass_adj = 1 - seed_mass_mg
  )][, c("longevity_yrs", "seed_mass_mg") := NULL]
  
  # Save the adjusted tree traits
  fwrite(adjusted_tree_traits, adjusted_tree_traits_path)
  rm(unadjusted_tree_traits, adjusted_tree_traits)
  
  print("Adjusted tree traits saved successfully.")
} else {
  print("Adjusted tree traits file already exists, skipping adjustment step.")
}

# Next, we will generate the resistance and recovery traits
# this process by itself should be about 4 minutes on aurora
if (redo_all || !file.exists(scanfi_tree_traits_resistance_recovery_path)) {
  print("Generating resistance and recovery traits...")
  resistance_recovery <- fread(adjusted_tree_traits_path)
  
  # Calculate rowwise mean for resistance: mean(avg_bark_percent, height_m, pruning)
  resistance_recovery[, resistance := rowMeans(.SD), 
                      .SDcols = c("avg_bark_percent", "height_m", "pruning")]
  
  # Calculate rowwise mean for recovery: mean(longevity_yrs_adj, resprout, seed_mass_adj, serotiny)
  resistance_recovery[, recovery := rowMeans(.SD), 
                      .SDcols = c("longevity_yrs_adj", "resprout", "seed_mass_adj", "serotiny")]
  
  # select needed columns
  resistance_recovery <- resistance_recovery[, .(x, y, resistance, recovery)]
  
  # Save the resistance and recovery traits
  fwrite(resistance_recovery, scanfi_tree_traits_resistance_recovery_path)
  rm(resistance_recovery)
  
  print("Resistance and recovery traits saved successfully.")
} else {
  print("Resistance and recovery traits file already exists, skipping generation step.")
}

# Now lets generate the final raster!
print("Rasterizing data into the scanfi template raster and then the final raster...")
resistance_recovery <- data.table::fread(scanfi_tree_traits_resistance_recovery_path)

# set terra threads otptions to 8
terraOptions(threads = 8)

# load the scanfi template raster
scanfi_template_raster <- terra::rast(scanfi_template_raster)

# turn your table into a SpatVector in that CRS
print("Converting resistance and recovery data to SpatVector...")
pts <- terra::vect(
  resistance_recovery,
  geom = c("x", "y"),
  crs  = terra::crs(scanfi_template_raster)
)
rm(resistance_recovery)

if (redo_all || !file.exists(resistance_tif_save_path)) {
  print("Rasterizing resistance and recovery traits onto the scanfi grid...")
  # rasterize onto the scanfi grid
  resistance_rast <- terra::rasterize(
    x = pts,
    y = scanfi_template_raster, 
    field = "resistance", 
    background = NA
  )
  
  # read in the template raster for alignment
  template_raster <- terra::rast(template_raster_path)
  
  # align the raster to the template
  print("Aligning resistance raster to the template raster...")
  resistance_rast <- align_raster_to_template(
    input_raster = resistance_rast,
    template_raster = template_raster,
    input_type = "continuous"
  )
  
  names(resistance_rast) <- "scanfi_traits_resistance"
  
  print("Alignment complete. Now saving resistance raster...")
  # save the resistance raster
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

if (redo_all || !file.exists(recovery_tif_save_path)) {
  # now repeat steps for recovery
  print("Rasterizing recovery traits onto the scanfi grid...")
  # rasterize onto the scanfi grid
  recovery_rast <- terra::rasterize(
    x = pts,
    y = scanfi_template_raster, 
    field = "recovery", 
    background = NA
  )
  
  # read in the template raster for alignment
  template_raster <- terra::rast(template_raster_path)
  
  # align the raster to the template
  print("Aligning resistance raster to the template raster...")
  recovery_rast <- align_raster_to_template(
    input_raster = recovery_rast,
    template_raster = template_raster,
    input_type = "continuous"
  )
  
  names(recovery_rast) <- "scanfi_traits_recovery"
  
  print("Alignment complete. Now saving recovery raster...")
  # save the resistance raster
  terra::writeRaster(
    recovery_rast,
    recovery_tif_save_path,
    overwrite = TRUE
  )
  # clean up everything remaining
  rm(recovery_rast, pts, scanfi_template_raster, template_raster)
  gc()
} else {
  print("Recovery raster already exists, skipping generation step.")
}
print("All SCANFI tree trait rasters generated and saved successfully or already exist.")

