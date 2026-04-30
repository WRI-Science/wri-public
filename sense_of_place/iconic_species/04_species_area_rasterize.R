# script to calculate the species area and rasterize 

library(terra)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)
library(scales)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_species"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Boundary layers ####
moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'
study_area_1km_moll <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_with_na_moll.tif"))
study_area_admin1_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>%
  st_transform(moll_crs)
study_area_admin0.5_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_with_countries.shp")) %>%
  st_transform(moll_crs)
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

# List of states/provinces to process
states_provinces <- c(
  "Nevada", "California", "Oregon", "Washington",
  "Idaho", "Montana", "Wyoming", "Utah",
  "Arizona", "New Mexico", "Colorado", "Alaska",
  "British Columbia", "Yukon", "Canada", "United States")

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
# read in list of iconic species within each state
iconic_species_list <- read_csv(file.path(raw_data_file_path, "iconic_species_list - Sheet1 (16).csv")) %>% 
  # remove unnecessary cols
  dplyr::select(ns_sci_name, rgbif_mol_sci_name, state, common_name)

# Read in the species North American range shapefiles, check crs 5070, calculate North American range area
species_range_maps <- file.path(intermediate_data_file_path, "species_range_maps_north_america")
iconic_species_filepaths <- list.files(species_range_maps, pattern = "\\.(shp|gpkg)$", recursive = TRUE, full.names = TRUE)

# Convert ranges to Mollweide 
species_range_shapefiles <- lapply(iconic_species_filepaths, function(f) {
  shp <- st_read(f)
  shp <- if (st_crs(shp) != st_crs(moll_crs)) st_transform(shp, moll_crs) else shp
  shp <- if (!all(st_is_valid(shp))) st_make_valid(shp) else shp
})

names(species_range_shapefiles) <- tools::file_path_sans_ext(basename(iconic_species_filepaths))

# 100 species ranges read in
message("Successfully read in ", length(species_range_shapefiles), " files.")

#### Calculate North American species range area and rescale ####

species_area_summary <- tibble(
  sci_name = names(species_range_shapefiles),
  total_area_m2 = map_dbl(species_range_shapefiles, ~ as.numeric(sum(st_area(.x), na.rm = TRUE)))
) %>%
  mutate(
    rescaled_area = rescale(total_area_m2, to = c(0,1))
  )

# Join scores to species-state df
state_spp_area_scores <- iconic_species_list %>%
  mutate(rgbif_mol_sci_name_underscore = gsub(" ", "_", rgbif_mol_sci_name)) %>% 
  left_join(
    species_area_summary %>%
      dplyr::select(sci_name, rescaled_area),
    by = c("rgbif_mol_sci_name_underscore" = "sci_name"),
    relationship = "many-to-many"
  )

#### Resistance trait rasters ####

# prepare output folder
species_rasters_output_area_recovery <- file.path(
  intermediate_data_file_path, "2024/area_recovery/species_raster_range_1km_moll_area_recovery_cropped_masked_rescaled")

empty_rasters <- list()

for (i in seq_len(nrow(state_spp_area_scores))) {
  species_name <- state_spp_area_scores$rgbif_mol_sci_name_underscore[i]
  state <- state_spp_area_scores$state[i]
  rescaled_area_score <- state_spp_area_scores$rescaled_area[i]
  
  if (!species_name %in% names(species_range_shapefiles)) {
    cat("No shapefile found for:", species_name, "\n")
    next
  }
  
  species_shape <- species_range_shapefiles[[species_name]]
  species_shape$rescaled_area_score <- rescaled_area_score
  
  species_raster <- terra::rasterize(
    vect(species_shape),
    study_area_1km_moll,
    field = "rescaled_area_score",
    touches = TRUE
  )
  
  # Check for all-NA raster
  is_empty <- all(is.na(values(species_raster)))
  
  if (is_empty) {
    cat("Empty raster for:", species_name, "-", state, "\n")
    empty_rasters[[paste0(species_name, "_", state)]] <- vect(species_shape)
  }
  
  species_raster_masked <- terra::mask(species_raster, study_area_1km_moll)
  
  # Adjust filename based on whether raster is empty
  raster_suffix <- if (is_empty) "_no_rescaled_area_score.tif" else "_rescaled_area_score.tif"
  
  raster_filename <- file.path(
    species_rasters_output_area_recovery,
    paste0(species_name, "_", state, raster_suffix)
  )
  
  # Always write (and overwrite if already present)
  terra::writeRaster(
    species_raster_masked,
    raster_filename,
    overwrite = TRUE
  )
  cat("Written (overwrite=TRUE) for:", species_name, "-", state, "\n")
}

#### Recovery area rasters - organize rasters by state to score individual states downstream ####

# Read all raster files
all_species_rasters <- list.files(species_rasters_output_area_recovery, pattern = "\\.tif$", full.names = TRUE)

# Extract state name from filename
state_from_filename <- function(filename) {
  fname <- tools::file_path_sans_ext(basename(filename))
  # grab whatever’s between the LAST two “_” before the suffix
  sub("^.+_([^_]+(?: [^_]+)*)_(?:no_)?rescaled_area_score$", "\\1", fname)
}

# Build list of rasters grouped by state
state_raster_list <- split(all_species_rasters, sapply(all_species_rasters, state_from_filename))

# Visualization PDF only: Plotting species rasters for each state 
output_pdf <- file.path(intermediate_data_file_path, "2024/area_recovery/species_rasters_all_states_area_recovery.pdf")

plot_state_rasters <- function(state, state_raster_list, state_boundaries) {
  matched_rasters <- state_raster_list[[state]]
  if (is.null(matched_rasters) || length(matched_rasters) == 0) {
    cat(paste("No rasters to plot for state:", state, "\n"))
    return()
  }
  
  cat(paste("Plotting rasters for state:", state, "\n"))
  n_rasters <- length(matched_rasters)
  nrows <- ceiling(sqrt(n_rasters))
  ncols <- ceiling(n_rasters / nrows)
  
  par(mfrow = c(nrows, ncols), mar = c(2, 2, 2, 2), oma = c(0, 0, 2, 0))
  
  for (raster_path in matched_rasters) {
    rast_obj <- terra::rast(raster_path)
    terra::plot(
      rast_obj,
      main = basename(raster_path),
      col = "darkgreen",
      range = c(0, 1),
      colNA = "gray90",   # plots rasters with NA ranges as grey (6 species)
      axes = TRUE,
      frame = FALSE
    )
    
    boundary <- state_boundaries[state_boundaries$name == state, ]
    if (nrow(boundary) > 0) {
      plot(st_geometry(boundary), add = TRUE, border = "red", lwd = 1)
    }
  }
  
  mtext(paste("Rasters for State:", state), outer = TRUE, cex = 1.5, font = 2)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

pdf(output_pdf, width = 11, height = 8.5)  # Open PDF device

for (region in states_provinces) {
  plot_state_rasters(region, state_raster_list, study_area_admin1_shape_moll)
}

dev.off()  # Close PDF device
cat("Wrote all plots to", output_pdf, "\n")


#### Recovery area rasters - create state masked average area recovery rasters at 1 km ####

state_area_recovery_output_dir <- file.path(intermediate_data_file_path, "2024/area_recovery/state_raster_ranges_1km_moll_masked_area_recovery_scored")

masked_raster_averages <- list()

for (state in names(state_raster_list)) {
  matched_rasters <- state_raster_list[[state]]
  if (length(matched_rasters) == 0) next
  
  raster_stack <- rast(matched_rasters)
  
  # Select correct boundary
  if (state %in% c("United States", "Canada")) {
    boundary <- study_area_admin0.5_shape_moll %>% filter(country == state)
  } else {
    boundary <- study_area_admin1_shape_moll %>% filter(name == state)
  }
  
  masked_stack <- terra::mask(raster_stack, vect(boundary))
  
  average_raster <- terra::app(masked_stack, fun = mean, na.rm = TRUE) # na.rm = TRUE to ignore NA resistance species
  masked_raster_averages[[state]] <- average_raster
  
  output_file <- file.path(state_area_recovery_output_dir, paste0("average_raster_", gsub(" ", "_", state), ".tif"))
  writeRaster(average_raster, filename = output_file, overwrite = TRUE)
  
  cat(paste("Masked average raster for", state, "written to:", output_file, "\n"))
}

# Set output PDF path
output_pdf <- file.path(intermediate_data_file_path, "2024/area_recovery/all_state_average_area_recovery_rasters.pdf")

# Create PDF device
pdf(output_pdf, width = 11, height = 8.5)  # Landscape

for (state in names(masked_raster_averages)) {
  avg_raster <- masked_raster_averages[[state]]
  if (!is.null(avg_raster)) {
    plot(
      avg_raster, 
      main = paste("Average Recovery Raster -", state), 
      col = rev(terrain.colors(20)), 
      axes = FALSE, 
      box = FALSE
    )
  }
}

dev.off()

cat("PDF with all state average recovery rasters saved at:", output_pdf, "\n")

#### Recovery area rasters - bind all state species recovery rasters together for our study area ####

# Define the directory containing the state average rasters
state_species_recovery_dir <- file.path(intermediate_data_file_path, "2024/area_recovery/state_raster_ranges_1km_moll_masked_area_recovery_scored")

# List all .tif raster files
raster_files <- list.files(state_species_recovery_dir, pattern = "\\.tif$", full.names = TRUE)

# Read each raster file into a list
state_recovery_rasters <- lapply(raster_files, terra::rast)

# Mosaic (merge) all the rasters together into a single raster
state_recovery_rasters_merged <- do.call(terra::mosaic, state_recovery_rasters)

crs(state_recovery_rasters_merged) <- moll_crs

plot(state_recovery_rasters_merged, main = "Area Recovery")

# write out recovery 1km 5070 raster 
writeRaster(state_recovery_rasters_merged, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_area_recovery_1km_moll.tif"),
            overwrite = TRUE)

#### Recovery area rasters - rasterize and reproject to 90m and 5070 ####

# Align indicator with study_area_90m_template raster
species_area_recovery_90m_5070_rast <- align_raster_to_template(study_area_90m_5070, state_recovery_rasters_merged, input_type = "continuous")

# Write the updated raster to a file
writeRaster(species_area_recovery_90m_5070_rast, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_area_recovery.tif"),
            overwrite = TRUE)
