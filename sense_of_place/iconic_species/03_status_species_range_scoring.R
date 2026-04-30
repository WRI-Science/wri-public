wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# Load libraries
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(data.table)
library(here)

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

#### Boundary layers ####
moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

study_area_1km_moll <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_with_na_moll.tif"))
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

study_area_admin0_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_moll.shp"))
study_area_admin1_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) %>%
  st_transform(moll_crs)
study_area_admin0.5_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_with_countries.shp")) %>%
  st_transform(moll_crs)

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####

# Read iconic species list and status scores
iconic_species_list <- read_csv(file.path(raw_data_file_path, "iconic_species_list - Sheet1 (16).csv"))
iconic_species_status_scores <- read_csv(file.path(final_layers_file_path, "indicators/iconic_species_status_scores.csv"))

# Read in species range shapefiles
species_range_maps <- file.path(intermediate_data_file_path, "species_range_maps_north_america")
iconic_species_filepaths <- list.files(species_range_maps, pattern = "\\.(shp|gpkg)$", recursive = TRUE, full.names = TRUE)

species_range_shapefiles <- lapply(iconic_species_filepaths, function(f) {
  shp <- st_read(f)
  shp <- if (st_crs(shp) != st_crs(moll_crs)) st_transform(shp, moll_crs) else shp
  shp <- if (!all(st_is_valid(shp))) st_make_valid(shp) else shp
})

names(species_range_shapefiles) <- tools::file_path_sans_ext(basename(iconic_species_filepaths))

# Check for name differences between lists
cat("Species only in NS score list:\n")
print(setdiff(unique(iconic_species_status_scores$rgbif_mol_sci_name),
              unique(iconic_species_list$rgbif_mol_sci_name)))
cat("\nSpecies only in iconic species list:\n")
print(setdiff(unique(iconic_species_list$rgbif_mol_sci_name),
              unique(iconic_species_status_scores$rgbif_mol_sci_name)))

# Fix species names (underscores)
iconic_species_status_scores <- iconic_species_status_scores %>%
  mutate(rgbif_mol_sci_name_underscore = gsub(" ", "_", rgbif_mol_sci_name))

#### Rasterize species ranges to 1km Mollweide and apply species status score to species polygon range ####

# Prepare output path
species_rasters_output <- file.path(intermediate_data_file_path, "2024/status/species_raster_ranges_1km_moll_cropped_masked_status_scored")

empty_rasters <- list()

for (i in seq_len(nrow(iconic_species_status_scores))) {
  species_name <- iconic_species_status_scores$rgbif_mol_sci_name_underscore[i]
  state <- iconic_species_status_scores$state[i]
  threat_score <- iconic_species_status_scores$status_score[i]
  
  if (!species_name %in% names(species_range_shapefiles)) {
    cat("No shapefile found for:", species_name, "\n")
    next
  }
  
  species_shape <- species_range_shapefiles[[species_name]]
  species_shape$score <- threat_score
  
  species_raster <- terra::rasterize(
    vect(species_shape),
    study_area_1km_moll,
    field = "score",
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
  raster_suffix <- if (is_empty) "_no_score.tif" else "_scored.tif"
  
  raster_filename <- file.path(
    species_rasters_output,
    paste0(species_name, "_", state, raster_suffix)
  )
  
  if (!file.exists(raster_filename)) {
    terra::writeRaster(species_raster_masked, raster_filename, overwrite = FALSE)
    cat("Saved raster for:", species_name, "-", state, "\n")
  } else {
    cat("Skipped (already exists):", species_name, "-", state, "\n")
  }
}

#### Organize rasters by state to score individual states downstream ####

# Read all raster files
all_species_rasters <- list.files(species_rasters_output, pattern = "\\.tif$", full.names = TRUE)

# Extract state name from filename
state_from_filename <- function(filename) {
  parts <- unlist(strsplit(tools::file_path_sans_ext(basename(filename)), "_"))
  if (length(parts) >= 2) parts[length(parts) - 1] else NA
}

# Build list of rasters grouped by state
state_raster_list <- split(all_species_rasters, sapply(all_species_rasters, state_from_filename))

# List of states/provinces to process
states_provinces <- c(
  "Nevada", "California", "Oregon", "Washington",
  "Idaho", "Montana", "Wyoming", "Utah",
  "Arizona", "New Mexico", "Colorado", "Alaska",
  "British Columbia", "Yukon", "Canada", "United States"
)

#### Visualization PDF only: Plotting species rasters for each state ####
# Assign the output PDF file path here:
output_pdf <- file.path(intermediate_data_file_path, "2024/status/species_rasters_all_states_status.pdf")

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


#### Create state combined average rasters at 1 km ####

masked_output_dir <- file.path(intermediate_data_file_path, "2024/status/state_raster_ranges_1km_moll_masked_status_scored")

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
  
  average_raster <- terra::app(masked_stack, fun = mean, na.rm = TRUE) # na.rm = TRUE to ignore NA status score species
  masked_raster_averages[[state]] <- average_raster
  
  output_file <- file.path(masked_output_dir, paste0("average_raster_", gsub(" ", "_", state), ".tif"))
  writeRaster(average_raster, filename = output_file, overwrite = TRUE)
  
  cat(paste("Masked average raster for", state, "written to:", output_file, "\n"))
}

# Check the masked raster stacks for a specific state
plot(masked_raster_averages$`California`)

# Set output PDF path
output_pdf <- file.path(intermediate_data_file_path, "2024/status/all_state_average_status_rasters.pdf")

# Create PDF device
pdf(output_pdf, width = 11, height = 8.5)  # Landscape

for (state in names(masked_raster_averages)) {
  avg_raster <- masked_raster_averages[[state]]
  if (!is.null(avg_raster)) {
    plot(
      avg_raster, 
      main = paste("Average Raster -", state), 
      col = rev(terrain.colors(20)), 
      axes = FALSE, 
      box = FALSE
    )
  }
}

dev.off()

cat("PDF with all state average rasters saved at:", output_pdf, "\n")

#### Bind all state species status rasters together for our study area ####

# Define the directory containing the state average rasters
state_species_status_dir <- file.path(intermediate_data_file_path, "2024/status/state_raster_ranges_1km_moll_masked_status_scored")

# List all .tif raster files
raster_files <- list.files(state_species_status_dir, pattern = "\\.tif$", full.names = TRUE)

# Read each raster file into a list
state_status_rasters <- lapply(raster_files, terra::rast)

# Mosaic (merge) all the rasters together into a single raster
state_status_rasters_merged <- do.call(terra::mosaic, state_status_rasters)

plot(state_status_rasters_merged, main = "Status: no extinction threshold")

# write out status 1km 5070 raster 
writeRaster(state_status_rasters_merged, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_status_1km_moll_no_extinction.tif"),
            overwrite = TRUE)

#### Apply lower extinction threshold on status ####

# Custom rescaling function
rescale_75_extinction <- function(x) {
  ifelse(x < 0.25, 0, (x - 0.25) / 0.75)
}

# Apply rescaling to the merged raster
state_status_rescaled <- terra::app(state_status_rasters_merged, rescale_75_extinction)

# Plot for quick check
plot(state_status_rescaled, main = "Status: 75% extinction risk threshold rescaled")

# Write out the rescaled raster at 1km moll
writeRaster(state_status_rescaled, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_status_1km_moll_75_extinction_rescaled.tif"),
            overwrite = TRUE)

#### Write out the status rescaled raster at 90 5070 ####

# Align indicator with study_area_90m_template raster
sense_of_place_iconic_species_status_90m_5070 <- align_raster_to_template(study_area_90m_5070, state_status_rescaled, input_type = "continuous")

# Write the updated raster to a file
writeRaster(sense_of_place_iconic_species_status_90m_5070, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_status_75_extinction_rescaled.tif"),
            overwrite = TRUE)

plot(sense_of_place_iconic_species_status_90m_5070, main = "Aligned Status Raster at 90m")




