wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)
library(scales)

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

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

# Read in IUCN traits but only filter for those in iconic species list
# And remove Oncorhynchus clarkii, mykiss and tshawytscha bc they are already in the animal_traits csv
# 37 rows
iucn_animal_traits <- read_csv(file.path(multi_domain_data_file_path, "traits/final_trait_lists/animal_traits_iucn_species.csv")) %>%
  filter(sci_name %in% c(iconic_species_list$ns_sci_name, 
                         iconic_species_list$rgbif_mol_sci_name)) %>% 
  filter(!sci_name %in% c("Oncorhynchus clarkii", "Oncorhynchus mykiss", "Oncorhynchus tshawytscha"))
# Read in partial iconic species animal & plant & tree lists
# 21 rows
animal_traits <- read_csv(file.path(multi_domain_data_file_path, "traits/final_trait_lists/iconic_species_traits - animals.csv")) %>%
  dplyr::select(-"cell_wall")
# 23 rows
plant_traits <- read_csv(file.path(multi_domain_data_file_path, "traits/final_trait_lists/iconic_species_traits - plants (6).csv")) %>% 
  dplyr::select(-"crown_height", -"annual_perennial", -"serotiny")
# 16 rows
tree_traits <- read_csv(file.path(multi_domain_data_file_path, "traits/final_trait_lists/iconic_species_traits - tree (2).csv"))

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

#### RESISTANCE + RECOVERY: TRAITS ####

# For the animal_traits duplicate O.clarkii twice and O. mykiss once to create trait rows for those subspecies
# Duplicate rows where sci_name == "O.clarkii" and rename them
clarkii_duplicates <- animal_traits %>%
  filter(sci_name == "Oncorhynchus clarkii") %>%
  slice(rep(1:n(), each = 2)) %>%  # Duplicate each row twice
  mutate(sci_name = rep(c("Oncorhynchus henshawi", "Oncorhynchus virginalis"), length.out = n()))

# Duplicate row where sci_name == "O.mykiss" and rename it
mykiss_duplicate <- animal_traits %>%
  filter(sci_name == "Oncorhynchus mykiss") %>%
  mutate(sci_name = "Oncorhynchus aguabonita")

# Append duplicated rows to the original dataframe
animal_traits_updated <- animal_traits %>%
  bind_rows(clarkii_duplicates, mykiss_duplicate)

# add plant and animal and tree traits csv to full trait csv
all_traits_combined <- bind_rows(iucn_animal_traits, animal_traits_updated, plant_traits, tree_traits)

# Check both ns_sci_name and rgbif_mol_sci_name in trait data 
# 0 species = all species accounted for in trait data 
missing_species1 <- iconic_species_list %>%
  filter(!(ns_sci_name %in% all_traits_combined$sci_name) & 
           !(rgbif_mol_sci_name %in% all_traits_combined$sci_name))
print(missing_species1)

# Extract only the trait data for the species in the iconic_species_list ns_sci_name and rgbif_mol_sci_name
# 100 traits - all accounted for 
iconic_species_traits <- all_traits_combined %>%
  filter(sci_name %in% c(iconic_species_list$ns_sci_name, 
                         iconic_species_list$rgbif_mol_sci_name))

# Change the name of 5 animal sci_names in iconic_species_traits to match with range names for joining
iconic_species_traits <- iconic_species_traits %>%
  mutate(sci_name = case_when(
    sci_name == "Bison bison" ~ "Bos bison",
    sci_name == "Bubo scandiacus" ~ "Bubo scandiaca",
    sci_name == "Spinus tristis" ~ "Carduelis tristis",
    sci_name == "Chamerion angustifolium" ~ "Epilobium angustifolium",
    sci_name == "Pascopyrum smithii" ~ "Elymus smithii",
    TRUE ~ sci_name  # Keep other names unchanged
  ))

# Check missing species again 
# 0 species = all accounted for
missing_species2 <- iconic_species_list %>%
  filter(!(ns_sci_name %in% iconic_species_traits$sci_name) & 
           !(rgbif_mol_sci_name %in% iconic_species_traits$sci_name))
print(missing_species2)

#### Rescale animal, plant, and tree traits ####

# make traits with symbols in them numeric
clean_longevity <- function(longevity) {
  longevity <- as.character(longevity)  # Ensure it's character
  longevity <- str_trim(longevity)  # Trim extra spaces
  
  # Handle values with `>` (increase the number by 10%)
  longevity <- gsub(">(\\d+)", "(as.numeric(\\1) * 1.1)", longevity) 
  
  # Handle ranges like "5-10" → midpoint (7.5)
  longevity <- gsub("^(\\d+)-(\\d+)$", "(\\1 + \\2) / 2", longevity)
  
  # Convert to numeric, evaluating expressions
  longevity <- sapply(longevity, function(x) {
    if (is.na(x) || x == "") {
      return(NA)  # Keep NA values
    } else if (grepl("/", x) || grepl("\\*", x)) {  
      return(eval(parse(text = x)))  # Evaluate expressions (midpoints or > values)
    } else {
      return(as.numeric(x))  # Convert direct numbers
    }
  }, USE.NAMES = FALSE)
  
  return(longevity)
}

# Apply function to longevity column
iconic_species_traits <- iconic_species_traits %>%
  mutate(longevity = clean_longevity(longevity))

# Traits scoring and rescaling
# 100
iconic_species_traits_rescaled <- iconic_species_traits %>%
  mutate(
    # Animals
    semel_itero = ifelse(semel_itero == "semelparous", 0, 1),
    bipart_lifecycle = ifelse(bipart_lifecycle == "no", 0, 1),
    longevity_y = scales::rescale(longevity_y, to = c(1, 0)),
    annual_repro_young_per_y = scales::rescale(annual_repro_young_per_y, to = c(0, 1)),
    age_first_repro_y = scales::rescale(age_first_repro_y, to = c(1, 0)),
    asexual_repro = ifelse(asexual_repro == "yes", 1, 0),
    gills = ifelse(gills == "yes", 1, 0),
    wings = ifelse(wings == "yes", 1, 0),
    body_mass_g = scales::rescale(body_mass_g, to = c(1, 0)), 
    # Plants
    bark = ifelse(bark == "yes", 1, 0), 
    longevity = case_when(
      longevity <= 1 ~ 1,
      longevity > 1 & longevity < 5 ~ .75,
      longevity >= 5 & longevity < 10 ~ .50,
      longevity >= 10 & longevity < 20 ~ .25,
      longevity >= 20 ~ 0),
    resprout = ifelse(resprout == "yes", 1, 0),
    age_to_maturity = scales::rescale(age_to_maturity, to = c(1, 0)),
    asex = ifelse(asex == "yes", 1, 0),
    offspring_size_mg = scales::rescale(offspring_size_mg, to = c(1, 0)),
    height = scales::rescale(height, to = c(0, 1)),
    # Trees 
    avg_bark_percent = scales::rescale(avg_bark_percent, to = c(0, 1)),
    longevity_yrs = scales::rescale(longevity_yrs, to = c(1, 0)),
    # resprout is above
    seed_mass_mg = scales::rescale(seed_mass_mg, to = c(1, 0)),
    height_m = scales::rescale(height_m, to = c(0, 1)), 
    serotiny = ifelse(serotiny == "yes", 1, 0), 
    pruning = ifelse(pruning == "yes", 1, 0)
  )

#### Resistance & Recovery Traits Calculation ####

# Calculate the resistance and recovery
# 100 species - all accounted for 
spp_resistance_recovery <- iconic_species_traits_rescaled %>%
  group_by(sci_name) %>% 
  summarize(
    traits_recovery = mean(c(semel_itero, longevity_y, annual_repro_young_per_y, 
                             age_first_repro_y, body_mass_g, asexual_repro, wings, 
                             longevity, resprout, age_to_maturity, asex, offspring_size_mg,
                             longevity_yrs, seed_mass_mg, serotiny), na.rm = TRUE),  
    traits_resistance = mean(c(bipart_lifecycle, gills, wings, body_mass_g, 
                               bark, height, avg_bark_percent, height_m, pruning), 
                             na.rm = TRUE)) %>%
  dplyr::select(sci_name, traits_recovery, traits_resistance) %>%
  ungroup()

# Join scores to species-state df
state_spp_resistance_recovery_scores <- iconic_species_list %>%
  left_join(
    spp_resistance_recovery %>%
      dplyr::select(sci_name, traits_resistance, traits_recovery),
    by = c("rgbif_mol_sci_name" = "sci_name")
  )

# # join iconic_species_traits_rescaled and spp_resistance_recovery and write out rescaled resistance and recovery score to server 
# full_resistance_recovery_data <- full_join(iconic_species_traits_rescaled, state_spp_resistance_recovery_scores, by = "sci_name")
# write_csv(full_resistance_recovery_data, file.path(intermediate_data_file_path, "2024/species_trait_recovery_resistance_score.csv"))

# Add underscore to species name to match with shapefile names
state_resistance_recovery_scores <- state_spp_resistance_recovery_scores %>%
  mutate(rgbif_mol_sci_name_underscore = gsub(" ", "_", rgbif_mol_sci_name))

#### Resistance trait rasters ####
# prepare output folder
species_rasters_output_traits_resistance <- file.path(
  intermediate_data_file_path, "2024/traits_resistance/species_raster_range_1km_moll_traits_resistance_cropped_masked_rescaled")

empty_rasters <- list()

for (i in seq_len(nrow(state_resistance_recovery_scores))) {
  species_name <- state_resistance_recovery_scores$rgbif_mol_sci_name_underscore[i]
  state <- state_resistance_recovery_scores$state[i]
  traits_resistance_score <- state_resistance_recovery_scores$traits_resistance[i]
  
  if (!species_name %in% names(species_range_shapefiles)) {
    cat("No shapefile found for:", species_name, "\n")
    next
  }
  
  species_shape <- species_range_shapefiles[[species_name]]
  species_shape$traits_resistance_score <- traits_resistance_score
  
  species_raster <- terra::rasterize(
    vect(species_shape),
    study_area_1km_moll,
    field = "traits_resistance_score",
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
  raster_suffix <- if (is_empty) "_no_traits_resistance_score.tif" else "_traits_resistance_score.tif"
  
  raster_filename <- file.path(
    species_rasters_output_traits_resistance,
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

#### Resistance trait rasters - organize rasters by state to score individual states downstream ####

# Read all raster files
all_species_rasters <- list.files(species_rasters_output_traits_resistance, pattern = "\\.tif$", full.names = TRUE)

# Extract state name from filename
state_from_filename <- function(filename) {
  fname <- tools::file_path_sans_ext(basename(filename))
  sub("^[^_]+_[^_]+_(.*?)_traits.*$", "\\1", fname)
}

# Build list of rasters grouped by state
state_raster_list <- split(all_species_rasters, sapply(all_species_rasters, state_from_filename))

# Visualization PDF only: Plotting species rasters for each state 
# Assign the output PDF file path here:
output_pdf <- file.path(intermediate_data_file_path, "2024/traits_resistance/species_rasters_all_states_traits_resistance.pdf")

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


#### Resistance trait rasters - create state masked average traits resistance rasters at 1 km ####

state_traits_resisatnce_output_dir <- file.path(intermediate_data_file_path, "2024/traits_resistance/state_raster_ranges_1km_moll_masked_resistance_scored")

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
  
  output_file <- file.path(state_traits_resisatnce_output_dir, paste0("average_raster_", gsub(" ", "_", state), ".tif"))
  writeRaster(average_raster, filename = output_file, overwrite = TRUE)
  
  cat(paste("Masked average raster for", state, "written to:", output_file, "\n"))
}

# Set output PDF path
output_pdf <- file.path(intermediate_data_file_path, "2024/traits_resistance/all_state_average_trait_resistance_rasters.pdf")

# Create PDF device
pdf(output_pdf, width = 11, height = 8.5)  # Landscape

for (state in names(masked_raster_averages)) {
  avg_raster <- masked_raster_averages[[state]]
  if (!is.null(avg_raster)) {
    plot(
      avg_raster, 
      main = paste("Average Resistance Raster -", state), 
      col = rev(terrain.colors(20)), 
      axes = FALSE, 
      box = FALSE
    )
  }
}

dev.off()

cat("PDF with all state average resistance rasters saved at:", output_pdf, "\n")

#### Resistance trait rasters - bind all state species resistance rasters together for our study area ####

# Define the directory containing the state average rasters
state_species_resistance_dir <- file.path(intermediate_data_file_path, "2024/traits_resistance/state_raster_ranges_1km_moll_masked_resistance_scored")

# List all .tif raster files
raster_files <- list.files(state_species_resistance_dir, pattern = "\\.tif$", full.names = TRUE)

# Read each raster file into a list
state_resistance_rasters <- lapply(raster_files, terra::rast)

# Mosaic (merge) all the rasters together into a single raster
state_resistance_rasters_merged <- do.call(terra::mosaic, state_resistance_rasters)

crs(state_resistance_rasters_merged) <- moll_crs

plot(state_resistance_rasters_merged, main = "Traits Resistance")

# write out resistance 1km 5070 raster 
writeRaster(state_resistance_rasters_merged, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_traits_resistance_1km_moll.tif"),
            overwrite = TRUE)

#### Resistance trait rasters - rasterize and reproject to 90m and 5070 ####

# Align indicator with study_area_90m_template raster
species_traits_resistance_90m_5070_rast <- align_raster_to_template(study_area_90m_5070, state_resistance_rasters_merged, input_type = "continuous")

# Write the updated raster to a file
writeRaster(species_traits_resistance_90m_5070_rast, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_traits_resistance.tif"),
            overwrite = TRUE)

#### Recovery traits rasters ####
# prepare output folder
species_rasters_output_traits_recovery <- file.path(
  intermediate_data_file_path, "2024/traits_recovery/species_raster_range_1km_moll_traits_recovery_cropped_masked_rescaled")

empty_rasters <- list()

for (i in seq_len(nrow(state_resistance_recovery_scores))) {
  species_name <- state_resistance_recovery_scores$rgbif_mol_sci_name_underscore[i]
  state <- state_resistance_recovery_scores$state[i]
  traits_recovery_score <- state_resistance_recovery_scores$traits_recovery[i]
  
  if (!species_name %in% names(species_range_shapefiles)) {
    cat("No shapefile found for:", species_name, "\n")
    next
  }
  
  species_shape <- species_range_shapefiles[[species_name]]
  species_shape$traits_recovery_score <- traits_recovery_score
  
  species_raster <- terra::rasterize(
    vect(species_shape),
    study_area_1km_moll,
    field = "traits_recovery_score",
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
  raster_suffix <- if (is_empty) "_no_traits_recovery_score.tif" else "_traits_recovery_score.tif"
  
  raster_filename <- file.path(
    species_rasters_output_traits_recovery,
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

#### Recovery trait rasters - organize rasters by state to score individual states downstream ####

# Read all raster files
all_species_rasters <- list.files(species_rasters_output_traits_recovery, pattern = "\\.tif$", full.names = TRUE)

# Extract state name from filename
state_from_filename <- function(filename) {
  fname <- tools::file_path_sans_ext(basename(filename))
  sub("^[^_]+_[^_]+_(.*?)_traits.*$", "\\1", fname)
}

# Build list of rasters grouped by state
state_raster_list <- split(all_species_rasters, sapply(all_species_rasters, state_from_filename))

# Visualization PDF only: Plotting species rasters for each state 

output_pdf <- file.path(intermediate_data_file_path, "2024/traits_recovery/species_rasters_all_states_traits_recovery.pdf")

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


#### Recovery trait rasters - create state masked average traits recovery rasters at 1 km ####

state_traits_recovery_output_dir <- file.path(intermediate_data_file_path, "2024/traits_recovery/state_raster_ranges_1km_moll_masked_recovery_scored")

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
  
  output_file <- file.path(state_traits_recovery_output_dir, paste0("average_raster_", gsub(" ", "_", state), ".tif"))
  writeRaster(average_raster, filename = output_file, overwrite = TRUE)
  
  cat(paste("Masked average raster for", state, "written to:", output_file, "\n"))
}

# Set output PDF path
output_pdf <- file.path(intermediate_data_file_path, "2024/traits_recovery/all_state_average_trait_recovery_rasters.pdf")

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

#### Recovery trait rasters - bind all state species recovery rasters together for our study area ####

# Define the directory containing the state average rasters
state_species_recovery_dir <- file.path(intermediate_data_file_path, "2024/traits_recovery/state_raster_ranges_1km_moll_masked_recovery_scored")

# List all .tif raster files
raster_files <- list.files(state_species_recovery_dir, pattern = "\\.tif$", full.names = TRUE)

# Read each raster file into a list
state_recovery_rasters <- lapply(raster_files, terra::rast)

# Mosaic (merge) all the rasters together into a single raster
state_recovery_rasters_merged <- do.call(terra::mosaic, state_recovery_rasters)

crs(state_recovery_rasters_merged) <- moll_crs

plot(state_recovery_rasters_merged, main = "Traits Recovery")

# write out recovery 1km 5070 raster 
writeRaster(state_recovery_rasters_merged, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_traits_recovery_1km_moll.tif"),
            overwrite = TRUE)

#### Recovery trait rasters - rasterize and reproject to 90m and 5070 ####

# Align indicator with study_area_90m_template raster
species_traits_recovery_90m_5070_rast <- align_raster_to_template(study_area_90m_5070, state_recovery_rasters_merged, input_type = "continuous")

# Write the updated raster to a file
writeRaster(species_traits_recovery_90m_5070_rast, 
            file.path(final_layers_file_path, "indicators/sense_of_place_iconic_species_traits_recovery.tif"),
            overwrite = TRUE)

