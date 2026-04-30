# this script is to pull feshwater fish RGBIF points and map points onto hydrobasins level 8
# pull hyrdobasin boundaries that intersect with points
# create fish range maps from those hydrobasins level 8 

library(rgbif)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(furrr)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_species"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Boundary Layers ####

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'
hydrobasins <- file.path(multi_domain_data_file_path, "raw/boundary_layers/hydro_basins/raw/")
north_america <- st_read("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/species_range_north_america_study_area.shp")

#### Data Layers ####

iconic_species_list <- read_csv(file.path(raw_data_file_path, "iconic_species_list - Sheet1 (16).csv"))
# read in geopackage from server if you don't want to pull the new GBIF points
#species_occurrence_in_studyarea_fish <- st_read(file.path(intermediate_data_file_path, "rgbif_2024/rgbif_iconic_species_point_occurrence_in_studyarea_fish.gpkg"))

#### Load species list and find taxonKey ####

# Filter the iconic species list for only fish species that we need to get RGBIF points for
rgbif_iconic_species_list_fish <- iconic_species_list %>%
  filter(`download source` == "hydrosheds")

# Feed iconic species fish subspecies list to RGBIF to find taxon keys and matches
species_taxonkey <- name_backbone_checklist(rgbif_iconic_species_list_fish$ns_sci_name)

# Extract unique taxon keys
# 10 taxon keys, but only returns 7 species data points
taxonKeys <- unique(species_taxonkey$usageKey)

#### Batch species download RGBIF code ####

# download the data
occ_download(
  pred_in("taxonKey", taxonKeys), # important to use pred_in
  pred_in("country", c("US", "CA", "MX")),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV"
)

# Copy individual download link and check status
# Most downloads finish within 15 min.
occ_download_wait('0043879-250525065834625')

# After it finishes, use the same code to retrieve your download
fish_species_occurrence <- occ_download_get('0043879-250525065834625') %>%
  occ_download_import()

# Remove rows with missing latitude or longitude values
fish_species_occurrence <- fish_species_occurrence[!is.na(fish_species_occurrence$decimalLongitude) & !is.na(fish_species_occurrence$decimalLatitude), ]

# Ensure longitude and latitude are numeric
fish_species_occurrence$decimalLongitude <- as.numeric(fish_species_occurrence$decimalLongitude)
fish_species_occurrence$decimalLatitude <- as.numeric(fish_species_occurrence$decimalLatitude)

# Convert the cleaned data to an sf object
fish_species_occurrence_sf <- sf::st_as_sf(fish_species_occurrence, coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84")

# Reproject to Mollweide
# 312075
fish_species_occurrence_moll <- sf::st_transform(fish_species_occurrence_sf, crs = moll_crs)

# Check structure
st_crs(fish_species_occurrence_moll)  
st_geometry(fish_species_occurrence_moll)  # Should show POINT geometries

# Check and fix invalid geometries
fish_species_occurrence_moll <- st_make_valid(fish_species_occurrence_moll)

# Split data evenly for parallel processing
workers <- 8
n <- nrow(fish_species_occurrence_moll)
split_groups <- rep(1:workers, length.out = n)
split_data <- split(fish_species_occurrence_moll, split_groups)

# Parallelized intersection with North America boundary
plan(multisession, workers = workers)

# freshwater fish occurence within studyarea
# 220472
species_occurrence_in_studyarea_fish <- future_map_dfr(
  split_data, 
  ~st_intersection(.x, north_america))

# Remove the problematic "FID" column if it exists
species_occurrence_in_studyarea_fish <- species_occurrence_in_studyarea_fish %>%
  dplyr::select(-FID)

# Write the GBIF points occurrence for future use
st_write(
  species_occurrence_in_studyarea_fish,
  file.path(intermediate_data_file_path, "rgbif_2024/rgbif_iconic_species_point_occurrence_in_studyarea_fish.gpkg"),
  delete_layer = TRUE # to overwrite if needed
)

#### Split combined df of species points into seperate species df's #### 

# Split the combined dataframe into a list of smaller dataframes by species
species_dfs <- split(species_occurrence_in_studyarea_fish, species_occurrence_in_studyarea_fish$species)

# Check the names of the species
species_names <- names(species_dfs)
print(species_names)

# Save each species dataframe as a separate object or file if needed
for (species_name in species_names) {
  assign(paste0("df_", gsub(" ", "_", species_name)), species_dfs[[species_name]])
}

# Get unique species
unique_species <- unique(species_occurrence_in_studyarea_fish$species)
species_list <- unique_species

#### Plotting GBIF points ####
output_file <- file.path(intermediate_data_file_path, "rgbif_2024/rgbif_fish_species_occurrence_points.pdf")

# plot all species occurrence points 
plot_all_species_occurrences <- function() {
  # Get all unique species names
  unique_species <- unique(species_occurrence_in_studyarea_fish$species)
  
  # Open a PDF device
  pdf(output_file, width = 8, height = 6)
  
  for (species_name in unique_species) {
    message("Plotting: ", species_name)
    
    # Filter data for the selected species
    species_data <- species_occurrence_in_studyarea_fish %>%
      filter(species == species_name) %>%
      mutate(
        longitude = st_coordinates(geometry)[,1],
        latitude = st_coordinates(geometry)[,2]
      )
    
    # Generate the ggplot
    p <- ggplot() +
      geom_sf(data = north_america, fill = NA, color = "black") +  # Study area boundary
      geom_hex(
        data = species_data,
        aes(x = longitude, y = latitude, fill = after_stat(count)), # Use extracted coordinates
        bins = 100  # Adjust bins as needed
      ) +
      scale_fill_viridis_c(option = "viridis", name = "Count", direction = -1) +  # Reverse color scale
      theme_minimal() +
      labs(
        title = paste("Hexbin Plot of", species_name, "Occurrence Points"),
        x = "Longitude",
        y = "Latitude"
      )
    
    # Print plot to PDF
    print(p)
  }
  
  # Close the PDF device
  dev.off()
  
  message("All species plots saved to: ", output_file)
}

# Run the function to generate and save all plots in a single PDF
plot_all_species_occurrences()


#### Load in hydrobasin level 8 polygons ####

# List all directories matching "lev08"
lev08_dirs <- list.dirs(hydrobasins, full.names = TRUE, recursive = FALSE)
lev08_dirs <- lev08_dirs[grepl("lev08", basename(lev08_dirs))]

# List all shapefiles within the selected directories
hb_shapefile_list <- unlist(lapply(lev08_dirs, function(dir) {
  list.files(dir, pattern = "\\.shp$", full.names = TRUE)
}))

# Read and merge shapefiles
hb_lev08 <- do.call(rbind, lapply(hb_shapefile_list, st_read))

# Ensure CRS of hb_lev08 is Mollweide
hb_lev08 <- st_transform(hb_lev08, crs = moll_crs)

#### Shapefile Creation ####

# Define output directory for shapefiles and pdf
shapefile_output_dir <- file.path(intermediate_data_file_path, "species_range_maps_north_america")
output_pdf_path <- file.path(intermediate_data_file_path, "rgbif_2024/hydrobasin_fish_species_occurrence_plots.pdf")

# Function to create and save shapefiles of intersecting hydrobasins for each species
create_fish_range_shapefiles <- function(species_list, hydrobasins, shapefile_output_dir) {
  
  # Ensure the output directory exists
  if (!dir.exists(shapefile_output_dir)) {
    dir.create(shapefile_output_dir, recursive = TRUE)
  }
  
  for (species_name in species_list) {
    message("Processing shapefile for: ", species_name)
    
    # Construct the dataframe variable name dynamically
    df_name <- paste0("df_", gsub(" ", "_", species_name))
    
    # Check if the dataframe exists
    if (!exists(df_name, envir = .GlobalEnv)) {
      message("Skipping ", species_name, ": Dataframe not found.")
      next
    }
    
    # Retrieve the species dataframe
    species_df <- get(df_name, envir = .GlobalEnv)
    
    # Find hydrobasins intersecting with species points
    species_intersection <- st_intersects(hydrobasins, species_df)
    
    # Keep only hydrobasins that have at least one intersecting point
    species_intersection_filtered <- hydrobasins[lengths(species_intersection) > 0, ]
    
    if (nrow(species_intersection_filtered) == 0) {
      message("No intersecting polygons found for ", species_name)
      next
    }
    
    # Create species-specific directory
    species_dir <- file.path(shapefile_output_dir, gsub(" ", "_", species_name))
    if (!dir.exists(species_dir)) {
      dir.create(species_dir, recursive = TRUE)
    }
    
    # Define output shapefile path
    shapefile_path <- file.path(species_dir, paste0(gsub(" ", "_", species_name), ".shp"))
    
    # Write to shapefile
    st_write(species_intersection_filtered, shapefile_path, delete_dsn = TRUE)
    
    message("Shapefile saved: ", shapefile_path)
  }
}

# Run the function to generate shapefiles before rasterization
create_fish_range_shapefiles(
  species_list = species_list,
  hydrobasins = hb_lev08,
  shapefile_output_dir = shapefile_output_dir
)

# Function to generate, visualize, and save species hydrobasins plots
save_and_show_species_hydrobasins <- function(species_list, base_map, output_pdf) {
  
  pdf(output_pdf, width = 8, height = 6)
  
  for (species_name in species_list) {
    message("Processing: ", species_name)
    
    species_folder <- gsub(" ", "_", species_name)
    shapefile_path <- file.path(shapefile_output_dir, species_folder, paste0(species_folder, ".shp"))
    
    if (!file.exists(shapefile_path)) {
      message("Skipping ", species_name, ": Shapefile not found at ", shapefile_path)
      next
    }
    
    species_hydrobasins <- st_read(shapefile_path, quiet = TRUE)
    
    p <- ggplot() +
      geom_sf(data = base_map, fill = "gray90", color = "black", size = 0.2) +
      geom_sf(data = species_hydrobasins, fill = "red", color = "black", alpha = 0.5) +
      labs(title = paste("Hydrobasins for", species_name), x = "Longitude", y = "Latitude") +
      theme_minimal()
    
    print(p)
  }
  
  dev.off()
  message("PDF saved at: ", output_pdf)
}

# Run the function to save and visualize plots
save_and_show_species_hydrobasins(species_list, north_america, output_pdf_path)


