wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(rgbif)
library(usethis)
library(httr)
library(curl)
library(furrr)
library(sf)
library(ggplot2)
library(tictoc)
library(dplyr)
library(callr)
library(readr)
library(grDevices)
library(gridExtra)
library(terra)
library(concaveman)
library(rnaturalearth)
library(rnaturalearthdata)
#usethis::edit_r_environ() - Add your GBIF username and password to your .Renviron

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

#### Boundary layers ####

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

# Get the basemap for North America
north_america <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "species_range_north_america_study_area.shp"))

#### Data Layers ####

iconic_species_list <- read_csv(file.path(raw_data_file_path, "iconic_species_list - Sheet1 (16).csv"))

# read in geopackage from server if you don't want to pull the new GBIF points
#species_occurrence_na_sf_no_fish <- st_read(file.path(intermediate_data_file_path, "rgbif_2024/rgbif_iconic_species_point_occurrence_in_studyarea_no_fish.gpkg"))

#### Load species list and find RGBIF taxonKey ####

# Filter the iconic species list for only species that we need to get RGBIF points for
rgbif_iconic_species_list_no_fish <- iconic_species_list %>%
  filter(`download source` == "concave hull")

# Feed iconic species list to RGBIF to find taxon keys and matches
species_taxonkey <- name_backbone_checklist(rgbif_iconic_species_list_no_fish$rgbif_mol_sci_name)

# Extract unique taxon keys
taxonKeys <- unique(species_taxonkey$usageKey)

#### Batch species point download from RGBIF ####

# Download the points for all of North America
occ_download(
  pred_in("taxonKey", taxonKeys), # important to use pred_in
  pred_in("country", c("US", "CA", "MX")),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV"
)

# Copy individual download link and check status
# Most downloads finish within 15 min.
occ_download_wait('0039787-250525065834625')

# After it finishes, use the same code to retrieve your download
species_occurrence <- occ_download_get('0039787-250525065834625') %>%
  occ_download_import()

# Remove rows with missing latitude or longitude values
# 628008
species_occurrence <- species_occurrence[!is.na(species_occurrence$decimalLongitude) & !is.na(species_occurrence$decimalLatitude), ]

# Ensure longitude and latitude are numeric
species_occurrence$decimalLongitude <- as.numeric(species_occurrence$decimalLongitude)
species_occurrence$decimalLatitude <- as.numeric(species_occurrence$decimalLatitude)

# Convert the cleaned data to an sf object and convert the CRS to WGS84
species_occurrence_sf <- sf::st_as_sf(species_occurrence, coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84")

# Reproject to Mollweide
species_occurrence_sf <- sf::st_transform(species_occurrence_sf, crs = moll_crs)

# Check CRS and structure
st_crs(species_occurrence_sf)  
st_geometry(species_occurrence_sf)  # Should show POINT geometries

# Fix validity
species_occurrence_sf <- st_make_valid(species_occurrence_sf)

plan(multisession, workers = 8)  # Adjust workers to available cores

# Parallelized intersection
# 610375
split_data <- split(species_occurrence_sf, 1:8)  # match workers
species_occurrence_na_sf_no_fish <- future_map_dfr(split_data, ~st_intersection(.x, north_america))

# Remove the problematic "FID" column if it exists
species_occurrence_na_sf_no_fish <- species_occurrence_na_sf_no_fish %>%
  dplyr::select(-FID)

# Write the GBIF points occurrence for future use
st_write(
  species_occurrence_na_sf_no_fish,
  file.path(intermediate_data_file_path, "rgbif_2024/rgbif_iconic_species_point_occurrence_in_studyarea_no_fish.gpkg"),
  delete_layer = TRUE # to overwrite if needed
)

#### Split combined df of species points into seperate species df's #### 

# Split the combined dataframe into a list of smaller dataframes by species
species_dfs <- split(species_occurrence_na_sf_no_fish, species_occurrence_na_sf_no_fish$species)

# Check the names of the species
species_names <- names(species_dfs) # 26 species
print(species_names)

# Save each species dataframe as a separate object or file if needed
for (species_name in species_names) {
  assign(paste0("df_", gsub(" ", "_", species_name)), species_dfs[[species_name]])
}

# Get unique species
unique_species <- unique(species_occurrence_na_sf_no_fish$species)

#### Plotting GBIF points ####

# Function to plot and save species occurrence points on a North America basemap in pdf form
plot_all_species_occurrences_na_pdf <- function(data, basemap, output_file) {
  # Get all unique species names
  unique_species <- unique(data$species)
  
  # Open a PDF device
  pdf(output_file, width = 8, height = 6)
  
  for (species_name in unique_species) {
    message("Plotting: ", species_name)
    
    # Filter data for the selected species
    species_data <- data %>%
      filter(species == species_name) %>%
      mutate(
        longitude = st_coordinates(geometry)[,1],
        latitude = st_coordinates(geometry)[,2]
      )
    
    # Generate the ggplot
    p <- ggplot() +
      geom_sf(data = basemap, fill = NA, color = "black") +
      geom_hex(
        data = species_data,
        aes(x = longitude, y = latitude),
        bins = 30,
        alpha = 0.7
      ) +
      scale_fill_viridis_c(option = "inferno", name = "Count") +
      coord_sf(
        xlim = st_bbox(basemap)[c("xmin", "xmax")],
        ylim = st_bbox(basemap)[c("ymin", "ymax")]
      ) +
      labs(
        title = paste("Occurrences of", species_name),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal()
    
    # Print plot to PDF
    print(p)
  }
  
  # Close PDF device
  dev.off()
  
  message("All species plots saved to PDF.")
}

# Usage Example
plot_all_species_occurrences_na_pdf(
  data = species_occurrence_na_sf_no_fish,
  basemap = north_america,
  output_file = file.path(intermediate_data_file_path, "rgbif_2024/rgbif_species_occurrence_plots.pdf")
)

#### Mahalanobis 95% Chisq Outlier Removal, Concave Hull concavity = 2, shapefile creation, pdf creation ####

# Define output directories
shapefile_output_dir <- file.path(intermediate_data_file_path, "species_range_maps_north_america")
output_dir <- file.path(intermediate_data_file_path, "rgbif_2024")

# Function to apply Mahalanobis distance filtering with 95% chisq
remove_outliers_mahalanobis_chisq <- function(df, alpha = 0.05) {
  if (nrow(df) < 3) return(df)
  coords <- st_coordinates(df$geometry)
  cov_matrix <- cov(coords)
  mean_vals <- colMeans(coords)
  mahal_dist <- mahalanobis(coords, center = mean_vals, cov = cov_matrix)
  cutoff <- qchisq(1 - alpha, df = ncol(coords))  # 95% cutoff if alpha=0.05
  df[mahal_dist <= cutoff, ]  # keep only "inliers"
  
}

# Function to compute concave hull
compute_concave_hull <- function(df, concavity, study_area = NULL) {
  if (nrow(df) < 3) return(NULL)  # At least 3 points required for a hull
  hull <- concaveman(df, concavity = concavity)
  hull_sf <- st_as_sf(hull)
  if (!is.null(study_area)) {
    # Mask the hull to your North America boundary
    hull_sf <- st_intersection(hull_sf, study_area)
  }
  return(hull_sf)
}


# Function to save concave hull as a shapefile
save_concave_hull_shapefile <- function(species_name, concave_hull) {
  if (is.null(concave_hull)) {
    message("Skipping shapefile saving for ", species_name, ": concave hull is NULL.")
    return(NULL)
  }
  
  # Create folder name with underscores
  folder_name <- gsub(" ", "_", species_name)
  species_folder <- file.path(shapefile_output_dir, folder_name)
  
  # Create directory if it doesn't exist
  if (!dir.exists(species_folder)) {
    dir.create(species_folder, recursive = TRUE)
  }
  
  # Save the shapefile inside the species folder
  shapefile_path <- file.path(species_folder, paste0(folder_name, ".shp"))
  
  st_write(concave_hull, shapefile_path, delete_layer = TRUE, quiet = TRUE)
  message("Shapefile saved: ", shapefile_path)
}


# General function to process species occurrences, remove outliers, compute concave hulls, and plot
plot_filtered_species_occurrences <- function(filter_function, method_name) {
  pdf_path <- file.path(output_dir, paste0("species_occurrence_", method_name, "_chisq_concave_hulls.pdf"))
  pdf(pdf_path, width = 8, height = 6)
  
  unique_species <- unique(species_occurrence_na_sf_no_fish$species)
  
  for (species_name in unique_species) {
    message("Processing: ", species_name, " using ", method_name)
    
    # Filter data for the selected species
    species_data <- species_occurrence_na_sf_no_fish %>%
      filter(species == species_name)
    
    # Apply outlier removal method
    species_filtered <- filter_function(species_data)
    
    # If no points remain after filtering, skip this species
    if (nrow(species_filtered) == 0) {
      message("Skipping ", species_name, ": No points remain after filtering.")
      next
    }
    
    # Extract coordinates
    species_filtered <- species_filtered %>%
      mutate(
        longitude = st_coordinates(geometry)[,1],
        latitude = st_coordinates(geometry)[,2]
      )
    
    # Compute concave hull and MASK to north_america boundary
    concave_hull_2 <- compute_concave_hull(species_filtered, concavity = 2, study_area = north_america)
    
    # *** Save Concave Hull as Shapefile ***
    save_concave_hull_shapefile(species_name, concave_hull_2)
    
    # Generate the plot
    p <- ggplot() +
      geom_sf(data = north_america, fill = NA, color = "black") +  # Study area boundary
      geom_hex(
        data = species_filtered,
        aes(x = longitude, y = latitude, fill = after_stat(count)), 
        bins = 100
      ) +
      scale_fill_viridis_c(option = "viridis", name = "Count", direction = -1) +
      theme_minimal() +
      labs(
        title = paste("Hexbin Plot of", species_name, "(", method_name, "2 SD)"),
        x = "Longitude",
        y = "Latitude"
      )
    
    # Add concave hulls if they exist
    if (!is.null(concave_hull_2)) {
      p <- p + geom_sf(data = concave_hull_2, color = "blue", fill = NA, linewidth = 0.5, linetype = "solid")
    }
    
    print(p)
  }
  
  dev.off()
  message("All species plots saved for method: ", method_name, " -> ", pdf_path)
}

# Run for Mahalanobis outlier detection (this now includes shapefile saving)
plot_filtered_species_occurrences(remove_outliers_mahalanobis_chisq, "mahalanobis")

