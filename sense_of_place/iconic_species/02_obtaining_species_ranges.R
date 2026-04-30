library(terra)
library(sf)
library(ggplot2)
library(natserv)
library(tibble)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Set base directories
data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species"
raw_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/raw"
intermediate_data_file_path <- "/home/shares/wwri-wildfire/data/sense_of_place/iconic_species/intermediate"
final_layers_file_path <- "/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/iconic_species"
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

#### Data Layers ####
tree_files <- file.path(raw_data_file_path, "species_range_files/E_Little_Maps _unzipped_files")
carnivora <- st_read(file.path(raw_data_file_path, "species_range_files/MDD_Carnivora/MDD_Carnivora.gpkg"))
artiodactyla <- st_read(file.path(raw_data_file_path, "species_range_files/MDD_Artiodactyla/MDD_Artiodactyla.gpkg"))
rodentia <- st_read(file.path(raw_data_file_path, "species_range_files/MDD_Rodentia/MDD_Rodentia.gpkg"))
chiroptera <- st_read(file.path(raw_data_file_path, "species_range_files/MDD_Chiroptera/MDD_Chiroptera.gpkg"))
reptiles <- st_read(file.path(raw_data_file_path, "species_range_files/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"))

# Define output directory
output_dir <- file.path(intermediate_data_file_path, "species_range_maps_north_america")

# Function to write a species shapefile to its own folder
write_species_shapefile <- function(species_data, species_name) {
  # Create a species-specific folder
  species_folder <- file.path(output_dir, species_name)
  if (!dir.exists(species_folder)) {
    dir.create(species_folder, recursive = TRUE)
  }
  
  # Write the shapefile to the species-specific folder
  st_write(species_data, file.path(species_folder, paste0(species_name, ".shp")), append = FALSE)
}


#### Pull 19 tree & flower species from Little 1999 ####

# List all shapefile paths
tree_species_filepaths <- list.files(tree_files, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

# Define tree species strings and scientific names
tree_species_string <- c("rhodmacr","cornnutt","artetrid","picesitc", "cercflor", "thujplic", "sequsemp", "sequgiga", "picepung", 
                         "pinumont", "pinupond", "pinulong", "pinumono", "pinuedul", "pseumenz", 
                         "poputrem", "tsughete", "abielasi", "popudelt")

tree_species_sciname <- c("Rhododendron macrophyllum","Cornus nuttallii","Artemisia tridentata","Picea sitchensis", "Cercidium floridum", "Thuja plicata", "Sequoia sempervirens", 
                          "Sequoiadendron giganteum", "Picea pungens", "Pinus monticola", "Pinus ponderosa", 
                          "Pinus longaeva", "Pinus monophylla", "Pinus edulis", "Pseudotsuga menziesii", 
                          "Populus tremuloides", "Tsuga heterophylla", "Abies lasiocarpa", "Populus deltoides")

# Map species string to scientific name
tree_species_map <- setNames(tree_species_sciname, tree_species_string)

# Create a named list to store filepaths by species
tree_species_files <- list()

for (species in tree_species_string) {
  # Find filepaths that match the current species
  matching_files <- tree_species_filepaths[grepl(species, basename(tree_species_filepaths), ignore.case = TRUE)]
  
  # Add to the list if there are matches
  if (length(matching_files) > 0) {
    tree_species_files[[species]] <- matching_files
  }
}

# Print species and their matched file paths
print(tree_species_files)

# Create an environment to store individual shapefiles
species_shapefiles <- new.env()

for (species in names(tree_species_files)) {
  # Create a folder for the species
  species_dir <- file.path(output_dir, species)
  if (!dir.exists(species_dir)) dir.create(species_dir)
  
  for (file in tree_species_files[[species]]) {
    # Read the shapefile
    shp <- st_read(file, quiet = TRUE)
    
    # Add the sciname column
    shp$sciname <- tree_species_map[[species]]
    
    # --- NEW: Smart CRS correction ---
    bbox <- st_bbox(shp)
    
    if (is.na(st_crs(shp))) {
      message("Shapefile ", basename(file), " has no CRS. Assigning WGS84.")
      st_crs(shp) <- 4326
    } else if (bbox$xmin > -200 & bbox$xmin < 200 & bbox$ymin > -90 & bbox$ymin < 90) {
      message("Shapefile ", basename(file), " has degree coordinates. Forcing WGS84 and transforming.")
      st_crs(shp) <- 4326
      shp <- st_transform(shp, crs = 5070)
    } else if (st_crs(shp)$epsg != 5070) {
      message("Shapefile ", basename(file), " has wrong EPSG. Transforming to 5070.")
      shp <- st_transform(shp, crs = 5070)
    }
    
    # --- End of CRS fix ---
    
    # Use the full scientific name in the file name
    scientific_name <- tree_species_map[[species]]
    scientific_name_safe <- gsub(" ", "_", scientific_name)  # Replace spaces with underscores
    
    # Create a directory for the scientific name
    species_dir <- file.path(output_dir, scientific_name_safe)
    if (!dir.exists(species_dir)) dir.create(species_dir)
    
    # Save the shapefile using the full scientific name
    output_path <- file.path(species_dir, paste0(scientific_name_safe, ".shp"))
    st_write(shp, output_path, append = FALSE, quiet = TRUE)
    
    # Store the shapefile in the environment
    assign(species, shp, envir = species_shapefiles)
  }
}

# plot all flower species to check range
plot(species_shapefiles$artetrid, main = "Range of artetrid")
plot(species_shapefiles$cornnutt, main = "Range of cornnutt")
plot(species_shapefiles$abielasi, main = "Range of rhodmacr")

# plot all tree species to check range
plot(species_shapefiles$abielasi, main = "Range of abielasi")
plot(species_shapefiles$cercflor, main = "Range of cercflor")
plot(species_shapefiles$picepung, main = "Range of picepung")
plot(species_shapefiles$picesitc, main = "Range of picesitc")
plot(species_shapefiles$pinuedul, main = "Range of pinuedul")
plot(species_shapefiles$pinulong, main = "Range of pinulong")
plot(species_shapefiles$pinumono, main = "Range of pinumono")
plot(species_shapefiles$pinumont, main = "Range of pinumont")
plot(species_shapefiles$pinupond, main = "Range of pinupond")
plot(species_shapefiles$popudelt, main = "Range of popudelt")
plot(species_shapefiles$poputrem, main = "Range of poputrem")
plot(species_shapefiles$pseumenz, main = "Range of pseumenz")
plot(species_shapefiles$sequgiga, main = "Range of sequgiga")
plot(species_shapefiles$sequsemp, main = "Range of sequsemp")
plot(species_shapefiles$thujplic, main = "Range of thujplic")
plot(species_shapefiles$tsughete, main = "Range of tsughete")


#### pull carnivora species maps ####

# Filter for Bassariscus astutus - ring tailed cat
bassariscus_astutus <- carnivora[carnivora$sciname == "Bassariscus astutus", ]

# Check if the data is correctly filtered
print(bassariscus_astutus)

# ggplot2 Plot
ggplot() +
  geom_sf(data = bassariscus_astutus, fill = "blue", color = "black") +
  ggtitle("Distribution of Bassariscus astutus") +
  theme_minimal()


# Filter for Ursus arctos - grizzly bear
ursus_arctos <- carnivora[carnivora$sciname == "Ursus arctos", ]

# Check if the data is correctly filtered
print(ursus_arctos)

# ggplot2 Plot
ggplot() +
  geom_sf(data = ursus_arctos, fill = "blue", color = "black") +
  ggtitle("Distribution of ursus_arctos") +
  theme_minimal()


# Filter for Ursus americanus - black bear 
ursus_americanus <- carnivora[carnivora$sciname == "Ursus americanus", ]

# Check if the data is correctly filtered
print(ursus_americanus)

# ggplot2 Plot
ggplot() +
  geom_sf(data = ursus_americanus, fill = "blue", color = "black") +
  ggtitle("Distribution of ursus_americanus") +
  theme_minimal()


#### pull artiodactyla species maps ####

# Filter for Bos bison 
bos_bison <- artiodactyla[artiodactyla$sciname == "Bos bison", ]

# Check if the data is correctly filtered
print(bos_bison)

# ggplot2 Plot
ggplot() +
  geom_sf(data = bos_bison, fill = "blue", color = "black") +
  ggtitle("Distribution of bos_bison") +
  theme_minimal()


# Filter for Cervus canadensis - elk
cervus_canadensis <- artiodactyla[artiodactyla$sciname == "Cervus canadensis", ]

# Check if the data is correctly filtered
print(cervus_canadensis)

# ggplot2 Plot
ggplot() +
  geom_sf(data = cervus_canadensis, fill = "blue", color = "black") +
  ggtitle("Distribution of cervus_canadensis") +
  theme_minimal()


# Filter for Alces alces - moose
alces_alces <- artiodactyla[artiodactyla$sciname == "Alces alces", ]

# Check if the data is correctly filtered
print(alces_alces)

# ggplot2 Plot
ggplot() +
  geom_sf(data = alces_alces, fill = "blue", color = "black") +
  ggtitle("Distribution of alces_alces") +
  theme_minimal()


#### pull rodentia species maps ####

# Filter for Castor canadensis - american beaver 
castor_canadensis <- rodentia[rodentia$sciname == "Castor canadensis", ]

# Check if the data is correctly filtered
print(castor_canadensis)

# ggplot2 Plot
ggplot() +
  geom_sf(data = castor_canadensis, fill = "blue", color = "black") +
  ggtitle("Distribution of castor_canadensis") +
  theme_minimal()


#### pull chiroptera species maps ####

# Filter for Antrozous pallidus - pallid bat
antrozous_pallidus <- chiroptera[chiroptera$sciname == "Antrozous pallidus", ]

# Check if the data is correctly filtered
print(antrozous_pallidus)

# ggplot2 Plot
ggplot() +
  geom_sf(data = antrozous_pallidus, fill = "blue", color = "black") +
  ggtitle("Distribution of antrozous_pallidus") +
  theme_minimal()


#### pull reptiles species maps ####

# Filter for Chrysemys picta - painted turtle 
chrysemys_picta <- reptiles[reptiles$Binomial == "Chrysemys picta", ]

# Rename the column "Binomial" to "sciname"
chrysemys_picta <- chrysemys_picta %>%
  rename(sciname = Binomial)

# Check if the data is correctly filtered
print(chrysemys_picta)

# ggplot2 Plot
ggplot() +
  geom_sf(data = chrysemys_picta, fill = "blue", color = "black") +
  ggtitle("Distribution of chrysemys_picta") +
  theme_minimal()




#### write out shapefiles of 8 species to server ####

# Write shapefiles for each species
write_species_shapefile(bassariscus_astutus, "Bassariscus_astutus")
write_species_shapefile(ursus_arctos, "Ursus_arctos")
write_species_shapefile(ursus_americanus, "Ursus_americanus")
write_species_shapefile(bos_bison, "Bos_bison")
write_species_shapefile(cervus_canadensis, "Cervus_canadensis")
write_species_shapefile(alces_alces, "Alces_alces")
write_species_shapefile(castor_canadensis, "Castor_canadensis")
write_species_shapefile(antrozous_pallidus, "Antrozous_pallidus")
write_species_shapefile(chrysemys_picta, "Chrysemys_picta")  # New species added



