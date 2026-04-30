wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_places", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_places")

#### Boundary layers ####
study_area_admin1_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_1.shp")) 
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 

#### Data Layers ####
# US historic structures geodatabase 
gdb_path <- file.path(raw_data_file_path, "NRIS_CR_Standards_Public.gdb")
st_layers(gdb_path)

# Load non-spatial attribute table
NR_Main <- st_read(gdb_path, layer = "NR_Main")

# Define the states of interest
states_of_interest <- c("WASHINGTON", "CALIFORNIA", "OREGON", "IDAHO", "ARIZONA", 
                        "NEVADA", "WYOMING", "COLORADO", "UTAH", "MONTANA", 
                        "NEW MEXICO", "ALASKA")

# Filter relevant states Property ID's
NR_Main_filtered <- NR_Main %>%
  filter(State %in% states_of_interest)

# Get unique Property_IDs from NR_Main_filtered and apply function to obtain relevant properties in layers
property_ids <- as.character(NR_Main_filtered$Property_ID)

# read in layers and transform to 5070 crs
filter_gdb_layer_chunked <- function(gdb_path, layer_name, property_ids, chunk_size = 500) {
  
  # Split property IDs into smaller chunks
  property_chunks <- split(property_ids, ceiling(seq_along(property_ids) / chunk_size))
  
  # Function to read a chunk
  read_chunk <- function(chunk) {
    property_ids_sql <- paste(sprintf("'%s'", chunk), collapse = ",")
    query <- sprintf("SELECT * FROM %s WHERE NR_PROPERTYID IN (%s)", layer_name, property_ids_sql)
    
    tryCatch({
      st_read(gdb_path, layer = layer_name, query = query, quiet = TRUE) %>%
        st_transform(5070) # Transform CRS to EPSG:5070
    }, error = function(e) {
      message(sprintf("Error reading chunk for layer %s: %s", layer_name, e$message))
      return(NULL)
    })
  }
  
  # Read and combine all chunks
  results <- lapply(property_chunks, read_chunk)
  results <- do.call(rbind, results) # Merge all filtered parts
  
  return(results)
}

# List of layers to filter
layers_to_filter <- c("crbldg_py", "crbldg_pt", 
                      "crsite_pt", "crsite_py",
                      "crdist_pt", "crdist_py", 
                      "crobj_pt", "crobj_py", 
                      "crstru_pt", "crstru_py") 

# Apply function to each layer with chunking
filtered_layers <- lapply(layers_to_filter, function(layer) {
  filter_gdb_layer_chunked(gdb_path, layer, property_ids, chunk_size = 500)
})

# Convert list to named list for easy access
names(filtered_layers) <- layers_to_filter

crbldg_points_filtered <- filtered_layers[["crbldg_pt"]] # 10925
crbldg_polygons_filtered <- filtered_layers[["crbldg_py"]] # 344
crsite_points_filtered <- filtered_layers[["crsite_pt"]] #310
crsite_polygons_filtered <- filtered_layers[["crsite_py"]] # 214 
crdist_points_filtered <- filtered_layers[["crdist_pt"]] # 480
crdist_polygons_filtered <- filtered_layers[["crdist_py"]] # 1603
crobj_points_filtered <- filtered_layers[["crobj_pt"]] #56
crobj_polygons_filtered <- filtered_layers[["crobj_py"]] # 1 
crstru_points_filtered <- filtered_layers[["crstru_pt"]] #871
crstru_polygons_filtered <- filtered_layers[["crstru_py"]] # 136

# join 5070 filtered US Property ID spatial layers 
# 14940 entries 
nris_spatial_points_5070_wwri_states <- bind_rows(crbldg_points_filtered, crbldg_polygons_filtered,
                                             crsite_points_filtered, crsite_polygons_filtered, 
                                             crdist_points_filtered, crdist_polygons_filtered,
                                             crobj_points_filtered, crobj_polygons_filtered,
                                             crstru_points_filtered, crstru_polygons_filtered)

# Get geometry types and counts
geometry_counts <- nris_spatial_points_5070_wwri_states %>%
  st_geometry_type() %>%
  table()

print(geometry_counts)
# have to remove the multisurface point because it causes errors 
# POINT 12642     
# MULTIPOLYGON 2297             
# MULTISURFACE 1                  

# Remove MULTISURFACE geometry
# 14939
nris_spatial_points_5070_wwri_states_clean <- nris_spatial_points_5070_wwri_states %>%
  filter(st_geometry_type(.) != "MULTISURFACE")

# Now run the spatial intersection to remove points outside of study area 
# 14721
us_nris_spatial_points_5070 <- st_intersection(nris_spatial_points_5070_wwri_states_clean, study_area_admin0_shape_5070)

# # inspect points that fall outside the study area boundary
# # rows in the original that did NOT make it into the intersection
# # the IDs you kept
# kept_ids <- us_nris_spatial_points_5070$NR_PROPERTYID
# # filter the original sf to those NOT in that vector
# points_removed <- nris_spatial_points_5070_wwri_states_clean %>%
#   filter(!NR_PROPERTYID %in% kept_ids)
# # plot on study area to check
# ggplot() +
#   # Study area boundary
#   geom_sf(data = study_area_admin0_shape_5070, fill = NA, color = "gray30") +
#   # Points in red
#   geom_sf(data = points_removed, color = "red", size = 0.1) +
#   # Legend
#   theme_minimal() +
#   labs(title = "Points Outside of Study Area Boundary (in Red)")

# Separate NRIS geometries by type
nris_polygons <- us_nris_spatial_points_5070 %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
nris_points <- us_nris_spatial_points_5070 %>%
  filter(st_geometry_type(.) == "POINT")

# visualize spatial data on study area to check 
ggplot() +
  # Study area boundary
  geom_sf(data = study_area_admin0_shape_5070, fill = NA, color = "gray30") +
  # Polygons filled orange
  geom_sf(data = nris_polygons, fill = "darkblue", alpha = 0.8) +
  # Points in blue
  geom_sf(data = nris_points, color = "darkorange", size = 0.1) +
  # Legend
  theme_minimal() +
  labs(title = "US Historic Places (Points in Blue, Polygons in Orange)")

# 14828
us_nris_spatial_points_5070_wwri_states_full <- left_join(us_nris_spatial_points_5070, NR_Main_filtered, by = c("NR_PROPERTYID" = "Property_ID"),
                                                          relationship = "many-to-many")

# Inspect duplicate Property IDs
# duplicates occur due to new creation date of point
# 603
duplicates <- us_nris_spatial_points_5070_wwri_states_full %>%
  group_by(NR_PROPERTYID) %>%
  filter(n() > 1) %>%
  ungroup()

# remove duplicates based on most recent Create Date
# 14705
us_nris_spatial_points_5070_wwri_states_full_no_duplicates <- us_nris_spatial_points_5070_wwri_states_full %>%
  group_by(NR_PROPERTYID) %>%
  filter(CREATEDATE == max(CREATEDATE, na.rm = TRUE)) %>%
  ungroup()

# remove unnecessary cols to write out shapefile
us_historic <- us_nris_spatial_points_5070_wwri_states_full_no_duplicates %>%
  dplyr::select(
    RESNAME, NR_PROPERTYID, Property_Name,
    City_us = City, County, State_us = State,
    geometry = SHAPE)

# write out US historic structures layer
st_write(us_historic, file.path(intermediate_data_file_path, "us_nrhp_spatial_points_5070.gpkg"), append = F)

#### BC historic structures ####

# Read in shapefiles 
bc_historic <- st_read(file.path(raw_data_file_path, "BCGW_02001F02_1740507768804_9112/HIST_HISTORIC_ENVIRONMNT_PA_SV/HISTENVPA_polygon.shp")) %>% 
  st_transform(5070) %>% 
  st_make_valid() 

# Intersect with wwri boundary  
bc_historic <- st_intersection(bc_historic, study_area_admin0_shape_5070)

# Plot
ggplot() +
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "gray30") +
  geom_sf(data = bc_historic, color = "red", size = 1)

# Remove unnecessary cols to write out shapefile
bc_historic <- bc_historic %>%
  dplyr::select(
    BRDNNMBR, CMMN_ST_NM, OBJECTID,
    City_bc = CITY, Province_bc = PROVINCE,
    SITE_ID, 
    RCGNTN_GVL, # include govt level protection col for recovery
    geometry) 

# Write the shapefile 
st_write(bc_historic, file.path(intermediate_data_file_path, "bc_historic_places_5070.gpkg"), append = F)

#### YT historic structures ####
yt_historic <- st_read(file.path(raw_data_file_path, "YT_Designated_Historic_Sites.shp/Designated_Historic_Sites.shp")) %>% 
  st_transform(5070) %>% 
  st_make_valid() 

# Intersect with wwri boundary  
yt_historic <- st_intersection(yt_historic, study_area_admin0_shape_5070)

# Remove unnecessary cols to write out shapefile
yt_historic <- yt_historic %>%
  dplyr::select(
    ID, YHSI_ID, SITE_TYPE, SITE_NAME, geometry) 

# Plot
ggplot() +
  # Study area boundary
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "gray30") +
  geom_sf(data = yt_historic, color = "red", size = 0.2)

# Write the 5070 shapefile 
st_write(yt_historic, file.path(intermediate_data_file_path, "yt_historic_places_5070.gpkg"), append = F)

#### visualize all the data ####

# Separate geometry types
us_polygons <- us_historic %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

us_points <- us_historic %>%
  filter(st_geometry_type(.) == "POINT")

bc_polygons <- bc_historic %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

# Plot
ggplot() +
  # Study area boundary
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "gray30") +
  
  # Polygons filled orange
  geom_sf(data = us_polygons, fill = "blue", color = "blue", alpha = 1) +
  geom_sf(data = bc_polygons, fill = "blue", color = "blue", alpha = 1) +
  
  # Points in different colors
  geom_sf(data = us_points, aes(color = "US"), size = 0.03) +
  geom_sf(data = yt_historic, aes(color = "YT"), size = 0.1) +
  
  # Legend
  scale_color_manual(values = c("US" = "green", "BC" = "red", "YT" = "red")) +
  theme_minimal() +
  labs(color = "Region", title = "Historic Places Projected onto Study Area (Polygons in Orange)")

# Plot only polygons
ggplot() +
  # Study area boundary
  geom_sf(data = study_area_admin1_shape_5070, fill = NA, color = "gray30") +
  
  # Polygons filled blue
  geom_sf(data = us_polygons, fill = "blue", color = "blue", alpha = 1) +
  geom_sf(data = bc_polygons, fill = "blue", color = "blue", alpha = 1) +
  
  # Theme and labels
  theme_minimal() +
  labs(title = "Historic Places Projected onto Study Area (Polygons Only)")

#### Combine all historic spatial places into one sf object ####
# 20096
wwri_historic_structures <- bind_rows(
  us_historic,
  bc_historic,
  yt_historic
)

# Check geometry types
geometry_types <- st_geometry_type(wwri_historic_structures)
table(geometry_types)

# Separate points and polygons from wwri_historic_structures
wwri_iconic_places_points <- wwri_historic_structures %>% filter(st_geometry_type(.) %in% c("POINT")) %>%
  st_make_valid() #12512
wwri_iconic_places_polygons <- wwri_historic_structures %>% filter(st_geometry_type(.) %in% c("MULTIPOLYGON", "POLYGON"))%>%
  st_make_valid() #7435+149

# Buffer the points with a 60m buffer which covers approximately two cells
wwri_historic_structures_buffered <- st_buffer(wwri_iconic_places_points, dist = 60)

# Combine buffered points with polygons 
wwri_iconic_structures_combined_buffer <- bind_rows(wwri_historic_structures_buffered, wwri_iconic_places_polygons)

# Write out the combined historic structures into a geopackage for use in future scripts
st_write(wwri_iconic_structures_combined_buffer, file.path(intermediate_data_file_path, "wwri_historic_structures_buffered.gpkg"), append = F)


