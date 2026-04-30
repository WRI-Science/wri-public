wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf) # For working with spatial (vector/shapefile) data
library(tidyverse) # For data manipulation
library(here) # For sourcing the functions script cleanly later
library(tictoc) # For timing the code execution
library(rnaturalearth) # For getting the world map
library(rnaturalearthdata) # For getting the world map data
sf::sf_use_s2(FALSE) # Turn it off entirely in case shapes are invalid with it on


#### Script Overview ####
# This script unions each species range, ensures the unioned range is valid, and calculate range area by species. This is done in Mollweide to ensure equal area is used in the calculation.


#### Base Directories ####
data_file_path <- file.path(wri_project_root, "data", "biodiversity")
path_year <- "2024"
int_data_file_path <- file.path(data_file_path, "int", path_year)
output_path_prev <- file.path(int_data_file_path, "species_dfs_status") # output_path from previous steps of status processing
output_path <- file.path(int_data_file_path, "species_dfs_resilience") # output_path for resilience files


#### Data Layers ####
# List all .gpkg (intersected ranges) files for resilience
gpkg_files <- list.files(output_path, pattern = "\\_resilience.gpkg$", full.names = TRUE)

# Read and combine them
all_spp_ranges <- purrr::map_dfr(gpkg_files, sf::st_read)


#### Functions ####
source(here("biodiversity", "00_biodiversity_custom_functions.R")) # For the valid_check function to check and fix invalid geometries


#### Data Processing ####
# Note: This is the reason I need to do this later on (this was before limiting to only North America and not global, but I get similar message after limiting to North America):
# > all_spp_ranges_unioned_by_spp <- all_spp_ranges %>%
#   +   group_by(id_no) %>%
#   +   summarize(geometry = st_union(geometry))
# Error in `summarize()`:
#   ℹ In argument: `geometry = st_union(geometry)`.
# ℹ In group 21: `id_no = 3745`.
# Caused by error in `wk_handle.wk_wkb()`:
#   ! Loop 0 is not valid: Edge 2322259 has duplicate vertex with edge 2322262


# Get unique species IDs for resilience (status minus EX/EW/EX in study area species)
unq_spp_ids <- unique(all_spp_ranges$id_no)
length(unq_spp_ids) # 1390

# Initiate ID counter for progress tracking
id_counter <- 1

# Iterate through each ID and union the geometries
for (id in unq_spp_ids) {
  message("starting species ", id_counter, " of ", length(unq_spp_ids))
  
  # Prepare ID counter for next iteration
  id_counter <- id_counter + 1
  
  # Filter to one species
  spp_df <- all_spp_ranges %>%
    dplyr::filter(id_no == id) %>%
    select(id_no, geom) # Keep just the geometry
  
  message(id, " going into st_union() and valid check")
  tryCatch({
    # Try unioning then valid checking to reduce the amount that needs to be checked
    spp_df_processed <- spp_df %>%
      group_by(id_no) %>% # Even though it's only one ID, ensure this is grouped correctly; can maybe take out?
      summarize(geom = st_union(geom)) %>% # Union the geometries for the species
      valid_check(., output_path) # Make sure your variable names are spp_df and output_path, or manually set
    
    assign("spp_df_processed", spp_df_processed, envir = globalenv()) # Need to do this to make sure we have the correct species or it could keep information from previous iteration
    
  }, error = function(err) {
    # if the union doesn't work before the valid check: 
    message("error occurred in ", id, ": ", conditionMessage(err))
    message("trying alt method...")
    
    # Do the validity check first to try to fix the error
    spp_df_processed <- spp_df %>%
      valid_check(., output_path) %>% # Make sure your variable names are spp_df and output_path, or manually set
      group_by(id_no) %>% # Even though it's only one ID, ensure this is grouped correctly; can maybe take out?
      summarize(geom = st_union(geom)) # Union the geometries for the species
    
    assign("spp_df_processed", spp_df_processed, envir = globalenv()) # Need to do this to make sure we have the correct species or it could keep information from previous iteration
  })
  
  # Ensure there are no duplicate geometries from the union
  spp_df_processed_simple <- spp_df_processed %>%
    distinct() 
  
  # Do a final check to make sure the unioned geometry is still valid
  valid_or_not <- st_is_valid(spp_df_processed_simple) # Check if the unioned geometry is valid
  
  if(valid_or_not == TRUE) { # If the unioned geometry is valid
    message("unioned geometry still valid!")
  } else { # If the unioned geometry is not valid
    message("unioned geometry not valid!")
    write.table(id, file.path(output_path, "final_unioned_geometry_not_valid.csv"), append = TRUE, row.names = FALSE, col.names = "id_no") # Will need to run these through valid check again or manually fix, and check if it's on the area_not_valid list, which would mean it were never valid anyway. If they're all valid after unioning, then that makes me think possibly the invalidness would be coming from looking at overlapping geometries in different rows? But st_is_valid is supposed to look at the individual rows, so maybe not.
  }
  
  # This output included multipolygons and polygons which gpkg doesn't really support but allows. To create a conformant GeoPackage, if using ogr2ogr, the -nlt option can be used to override the layer geometry type.
  tryCatch({
    # Write out spp id, spp name, and habitat type info
    st_write(spp_df_processed_simple, file.path(output_path, "ranges_made_valid_and_unioned.gpkg"), append = TRUE)
  }, error = function(err3) { # If writing out fails, which it shouldn't but just in case
    message("Error occurred during write out: ", conditionMessage(err3))
    st_write(spp_df_processed, file.path(output_path, "ranges_made_valid_and_unioned_problems.gpkg"), append = TRUE)
  })
}

# Read in the processed/unioned species
ranges_made_valid_and_unioned <- st_read(file.path(output_path, "ranges_made_valid_and_unioned.gpkg")) # 1390 IDs, which is correct
# ranges_made_valid_and_unioned_problems <- st_read(file.path(output_path, "ranges_made_valid_and_unioned_problems.gpkg")) # 0 ids, which is correct; make sure to run this, but it is commented out because it causes the code running in background/all at once to halt if there are 0 

# Convert geometrycollections to multipolygons for ease of working with in the calculations
geomcollecs <- ranges_made_valid_and_unioned %>%
  filter(st_geometry_type(.) == "GEOMETRYCOLLECTION") %>% # Get the geometrycollections
  st_collection_extract(., "POLYGON") %>% # There are linestrings in there too, but since they are disconnected lines we will ignore for these purposes
  group_by(id_no) %>% # Group by species ID
  summarize(geom = st_union(geom)) # Ensure all polygons are combined that can be

# Put the dataset back together
ranges_made_valid_and_unioned <- ranges_made_valid_and_unioned %>%
  filter(!st_geometry_type(.) == "GEOMETRYCOLLECTION") %>% # Filter out the geometrycollections
  rbind(geomcollecs) # Add in the polygon verions

# Check to make sure there are no empty geometries
ranges_made_valid_and_unioned_empties <- ranges_made_valid_and_unioned %>% filter(st_is_empty(geom)) # None!

# Cast multipolygons to polygons for use in calculations
ranges_made_valid_and_unioned_poly <- ranges_made_valid_and_unioned %>%
  st_cast(., "MULTIPOLYGON") %>% # Homogenize the geometry types (so this would make polygons into multipolygons and then back to polygons -- casting to polygon doesn't work correctly otherwise: https://github.com/r-spatial/sf/issues/763)
  st_cast(., "POLYGON") # Cast to polygons for easier calculations later on


# Calculate landscape metric of species range size
ranges_with_calcs <- ranges_made_valid_and_unioned_poly %>%
  group_by(id_no) %>% # Group by species ID
  summarize(geom_area = sum(st_area(geom))) %>% # Calculate total area by species
  mutate(geom_area_rescaled = scales::rescale(as.numeric(geom_area), to = c(0, 1))) %>% # Rescale to be between 0 and 1, with larger area being better
  st_drop_geometry() # Drop the geometry column since we used it for the calculation already

# Write out the area values
write_csv(ranges_with_calcs, file.path(output_path, "spp_range_sizes.csv"))



# Optional code if needed for checking things look correct with the ranges
# # Plot some ranges to check if patches counted seems right
# # Get background map to see the context
# world <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   st_transform(., "epsg:5070")
# 
# # Select a species to check
# species_range_parts <- ranges_made_valid_and_unioned_poly %>% filter(id_no == 22696453) # 41687 is good to check too
# 
# # Add an id for each part to distinguish easier
# species_range_parts$color <- 1:nrow(species_range_parts)
# 
# # Make the plot
# ggplot() +
#   geom_sf(data = world, fill = "lightgrey", color = "white") +
#   geom_sf(data = species_range_parts, aes(fill = as.factor(color)), alpha = 0.6) +
#   coord_sf(xlim = st_bbox(species_range_parts)[c("xmin", "xmax")],
#            ylim = st_bbox(species_range_parts)[c("ymin", "ymax")]) +
#   scale_fill_viridis_d() +
#   theme_minimal() +
#   theme(legend.position = "none") # Optional: If too many polygons and the legend obscures the map
#   
# # Check into some potentially odd stuff going on with st_make_valid()
# # None were made valid in the current version of the code (all were already valid at this stage)
# invalid_species_range <- all_spp_ranges %>%
#   filter(id_no == 181488862) # 41687 had issues in the past
# 
# valid_species_range <- ranges_made_valid_and_unioned %>%
#   filter(id_no == 181488862)
# 
# plot(invalid_species_range$geom, col = "red")
# plot(valid_species_range$geom, col = "green")
# # No issues right now for this species