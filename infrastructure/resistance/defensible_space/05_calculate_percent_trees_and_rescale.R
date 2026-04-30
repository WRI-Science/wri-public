#### Goal ####
# The goal of this script is to calculate what percent trees makeup each building
# footprint for 10 meters. This will be done by subtracting the building footprint 
# makeup from the 10m buffer data. The final output for this script will be the 
# indicator layer for defensible space resistance in the infrastructure domain.

#### Packages ####
library(tidyverse)  # Data manipulation and visualization
library(data.table) # Fast data manipulation
library(terra)
library(sf)

#### File Paths and Setup ####
year_of_interest <- 2024
# Set base directories
infrastructure_file_path <- "/home/shares/wwri-wildfire/data/infrastructure"
final_layers_file_path <- paste0("/home/shares/wwri-wildfire/final_layers/", as.character(year_of_interest), "/infrastructure/")
multi_domain_data_file_path <- "/home/shares/wwri-wildfire/data/multi_domain_data"

# Template raster path for alignment function
template_raster_path <- file.path(multi_domain_data_file_path, 
                                  "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

building_centroids_path <- file.path(infrastructure_file_path, "defensible-space/structure-polygons/building_defensible_space_polygons/zoom_7_centroids")
building_footprint_path <- file.path(infrastructure_file_path, "defensible-space/structure-landcover-counts/original")

buffer_10m_path <- file.path(infrastructure_file_path, "defensible-space/structure-landcover-counts/10m")
buffer_30m_path <- file.path(infrastructure_file_path, "defensible-space/structure-landcover-counts/30m")

# save paths
unrescaled_save_path <- file.path(infrastructure_file_path, "unrescaled_indicators/")
rescaled_save_path <- file.path(infrastructure_file_path, "rescaled_indicators/")
final_save_path <- file.path(final_layers_file_path, "indicators/infrastructure_resistance_d_space.tif")

# mask for human settlement layer
human_settlement_layer_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/int/human_settlement/human_sett_aligned.tif"

source(here::here("templates_and_functions", "align_raster_to_template.R"))

#### Main Processing ####

# read in all the building centroid csvs and combine them into one data.table
building_centroids <- list.files(building_centroids_path, pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist()

# read in all the building footprint csvs and combine them into one data.table
building_footprint <- list.files(building_footprint_path, pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist()

# Sum the total number of cells in each unique_id footprint, exclude any columns that start with "No Data"
cols_to_remove <- grep("^No Data", names(building_footprint), value = TRUE)
building_footprint[, (cols_to_remove) := NULL]

# Identify the columns to sum (exclude 'unique_id')
cols_to_sum <- setdiff(names(building_footprint), "unique_id")

# Group by 'unique_id' and sum the rest
building_footprint_summary <- building_footprint[
  , .(building_footprint_count = sum(unlist(.SD), na.rm = TRUE),
      # separately sum the Trees-2 column
      tree_footprint_count   = sum(`Trees-2`, na.rm = TRUE)
  ), 
  by      = unique_id,
  .SDcols = cols_to_sum
]

# read in all the 10m buffer csvs and combine them into one data.table
buffer_10m <- list.files(buffer_10m_path, pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist()

# Sum the total number of cells in each unique_id footprint, exclude any columns that start with "No Data"
cols_to_remove <- grep("^No Data", names(buffer_10m), value = TRUE)
buffer_10m[, (cols_to_remove) := NULL]

# Identify the columns to sum (exclude 'unique_id')
cols_to_sum <- setdiff(names(buffer_10m), "unique_id")

# Group by 'unique_id' and sum the rest
buffer_10m_summary <- buffer_10m[
  , .(
    # flatten all landcover columns (aside from Trees-2) into one sum
    buffer_10m_count = sum(unlist(.SD), na.rm = TRUE),
    # separately sum the Trees-2 column
    tree_10m_count   = sum(`Trees-2`, na.rm = TRUE)
  ),
  by      = unique_id,
  .SDcols = cols_to_sum
]

# repeat for the 30m buffer
buffer_30m <- list.files(buffer_30m_path, pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist()
# Sum the total number of cells in each unique_id footprint, exclude any columns that start with "No Data"
cols_to_remove <- grep("^No Data", names(buffer_30m), value = TRUE)
buffer_30m[, (cols_to_remove) := NULL]
# Identify the columns to sum (exclude 'unique_id')
cols_to_sum <- setdiff(names(buffer_30m), "unique_id")
# Group by 'unique_id' and sum the rest
buffer_30m_summary <- buffer_30m[
  , .(
    # flatten all landcover columns (aside from Trees-2) into one sum
    buffer_30m_count = sum(unlist(.SD), na.rm = TRUE),
    # separately sum the Trees-2 column
    tree_30m_count   = sum(`Trees-2`, na.rm = TRUE)
  ),
  by      = unique_id,
  .SDcols = cols_to_sum
]
# remove the raw files
rm(buffer_10m, buffer_30m, building_footprint)

# Join the building footprint summary with the 10m and 30m buffer summary
building_buffer_summary <- merge(
  building_footprint_summary,
  buffer_10m_summary,
  by = "unique_id",
  all.x = TRUE
)

building_buffer_summary <- merge(
  building_buffer_summary,
  buffer_30m_summary,
  by = "unique_id",
  all.x = TRUE
)
# remove the summary files
rm(buffer_10m_summary, buffer_30m_summary, building_footprint_summary)

# calculate the percent of tree coverage for each row
building_buffer_summary[, percent_trees_10m := ((tree_10m_count - tree_footprint_count) / (buffer_10m_count - building_footprint_count)) * 100]
building_buffer_summary[, percent_trees_30m := ((tree_30m_count - tree_footprint_count) / (buffer_30m_count - building_footprint_count)) * 100]

buildingpercentnan <- building_buffer_summary %>% 
  filter(is.na(percent_trees_10m) | is.infinite(percent_trees_10m) | percent_trees_10m < 0 | is.na(percent_trees_30m) | is.infinite(percent_trees_30m) | percent_trees_30m < 0)

# join NaN exploration with centroids data to see where this is occuring
buildingpercentnan <- merge(
  buildingpercentnan,
  building_centroids[, .(unique_id, centroid_lat, centroid_lon)],
  by = "unique_id",
  all.x = TRUE
)

# this NaN operation is less than 0.1% of the building data so we will just remove these building for now
# the issue only occurs with the 10m data not he 30m data
# use data.table to remove the rows with NaN values
building_buffer_summary <- building_buffer_summary[!is.na(percent_trees_10m) & !is.infinite(percent_trees_10m) & percent_trees_10m >= 0]

# divide percent_trees_10m and percent_trees_30m by 100 to convert to a proportion
building_buffer_summary[, percent_trees_10m := percent_trees_10m / 100]
building_buffer_summary[, percent_trees_30m := percent_trees_30m / 100]

# remerge with centroids
building_buffer_summary <- merge(
  building_buffer_summary,
  building_centroids[, .(unique_id, centroid_lon, centroid_lat)],
  by    = "unique_id",
  all.x = TRUE
)
rm(building_centroids)

# #### REMOVE THIS AFTER making historgram to check ####
# # define the breaks you want, e.g. 0–100 in 1‐unit bins
# building_buffer_summary_no_zeros <- building_buffer_summary[percent_trees_10m > 0]
# 
# brks <- seq(0, 1, by = 0.05)
# 
# # compute histogram object without plotting
# h <- hist(building_buffer_summary_no_zeros$percent_trees_10m,
#           breaks = brks,
#           plot   = FALSE)
# 
# # h$counts is a length‐100 integer vector,
# # h$mids the bin midpoints
# plot(h$mids, h$counts, type = "h",
#      xlab = "percent_trees_10m",
#      ylab = "frequency",
#      main = "Histogram of percent_trees_10m")
# #### Remove above

#### Raster Conversion ####

# — Read template
tpl <- rast(template_raster_path) 
# should be EPSG:5070, 90m resolution

# — Convert to sf, reproject, then to terra vector
pts_sf <- st_as_sf(
  building_buffer_summary,
  coords = c("centroid_lon", "centroid_lat"),
  crs    = 4326,                 # WGS84 assumed for lon/lat
  remove = FALSE
)

pts_sf5070 <- st_transform(pts_sf, crs(tpl))

pts_vect <- vect(pts_sf5070)     # terra SpatVector

# — Rasterize: mean of percent_trees_10m per cell, leave others NA
tree_rast <- rasterize(
  x          = pts_vect,
  y          = tpl,
  field      = "percent_trees_10m",
  fun        = max,
  background = NA,
  overwrite  = TRUE
)

names(tree_rast) <- "proportion_trees_10m"

# save the unrescaled raster
writeRaster(
  tree_rast,
  filename = file.path(unrescaled_save_path, "defensible_space_proportion_trees_10m.tif"),
  overwrite = TRUE
)

# — Rescale by making all values  greater than 50 = 50
tree_rast[tree_rast > 0.5] <- 0.5

# now rescale for rast to be between 0 and 1 and then 1 minus the current value
tree_rast <- (1 - (tree_rast / 0.5))

# rename the layer
names(tree_rast) <- "infrastructure_resistance_defensible_space"

# save to rescaled directory
writeRaster(
  tree_rast,
  filename = file.path(rescaled_save_path, "infrastructure_resistance_defensible_space_no_mask.tif"),
  overwrite = TRUE
)

# align the raster to the template
tree_rast <- align_raster_to_template(
  input_raster = tree_rast,
  template_raster = tpl,
  input_type = "continuous"
)

human_settlement_layer <- rast(human_settlement_layer_path)

# mask on status human settlement layer
tree_rast <- mask(tree_rast, human_settlement_layer)

# align the raster to the template again just incase
tree_rast <- align_raster_to_template(
  input_raster = tree_rast,
  template_raster = tpl,
  input_type = "continuous"
)

# save the final raster
writeRaster(
  tree_rast,
  filename = final_save_path,
  overwrite = TRUE
)

# library(terra)
# 
# # 1) Get a table of unique values and their counts
# f <- terra::freq(tree_rast)
# #    f is a two‐column data.frame: $value and $count
# 
# # 2) If your raster is truly continuous, you might want to group into bins:
# #    Here’s how to cut into 5 equal‐width bins:
# bins <- cut(f$value, breaks = 5, include.lowest = TRUE)
# bin_table <- aggregate(f$count, by = list(bin = bins), FUN = sum)
# 
# # 3) Make a barplot (or use ggplot2)
# barplot(bin_table$x,
#         names.arg = levels(bins),
#         main = "Full Raster Histogram (no NAs)",
#         xlab = "Resistance bins",
#         ylab = "Frequency",
#         las = 2)    # rotate labels if needed