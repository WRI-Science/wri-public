wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(terra)
library(spatstat)
library(gridExtra)

#### Base directories ####
# MAKE SURE TO CHANGE DOMAIN PATH NAME ACCORDINGLY
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")
data_file_path <- file.path(wri_project_root, "data", "air_quality")
raw_data_file_path <- file.path(wri_project_root, "data", "air_quality", "raw", "hospital_locations")
intermediate_data_file_path <- file.path(wri_project_root, "data", "air_quality", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "air_quality")

#### Boundary layers ####
study_area_admin0_shape_moll <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0_moll.shp")) 
study_area_admin0_shape_5070 <- st_read(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")) 
study_area_90m_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))
study_area_1km_moll <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_moll.tif"))
study_area_1km_5070 <- rast(file.path(multi_domain_data_file_path, "int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0.tif"))

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

# Filter for only study area states 
west_states_abb = c("AK", "CA", "NV", "WY", "OR", "WA", "ID", "UT", "NM", "AZ", "CO", "MT")

#### Functions ####
source(here("templates_and_functions", "align_raster_to_template.R"))

#### Data Layers ####
us_hospital_shape <- st_read(file.path(raw_data_file_path, "2024/Hospitals_gdb_-6027864529607094666")) %>% 
  st_transform(moll_crs) %>% 
  filter(STATE %in% west_states_abb)

bc_hospital_shape <- st_read(file.path(raw_data_file_path, "2024/BCGW_02001F02_1748547224138_2772/GSR_HOSPITALS_SVW")) %>% 
  st_transform(moll_crs) 

yt_hospital_shape <- st_read(file.path(raw_data_file_path, "2023/Yukon_Health_Care_Facilities_50k")) %>% 
  st_transform(moll_crs)

#### visualize hospital points ####
ggplot() +
  geom_sf(data = study_area_admin0_shape_moll) +
  geom_sf(data = us_hospital_shape, color = "red", size = 1) +
  geom_sf(data = yt_hospital_shape, color = "blue", size = 1) +
  geom_sf(data = bc_hospital_shape, color = "green", size = 1)


#### Join BC, YT, US Hospital Shapefiles ####
all_hospitals_moll <- bind_rows(us_hospital_shape, bc_hospital_shape, yt_hospital_shape)

#### Kernel Density Estimate on hospital locations w. 1km moll study area raster ####

# Extract raster info
r_ext <- ext(study_area_1km_moll)
xmin <- r_ext[1]; xmax <- r_ext[2]; ymin <- r_ext[3]; ymax <- r_ext[4]
ncol_r <- ncol(study_area_1km_moll)
nrow_r <- nrow(study_area_1km_moll)

# Window for spatstat
window <- owin(xrange = c(xmin, xmax), yrange = c(ymin, ymax))

# Unique points
coords_unique <- unique(st_coordinates(all_hospitals_moll))
pp <- ppp(coords_unique[,1], coords_unique[,2], window = window)

# KDE
bw <- bw.ppl(pp)
dens <- density.ppp(pp, sigma = bw, dimyx = c(nrow_r, ncol_r), positive = TRUE)

# Convert to terra raster
dens_raster <- rast(dens)
crs(dens_raster) <- crs(study_area_1km_moll)
ext(dens_raster) <- ext(study_area_1km_moll)

plot(dens_raster)

# Mask to study area raster (if NA outside area)
density_rast_masked <- mask(dens_raster, study_area_1km_moll)

# Convert to hospitals per km² if needed
density_per_km2 <- density_rast_masked * 1e6

# Plot
tm_shape(density_per_km2) +
  tm_raster(style = "cont", palette = "-RdYlBu", title = "Hospital Density (km²)", alpha = 0.7) +
  tm_shape(study_area_admin0_shape_moll) + tm_borders(lwd = 0.09, col = "grey") +
  tm_shape(all_hospitals_moll) + tm_dots(size = 0.001, col = "black") +
  tm_layout(
    title = "Hospital Density (km²)",
    title.position = c("left", "top"),
    legend.outside = FALSE,
    bg.color = "white"
  )

# Rescale
min_val <- global(density_per_km2, "min", na.rm = TRUE)[[1]]
max_val <- global(density_per_km2, "max", na.rm = TRUE)[[1]]
density_0_1 <- (density_per_km2 - min_val) / (max_val - min_val)

# Reproject to EPSG:5070 
density_0_1_5070 <- project(
  density_0_1, 
  "EPSG:5070", 
  method = "bilinear",
  #res = 1000
)

# Mask with study area shapefile in 5070
density_0_1_5070_masked <- mask(density_0_1_5070, vect(study_area_admin0_shape_5070))

plot(density_0_1_5070_masked)

#### Rasterize indicator and resample the density raster to study area 90m raster ####

# Align indicator with study_area_90m_template raster
air_quality_resistance_hospital_density <- align_raster_to_template(study_area_90m_5070, density_0_1_5070_masked, input_type = "continuous")

# Plot the final raster
plot(air_quality_resistance_hospital_density, main = "Resistance: Hospital Density (EPSG:5070, 90m)")

# Write the updated raster 
writeRaster(air_quality_resistance_hospital_density, 
            file.path(final_layers_file_path, "indicators/air_quality_resistance_hospital_density.tif"), 
            overwrite = TRUE)



#### Histograms and Summary Statistics ####

# Extract density values from the raster
density_values <- values(density_raster_ppl)

# Remove NA values
density_values <- density_values[!is.na(density_values)]

# Check for negative values
num_negative_values <- sum(density_values < 0)
cat("Number of negative density values:", num_negative_values, "\n")

# Set negative values to zero
density_values[density_values < 0] <- 0

# Calculate statistics
mean_density <- mean(density_values)
median_density <- median(density_values)
min_density <- min(density_values)
max_density <- max(density_values)

# Create histogram
ggplot(data.frame(Density = density_values), aes(x = Density)) +
  geom_histogram(bins = 300, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Kernel Density Estimate for Hospitals",
       x = "Density",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(Density)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(Density)), color = "blue", linetype = "dashed", linewidth = 1)

# Create density plot
ggplot(data.frame(Density = density_values), aes(x = Density)) +
  geom_density(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Density Plot of KDE Values",
       x = "Density",
       y = "Density")

# Print statistics
cat("Mean Density:", mean_density, "\n")
cat("Median Density:", median_density, "\n")
cat("Min Density:", min_density, "\n")
cat("Max Density:", max_density, "\n")


#### Bootstrapping for Validation ####
# Function to perform KDE on bootstrapped samples

bootstrap_kde <- function(pp, bw, window, dimyx) {
  sample_indices <- sample(1:npoints(pp), replace = TRUE)
  bootstrap_pp <- ppp(pp$x[sample_indices], pp$y[sample_indices], window = window)
  density.ppp(bootstrap_pp, sigma = bw, dimyx = dimyx)
}

# Bootstrapping to create confidence intervals for KDE
n_boot <- 100  # Number of bootstrap samples
dimyx <- c(500, 500)  # Grid size

# Perform bootstrapping
bootstrap_densities <- replicate(n_boot, bootstrap_kde(pp, bw_ppl, window, dimyx), simplify = FALSE)

# Extract density values and store them in an array
density_values_array <- array(unlist(lapply(bootstrap_densities, as.matrix)), dim = c(dimyx[2], dimyx[1], n_boot))

# Calculate mean and standard deviation of bootstrap densities
mean_density <- apply(density_values_array, c(1, 2), mean)
sd_density <- apply(density_values_array, c(1, 2), sd)

# Convert to raster for plotting
mean_density_raster <- raster(list(x = density$xcol, y = density$yrow, z = t(mean_density)))
sd_density_raster <- raster(list(x = density$xcol, y = density$yrow, z = t(sd_density)))

# Plot mean and standard deviation of bootstrap densities
par(mfrow = c(1, 2))
plot(mean_density_raster, main = "Mean Bootstrap KDE")
plot(sd_density_raster, main = "SD Bootstrap KDE")



#### Figure creation for newsletter 5070 100m ####

# Create the map with the masked raster MAGMA color
hospital_5070_masked_fig <- tm_shape(density_raster_ppl_masked_5070) +
  tm_raster(style = "cont", palette = "magma", title = "Density", midpoint = NA, alpha = .7) +
  tm_shape(study_area_admin2_shape_5070) +                 
  tm_borders(lwd = 0.09, col = "grey") +
  tm_shape(wwri_hospital_shape_5070) +
  tm_dots(size = 0.004, col = "black") +
  tm_layout(
    title = "Hospital Access",
    title.position = c("left", "top"),
    title.size = 2,          # Increase the main title font size
    legend.outside = F,      # Keep the legend inside the map
    legend.title.size = 1.5, # Increase the legend title font size
    legend.text.size = 1.2,  # Adjust the font size of the legend text
    bg.color = "white"       # Set the background color
  )
hospital_5070_masked_fig

# figure with no legend
hospital_5070_masked_fig <- tm_shape(density_raster_ppl_masked_5070) +
  tm_raster(style = "cont", palette = "magma", title = "Density", midpoint = NA, alpha = .7) +
  tm_shape(study_area_admin2_shape_5070) +                 
  tm_borders(lwd = 0.09, col = "grey") +
  tm_shape(wwri_hospital_shape_5070) +
  tm_dots(size = 0.004, col = "black") +
  tm_layout(legend.show = FALSE) # Disable the legend
hospital_5070_masked_fig


# Convert tmap to a static plot (ggplot-compatible)
hospital_plot <- tmap::tmap_grob(hospital_5070_masked_fig)

# Save tmap as a grob object
tmap_grob <- tmap_grob(hospital_5070_masked_fig)

# Arrange tmap and ggplot side by side
grid.arrange(tmap_grob, vuln_workers_5070, ncol = 2)
