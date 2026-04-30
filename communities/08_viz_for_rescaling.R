wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)
library(readr)
library(knitr)
library(kableExtra)

# read in vector of study area to add as borders on the plots
study_area_vect <- vect(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_admin_0.shp"))

# read in all data rasts
vol_fire_depts_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "vol_fire_depts_4269.tif"))
firewise_comms_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "firewise_comms_4269.tif"))
not_in_poverty_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "poverty_data_rast_4269.tif"))
not_renter_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "renter_data_rast_4269.tif"))
greater_than_200k_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "greater_than_200k_rast_4269.tif"))
age_64_under_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "age_65_plus_rast_4269.tif"))
no_disability_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "disability_rast_4269.tif"))
has_vehicle_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "no_vehicle_rast_4269.tif"))
population_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "population_rast_4269.tif"))
cwpp_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "cwpps_4269.tif"))
incorporation_rast <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "incorporation_4269.tif"))

# # read in csv form of data for histograms/summary stats
# census_variables_people_full <- read_csv(file.path(wri_project_root, "domains", "sense-of-place", "people", "census_variables_people_full.csv"))
# census_variables_people_full_correct_direction <- read_csv(file.path(wri_project_root, "domains", "sense-of-place", "people", "census_variables_people_full_correct_direction.csv"))

# get the study area extent for cropping/plotting
study_area_1km <- rast(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0.tif"))
ref_extent <- ext(study_area_1km)

# crop to ref extent
vol_fire_depts_rast <- crop(vol_fire_depts_rast, ref_extent)
firewise_comms_rast <- crop(firewise_comms_rast, ref_extent)
not_in_poverty_rast <- crop(not_in_poverty_rast, ref_extent)
not_renter_rast <- crop(not_renter_rast, ref_extent)
greater_than_200k_rast <- crop(greater_than_200k_rast, ref_extent)
age_64_under_rast <- crop(age_64_under_rast, ref_extent)
no_disability_rast <- crop(no_disability_rast, ref_extent)
has_vehicle_rast <- crop(has_vehicle_rast, ref_extent)
population_rast <- crop(population_rast, ref_extent)
cwpp_rast <- crop(cwpp_rast, ref_extent)
incorporation_rast <- crop(incorporation_rast, ref_extent)

# all of these are phrased as beneficial to resilience (ie. 1 = good)
# plot(vol_fire_depts_rast)
# plot(firewise_comms_rast)
# plot(not_in_poverty_rast)
# plot(not_renter_rast)
# plot(greater_than_200k_rast)
# plot(age_64_under_rast)
# plot(no_disability_rast)
# plot(has_vehicle_rast)
# plot(log(population_rast + 1))

# make 3x3 plotting grid
par(mfrow = c(3, 3))

# plot each raster in color palette
color_palette <- colorRampPalette(c("#FFFFFF", "#F8B267", "#E09034")) # F8B267 is the sense of place

plot(vol_fire_depts_rast, col = color_palette(100), main = "Volunteer Fire Departments", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(firewise_comms_rast, col = color_palette(100), main = "Firewise Communities", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(not_in_poverty_rast, col = color_palette(100), main = "Not in Poverty (Poverty Inverted)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(not_renter_rast, col = color_palette(100), main = "Not a Renter (Renters Inverted)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(greater_than_200k_rast, col = color_palette(100), main = "Income > 200k", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(age_64_under_rast, col = color_palette(100), main = "Age < 65 (Age 65+ Inverted)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(no_disability_rast, col = color_palette(100), main = "No Disability (Disability Inverted)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(has_vehicle_rast, col = color_palette(100), main = "Has Vehicle (No Vehicle Inverted)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(log(population_rast + 1), col = color_palette(100), main = "Log(Population + 1)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(cwpp_rast, col = color_palette(100), main = "Community Wildfire Protection Plans (CWPPs)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
category_colors <- c("0" = "#FFFFFF", "1" = "#E09034")
incorporation_rast <- as.factor(incorporation_rast)
plot(incorporation_rast, col = category_colors, main = "Incorporation (Yes or No)", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)

# reset plotting area
par(mfrow = c(1, 1))

# get hists and summary stats
par(mfrow = c(1, 2))
plot(vol_fire_depts_rast, main = "Volunteer Fire Departments")
hist(values(vol_fire_depts_rast, na.rm = TRUE), main = "Volunteer Fire Departments Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")


plot(firewise_comms_rast, main = "Firewise Communities")
hist(values(firewise_comms_rast, na.rm = TRUE), main = "Firewise Communities Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(not_in_poverty_rast, main = "Not in Poverty (Poverty Inverted)")
hist(values(not_in_poverty_rast, na.rm = TRUE), main = "Not in Poverty (Poverty Inverted) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(not_renter_rast, main = "Not a Renter (Renters Inverted)")
hist(values(not_renter_rast, na.rm = TRUE), main = "Not a Renter (Renters Inverted) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")
# hist(census_variables_people_full$renter, main = "Not a Renter (Unrasterized)", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(greater_than_200k_rast, main = "Income > 200k")
hist(values(greater_than_200k_rast, na.rm = TRUE), main = "Income > 200k Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(age_64_under_rast, main = "Age < 65 (Age 65+ Inverted)")
hist(values(age_64_under_rast, na.rm = TRUE), main = "Age < 65 (Age 65+ Inverted) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(no_disability_rast, main = "No Disability (Disability Inverted)")
hist(values(no_disability_rast, na.rm = TRUE), main = "No Disability (Disability Inverted) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(has_vehicle_rast, main = "Has Vehicle (No Vehicle Inverted)")
hist(values(has_vehicle_rast, na.rm = TRUE), main = "Has Vehicle (No Vehicle Inverted) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(log(population_rast + 1), main = "Log(Population + 1)")
hist(values(log(population_rast + 1), na.rm = TRUE), main = "Log(Population + 1) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(cwpp_rast, main = "Community Wildfire Protection Plans (CWPPs)")
hist(values(cwpp_rast, na.rm = TRUE), main = "Community Wildfire Protection Plans (CWPPs) Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")

plot(incorporation_rast, main = "Incorporation")
hist(values(incorporation_rast, na.rm = TRUE), main = "Incorporation Histogram", col = "darkred", xlab = "Values", ylab = "Frequency")




# alt plotting method - less useful but leaving the code for now
# set up list of rasters
population_rast_logged <- log(population_rast + 1)

rasters <- list(
  vol_fire_depts_rast = vol_fire_depts_rast,
  firewise_comms_rast = firewise_comms_rast,
  not_in_poverty_rast = not_in_poverty_rast,
  not_renter_rast = not_renter_rast,
  greater_than_200k_rast = greater_than_200k_rast,
  age_64_under_rast = age_64_under_rast,
  no_disability_rast = no_disability_rast,
  has_vehicle_rast = has_vehicle_rast,
  population_rast_logged = population_rast_logged
)

# set up raster titles
titles <- c(
  "Volunteer Fire Departments",
  "Firewise Communities",
  "Not in Poverty (Poverty Inverted)",
  "Not a Renter (Renters Inverted)",
  "Income > 200k",
  "Age < 65 (Age 65+ Inverted)",
  "No Disability (Disability Inverted)",
  "Has Vehicle (No Vehicle Inverted)",
  "Population (Logged + 1)"
)

# set up 3x3 plotting area
par(mfrow = c(3, 3))

# colors for histograms
colors <- c("skyblue", "lightgreen", "salmon", "gold", "lightpink", 
            "lightcoral", "lightyellow", "lightsalmon", "lavender")

# plot rasters and histograms
for (i in seq_along(rasters)) {
  # Plot each raster
  plot(rasters[[i]], main = titles[i])
  
  # # get vals for histograms
  # raster_values <- values(rasters[[i]], na.rm = TRUE)
  
  # get histograms with the specified colors
  hist(rasters[[i]],
       main = paste(titles[i], "Histogram"),
       col = colors[i],
       xlab = "Values",
       ylab = "Frequency",
       xlim = range(unlist(lapply(rasters, function(x) values(x, na.rm = TRUE)))),
       breaks = 20)
}

# reset plotting area
par(mfrow = c(1, 1))


# get summary stats
get_raster_summary <- function(raster, raster_name) {

  stats <- global(raster, c("min", "max", "mean", "sd"), na.rm = TRUE)
  
  # get non-NA values to calculate median (separately)
  raster_values <- values(raster, na.rm = TRUE)
  raster_median <- median(raster_values)
  
  # make df of the stats
  stats_df <- data.frame(
    raster_name = raster_name, # needed a named list to do this
    min = round(stats[1], 5),
    max = round(stats[2], 5),
    mean = round(stats[3], 5),
    sd = round(stats[4], 5),
    median = round(raster_median, 5),
    stringsAsFactors = FALSE
  )
  
  return(stats_df)
}

# apply function to the rasters
all_summary_stats <- do.call(rbind, lapply(names(rasters), function(name) {
  get_raster_summary(rasters[[name]], name)  # pass the name directly
}))

# get rid of row names
row.names(all_summary_stats) <- NULL

# view the df
View(all_summary_stats)

# make it prettier
all_summary_stats %>%
  kable("html", caption = "Sense of Place: Communities Summary Stats") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = FALSE)
