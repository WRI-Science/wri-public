wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)
library(tidyverse)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))

# study area
study_area_raster <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"))

# read in these rasters: CWPPs, firewise communities, volunteer fire departments, age, disability, car access
cwpps_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_cwpps.tif"))
firewise_comms_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_firewise_comms.tif"))
vol_fire_depts_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_vol_fire_stations.tif"))
age_65_plus_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_age_65_plus.tif"))
disability_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_disability.tif"))
no_vehicle_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_resistance_no_vehicle.tif"))
roads_rast <- rast(file.path(wri_project_root, "final_layers", "2023", "infrastructure", "indicators", "infrastructure_resistance_egress.tif"))

# align each raster
# cwpps_rast <- align_raster_to_template(study_area_raster_90m, cwpps_rast)
# firewise_comms_rast <- align_raster_to_template(study_area_raster_90m, firewise_comms_rast)
# vol_fire_depts_rast <- align_raster_to_template(study_area_raster_90m, vol_fire_depts_rast)
# age_65_plus_rast <- align_raster_to_template(study_area_raster_90m, age_65_plus_rast)
# disability_rast <- align_raster_to_template(study_area_raster_90m, disability_rast)
# no_vehicle_rast <- align_raster_to_template(study_area_raster_90m, no_vehicle_rast)
#roads_rast_aligned <- align_raster_to_template(study_area_raster, roads_rast)


# stack the rasters for the calculation
resistance_raster_stack <- c(cwpps_rast,
                             firewise_comms_rast,
                             vol_fire_depts_rast,
                             age_65_plus_rast, 
                             disability_rast, 
                             no_vehicle_rast,
                             roads_rast)

# take the average
resistance_raster <- terra::mean(resistance_raster_stack, na.rm = TRUE) # app(resistance_raster_stack, fun = mean, na.rm = TRUE)

# convert to 90 m (temporary fix, should ultimately rasterize everything to 90)
# r90 <- rast(resistance_raster)
# res(r90) <- 90
# resistance_raster_90m <- project(resistance_raster, r90, method = "bilinear")

# plot and write out resistance
plot(resistance_raster, main = "Resistance Raster")
#writeRaster(resistance_raster, file.path(wri_project_root, "final_layers", "2023", "communities", "people", "resistance_raster.tif"), overwrite = TRUE)

#resistance_raster_100 <- resistance_raster * 100
writeRaster(resistance_raster, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_resistance.tif"), overwrite = TRUE)
# resistance_raster_4269 <- resistance_raster %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(resistance_raster_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "resistance_raster_4269.tif"), overwrite = TRUE)


# read in rasters: home ownership, incorporation, income (above 200k+ and below poverty level)
incorporation_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_incorporation.tif"))
income_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "communities_recovery_income.tif"))
# poverty_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_poverty.tif"))
owner_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_owners.tif"))
# greater_than_200k_rast <- rast(file.path(wri_project_root, "final_layers", "2024", "communities", "indicators", "communities_recovery_greater_than_200k.tif"))

# # average income: 200k+ and poverty level first to create income indicator
# income_raster_stack <- c(poverty_rast, greater_than_200k_rast)
# income_rast <- terra::mean(income_raster_stack, na.rm = TRUE)
# 
# writeRaster(income_rast, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_recovery_income.tif"), overwrite = TRUE)

# average all rasters after creating income raster
recovery_raster_stack <- c(income_rast, owner_rast, incorporation_rast)
# take the average
recovery_raster <- terra::mean(recovery_raster_stack, na.rm = TRUE) #app(recovery_raster_stack, fun = mean, na.rm = TRUE)

# plot and write out recovery
plot(recovery_raster, main = "Recovery Raster")

#recovery_raster_100 <- recovery_raster * 100

writeRaster(recovery_raster, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_recovery.tif"), overwrite = TRUE)

# recovery_raster_4269 <- recovery_raster %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(recovery_raster_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "recovery_raster_4269.tif"), overwrite = TRUE)



# calculate resilience score = 1-(1-resistance)*(1-recovery)
# set up resilience function
calc_resilience <- function(resistance, recovery) {
  message("-- Computing resilience composite via vector math + cover()")
  # 1) pure composite where both exist
  composite <- 1 - (1 - resistance) * (1 - recovery)
  names(composite) <- "resilience_temp"
  # 2) if composite is NA but resistance exists, fall back to resistance
  step2 <- cover(composite, resistance)
  # 3) if still NA but recovery exists, fall back to recovery
  resilience <- cover(step2, recovery)
  names(resilience) <- "resilience"
  return(resilience)
}

resilience_raster <- calc_resilience(resistance_raster, recovery_raster)

# # convert to 90 m (temporary fix, should ultimately rasterize everything to 90)
# r90 <- rast(resilience_scores)
# res(r90) <- 90
# resilience_scores_90m <- project(resilience_scores, r90, method = "bilinear")

# multiply by 100
#resilience_raster_100 <- resilience_raster * 100

# plot and write out resilience
plot(resilience_raster, main = "Resilience Scores")
writeRaster(resilience_raster, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_resilience.tif"), overwrite = TRUE)
# writeRaster(resilience_scores_90m, file.path(wri_project_root, "domains", "sense-of-place", "people", "resilience_raster_90m.tif"), overwrite = TRUE)
# resilience_scores_4269 <- resilience_scores %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(resilience_scores_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "resilience_raster_4269.tif"), overwrite = TRUE)


# read in status raster
status_raster <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned.tif"))

# multiply by 100
#status_raster_100 <- status_raster * 100
# write out
writeRaster(status_raster, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_status.tif"), overwrite = TRUE)

# calculate domain scores
# domain score = status x resilience (just drops out any tracts with no population for this domain)
domain_score_raster <- mask(resilience_raster, status_raster)

domain_score_raster_100 <- domain_score_raster * 100

# # convert to 90 m (temporary fix, should ultimately rasterize everything to 90)
# r90 <- rast(domain_scores)
# res(r90) <- 90
# domain_scores_90m <- project(domain_scores, r90, method = "bilinear")


# plot and write out domain scores
plot(domain_score_raster, main = "Domain Scores")
writeRaster(domain_score_raster_100, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_domain_score.tif"), overwrite = TRUE)
# writeRaster(domain_scores_90m, file.path(wri_project_root, "domains", "sense-of-place", "people", "domain_score_raster_90m.tif"), overwrite = TRUE)
# domain_scores_4269 <- domain_scores %>%
#   project(x = ., y = "EPSG:4269")
# writeRaster(domain_scores_4269, file.path(wri_project_root, "domains", "sense-of-place", "people", "domain_score_raster_4269.tif"), overwrite = TRUE)


# masking temporarily
#communities_domain_score <- rast(file.path(wri_project_root, "final_layers", "2023", "communities", "communities_domain_score.tif"))
# communities_resistance <- rast(file.path(wri_project_root, "final_layers", "2023", "communities", "communities_resistance.tif"))/100
# communities_resilience <- rast(file.path(wri_project_root, "final_layers", "2023", "communities", "communities_resilience.tif"))
# communities_resilience <- align_raster_to_template(study_area_rast, communities_resilience)
# communities_status <- rast(file.path(wri_project_root, "final_layers", "2023", "communities", "communities_status.tif"))/100
# communities_status <- align_raster_to_template(study_area_rast, communities_status)
# communities_recovery <- rast(file.path(wri_project_root, "final_layers", "2023", "communities", "communities_recovery.tif"))/100
# communities_domain_score <- mask(communities_resilience, communities_status)
# 
# 
# # read in mask
# human_sett_mask <- rast(file.path(wri_project_root, "data", "multi_domain_data", "int", "human_settlement", "human_sett_aligned_old.tif"))



# align each raster
# communities_domain_score <- align_raster_to_template(study_area_rast, communities_domain_score)
# communities_resistance <- align_raster_to_template(study_area_rast, communities_resistance)
# #communities_resilience <- align_raster_to_template(study_area_rast, communities_resilience)
# #communities_status <- align_raster_to_template(study_area_rast, communities_status)
# communities_recovery <- align_raster_to_template(study_area_rast, communities_recovery)
# 
# # multiply each by 100
# communities_domain_score <- communities_domain_score * 100
# communities_resistance <- communities_resistance * 100
# communities_resilience <- communities_resilience * 100
# communities_status <- communities_status * 100
# communities_recovery <- communities_recovery * 100

# write out
# writeRaster(communities_domain_score, file.path(wri_project_root, "final_layers", "2023", "communities", "communities_domain_score.tif"), overwrite = TRUE)
# writeRaster(communities_resistance, file.path(wri_project_root, "final_layers", "2023", "communities", "communities_resistance.tif"), overwrite = TRUE)
# writeRaster(communities_resilience, file.path(wri_project_root, "final_layers", "2023", "communities", "communities_resilience.tif"), overwrite = TRUE)
# writeRaster(communities_status, file.path(wri_project_root, "final_layers", "2023", "communities", "communities_status.tif"), overwrite = TRUE)
# writeRaster(communities_recovery, file.path(wri_project_root, "final_layers", "2023", "communities", "communities_recovery.tif"), overwrite = TRUE)


# apply the function to each layer
#masked_communities_domain_score <- mask(communities_domain_score, human_sett_mask)
masked_communities_resistance <- mask(resistance_raster, status_raster)
masked_communities_resilience <- mask(resilience_raster, status_raster)
#masked_communities_status <- mask(communities_status, human_sett_mask)
masked_communities_recovery <- mask(recovery_raster, status_raster)

#masked_communities_resistance_100 <- masked_communities_resistance * 100
#masked_communities_resilience_100 <- masked_communities_resilience * 100
#masked_communities_recovery_100 <- masked_communities_recovery * 100

# write out masked layers
names(domain_score_raster_100) <- "communities_domain_score"
names(status_raster) <- "communities_status"
names(masked_communities_resilience) <- "communities_resilience"
names(masked_communities_recovery) <- "communities_recovery"
names(masked_communities_resistance) <- "communities_resistance"

writeRaster(domain_score_raster_100, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_domain_score_masked.tif"), overwrite = TRUE)
writeRaster(masked_communities_resistance, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_resistance_masked.tif"), overwrite = TRUE)
writeRaster(masked_communities_resilience, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_resilience_masked.tif"), overwrite = TRUE)
writeRaster(status_raster, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_status_masked.tif"), overwrite = TRUE)
writeRaster(masked_communities_recovery, file.path(wri_project_root, "final_layers", "2024", "communities", "communities_recovery_masked.tif"), overwrite = TRUE)