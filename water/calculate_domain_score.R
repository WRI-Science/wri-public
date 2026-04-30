library(terra)
library(here) # To assemble file paths within project
# Source functions
source(here("templates_and_functions", "align_raster_to_template.R"))

study_area_rast <- rast("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# resistance
drought_plans <- rast("/home/shares/wwri-wildfire/final_layers/2024/water/indicators/drought_plan_scores.tif")
drought_plans <- align_raster_to_template(study_area_rast, drought_plans)
water_treatment <- rast("/home/shares/wwri-wildfire/final_layers/2023/water/indicators/water_treatment_scores_2024.tif")
water_treatment <- align_raster_to_template(study_area_rast, water_treatment)

# take the average of drought plans and water treatment to get the resistance score
resistance <- mean(c(drought_plans, water_treatment), na.rm = TRUE)
resistance

# align resistance with template
resistance_aligned <- align_raster_to_template(study_area_rast, resistance)


# calculate resilience score (same as resistance but use equation for consistency)
# 1-(1-resistance)
resilience <- 1 - (1 - resistance)
resilience

# align resilience to template
resilience_aligned <- align_raster_to_template(study_area_rast, resilience)

# status
status <- rast("/home/shares/wwri-wildfire/final_layers/2024/water/indicators/streamflow_status_scores_2024.tif")
status

# calculate domain score
# multiply status by resilience (resistance in this case)

# resilience <- rast("/home/shares/wwri-wildfire/final_layers/water/water_resilience.tif")
# status <- rast("/home/shares/wwri-wildfire/final_layers/water/water_status.tif")

# align status to template
status_aligned <- align_raster_to_template(study_area_rast, status)

domain_score <- mean(c(status_aligned, resilience_aligned), na.rm = TRUE)
# domain_score <- ifel(
#   is.na(resilience),
#   status,
#   status * resilience
# )
plot(domain_score)

# multiply each by 100
domain_score <- domain_score * 100
status_aligned <- status_aligned * 100
resilience_aligned <- resilience_aligned * 100
resistance_aligned <- resistance_aligned * 100

# write out resistance score
writeRaster(resistance_aligned, "/home/shares/wwri-wildfire/final_layers/2024/water/water_resistance.tif", overwrite = TRUE)
# write out resilience score
writeRaster(resilience_aligned, "/home/shares/wwri-wildfire/final_layers/2024/water/water_resilience.tif", overwrite = TRUE)
# write out status score
writeRaster(status_aligned, "/home/shares/wwri-wildfire/final_layers/2024/water/water_status.tif", overwrite = TRUE)
# write out domain score
writeRaster(domain_score, "/home/shares/wwri-wildfire/final_layers/2024/water/water_domain_score_mean.tif", overwrite = TRUE)



# # write each out as 90 m too
# # use this code as a template
# resistance90 <- rast(resistance)
# res(resistance90) <- 90
# resistance_90m <- project(resistance, resistance90, method = "bilinear")
# writeRaster(resistance_90m, "/home/shares/wwri-wildfire/domains/water/resistance_scores_90m.tif", overwrite = TRUE)
# resilience90 <- rast(resilience)
# res(resilience90) <- 90
# resilience_90m <- project(resilience, resilience90, method = "bilinear")
# writeRaster(resilience_90m, "/home/shares/wwri-wildfire/domains/water/resilience_scores_90m.tif", overwrite = TRUE)
# status90 <- rast(status)
# res(status90) <- 90
# status_90m <- project(status, status90, method = "bilinear")
# writeRaster(status_90m, "/home/shares/wwri-wildfire/domains/water/status_scores_90m.tif", overwrite = TRUE)
# 
# # domain_score90 <- rast(domain_score)
# # res(domain_score90) <- 90
# # domain_score_90m <- project(domain_score, domain_score90, method = "bilinear")
# writeRaster(domain_score_90m, "/home/shares/wwri-wildfire/domains/water/domain_score_90m.tif", overwrite = TRUE)
# 
# 
# 
# 
# # align raster
# # align with our template
# domain_score_aligned <- align_raster_to_template(study_area_rast, domain_score)
# 
# domain_score_aligned_0_100 <- domain_score_aligned * 100
# plot(domain_score_aligned)
# plot(domain_score_aligned_0_100)
# 
# writeRaster(domain_score_aligned_0_100, "/home/shares/wwri-wildfire/final_layers/water/water_domain_score_mean.tif", overwrite = TRUE)
