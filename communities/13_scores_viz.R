wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(terra)

# read in vector of study area to add as borders on the plots
study_area_vect <- vect(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_admin_0.shp"))

# prepare reference extent for cropping
study_area_1km <- rast(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0.tif"))
ref_extent <- ext(study_area_1km)


# read in scores rasters (4269 for viz)
status_raster <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "status_raster_4269.tif"))
resistance_raster <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "resistance_raster_4269.tif"))
recovery_raster <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "recovery_raster_4269.tif"))
resilience_raster <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "resilience_raster_4269.tif"))
domain_score_raster <- rast(file.path(wri_project_root, "domains", "sense-of-place", "people", "domain_score_raster_4269.tif"))

# crop to ref extent
status_raster <- crop(status_raster, ref_extent)
resistance_raster <- crop(resistance_raster, ref_extent)
recovery_raster <- crop(recovery_raster, ref_extent)
resilience_raster <- crop(resilience_raster, ref_extent)
domain_score_raster <- crop(domain_score_raster, ref_extent)

# create color palette
color_palette <- colorRampPalette(c("#FFFFFF", "#F8B267", "#E09034")) # F8B267 is the sense of place

# plot scores rasters
plot(status_raster, col = color_palette(100), main = "Status", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(resistance_raster, col = color_palette(100), main = "Resistance", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(recovery_raster, col = color_palette(100), main = "Recovery", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(resilience_raster, col = color_palette(100), main = "Resilience", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)
plot(domain_score_raster, col = color_palette(100), main = "Domain Score", colNA = "lightgrey", axes = FALSE, box = FALSE)
plot(study_area_vect, 
     add = TRUE, 
     border = "black", 
     lwd = .2)