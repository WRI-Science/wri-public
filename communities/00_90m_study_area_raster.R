wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#study_area_1km <- rast(file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0.tif")) # eventually this should be 90 m
library(terra)
library(sf)
library(tidyverse)

moll_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m'

# read in study area vector file
study_area_vect <- vect(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_0.shp"))
# already in equal area so we can get exactly 90 m x 90 m res
study_area_vect_moll <- vect(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_0_moll.shp"))

# study_area_vect_4269 <- project(study_area_vect, "EPSG:4269") # make 4269 version for plotting data

desired_resolution <- 90 # set 90 m resolution 

# set up template raster
study_area_raster_template <- rast(extent = ext(study_area_vect), resolution = desired_resolution, crs = "EPSG:5070")
study_area_raster_template_moll <- rast(extent = ext(study_area_vect_moll), resolution = desired_resolution, crs = moll_crs)

# rasterize study area to template raster
# study_area_raster_90m <- rasterize(study_area_vect, study_area_raster_template, background = 0)
study_area_raster_90m_na <- rasterize(study_area_vect, study_area_raster_template)
study_area_raster_90m_na_moll <- rasterize(study_area_vect_moll, study_area_raster_template_moll)

# ensure it looks correct
# plot(study_area_raster_90m)
plot(study_area_raster_90m_na)
plot(study_area_raster_90m_na_moll)

# write it out for future use
#writeRaster(study_area_raster_90m, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0-90m.tif"), overwrite = TRUE)
writeRaster(study_area_raster_90m_na, file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"), overwrite = TRUE)
writeRaster(study_area_raster_90m_na_moll, file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_raster_mask_lvl_0_90m_with_na_moll.tif"), overwrite = TRUE)


# # convert to 4269
# study_area_raster_90m_4269 <- study_area_raster_90m %>%
#   project(., "EPSG:4269") %>%
#   crop(., study_area_vect_4269) # need to recrop the extent
# study_area_raster_90m_na_4269 <- study_area_raster_90m_na %>%
#   project(., "EPSG:4269") %>%
#   crop(., study_area_vect_4269) # need to recrop the extent

# # ensure it looks correct
# plot(study_area_raster_90m_4269)
# plot(study_area_raster_90m_na_4269)

# # write out the transformed one
# writeRaster(study_area_raster_90m_4269, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0-90m-4269.tif"), overwrite = TRUE)
# writeRaster(study_area_raster_90m_na_4269, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "wwri_study_area_raster-mask-lvl-0-90m-4269-with-na.tif"), overwrite = TRUE)


# make one for just US too for the US only data (so that Canada shows up as NA rather than 0)
us_study_area_vect <- vect(st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_1.shp")) %>% filter(!(name %in% c("British Columbia", "Yukon"))))
# already in equal area so we can get almost exactly 90 m x 90 m res (it is conus equal area)

us_study_area_vect_5070 <- project(us_study_area_vect, "EPSG:5070") # make 4269 version for plotting data

us_desired_resolution <- 90 # set 90 m resolution 

# set up template raster
us_study_area_raster_template <- rast(extent = ext(us_study_area_vect_5070), resolution = us_desired_resolution, crs = "EPSG:5070")

# rasterize study area to template raster
# us_study_area_raster_90m <- rasterize(us_study_area_vect_5070, us_study_area_raster_template, background = 0)
us_study_area_raster_90m_na <- rasterize(us_study_area_vect_5070, us_study_area_raster_template)

# ensure it looks correct
# plot(us_study_area_raster_90m)
plot(us_study_area_raster_90m_na)

# write it out for future use
# writeRaster(us_study_area_raster_90m, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "us_wwri_study_area_raster-mask-lvl-0-90m.tif"), overwrite = TRUE)
writeRaster(us_study_area_raster_90m_na, file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "us_wwri_study_area_raster_mask_lvl_0_90m_with_na.tif"), overwrite = TRUE)

# # convert to 4269
# us_study_area_raster_90m_4269 <- us_study_area_raster_90m %>%
#   project(., "EPSG:4269") %>%
#   crop(., us_study_area_vect) # need to recrop the extent
# us_study_area_raster_90m_na_4269 <- us_study_area_raster_90m_na %>%
#   project(., "EPSG:4269") %>%
#   crop(., us_study_area_vect) # need to recrop the extent

# # ensure it looks correct
# plot(us_study_area_raster_90m_4269)
# plot(us_study_area_raster_90m_na_4269)

# # write out the transformed one
# writeRaster(us_study_area_raster_90m_4269, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "us_wwri_study_area_raster-mask-lvl-0-90m-4269.tif"), overwrite = TRUE)
# writeRaster(us_study_area_raster_90m_na_4269, file.path(wri_project_root, "data", "multi-domain-data", "boundary-layers", "processed", "admin-boundary-layers", "us_wwri_study_area_raster-mask-lvl-0-90m-4269-with-na.tif"), overwrite = TRUE)