# create a CONUS CWPP viz for the newsletter
library(tmap)
library(sf)

# read in study area files
study_area_lvl_1 <- st_read("/home/shares/wwri-wildfire/data/multi-domain-data/boundary-layers/processed/admin-boundary-layers/wwri_study_area_admin_1.shp") %>%
  filter(!(name %in% c("British Columbia", "Yukon", "Alaska"))) %>%
  st_transform(5070)

study_area_lvl_2 <- st_read("/home/shares/wwri-wildfire/data/multi-domain-data/boundary-layers/processed/admin-boundary-layers/wwri_study_area_admin_2.shp") %>%
  filter(!(state_name == "Alaska" | country == "Canada")) %>%
  st_transform(5070)

# read in and prepare CONUS CWPP raster
conus_masked_raster_simple <- mask(conus_cwpps_rast, study_area_lvl_1)
conus_masked_raster_simple_cropped <- crop(conus_masked_raster_simple, c(-2400000, -504639.7, 991231.7, 3200000))
plot(conus_masked_raster_simple_cropped)

# create custim palette
#palette <- c("#FAD4D3", "#F69792", "#F15958", "#EC504F", "#A62B29")
#palette <- c("#FDE0D6", "#F8A97E", "#F57C4E", "#EC504F", "#C4423C")
# palette <- c("#FFFFFF", "#F5A3A3", "#E87373", "#CE4848", "#A83838", "#6D1F1F")
# 
# # plot the raster
# plot(conus_masked_raster_simple_cropped, col = color_ramp, main = "Number of CWPPs Covering Each Area in the Western CONUS", axes = FALSE, legend = TRUE, add = FALSE)
# 
# # add state boundaries for plot
# plot(st_geometry(study_area_lvl_1), add = TRUE, border = "black", lwd = 1)
# plot(st_geometry(study_area_lvl_2), add = TRUE, border = "black", lwd = .5)


# better version than the commented out
# create a final viz
tm_shape(conus_masked_raster_simple_cropped) +
  tm_raster(
    palette = colorRampPalette(c("#FFFFFF", "#F5A3A3", "#E87373", "#CE4848", "#A83838", "#6D1F1F"))(100),
    style = "cont",  # treat rast as continuous
    legend.is.portrait = FALSE,  # horiztonal legend
    title = "  # of CWPPs"
  ) +
  tm_shape(study_area_lvl_1) +
  tm_borders(col = "black", lwd = 1) +
  tm_shape(study_area_lvl_2) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_layout(
    #main.title = "Number of CWPPs Covering Each Area in the Western CONUS",  # Title with wrap after 'Area'
    #main.title.size = 1.2,
    #main.title.position = "center",  
    legend.position = c(0.25, -0.03), # legend at bottom w/ vert offset
    #legend.width = 2,
    frame = FALSE, # no frame
    asp = 0, # no aspect ratio stretching
    legend.text.size = .7
  )

# exported at the end with export button