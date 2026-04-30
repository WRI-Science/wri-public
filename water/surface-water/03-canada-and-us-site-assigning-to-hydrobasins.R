wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse)
library(sf)

# set base dir
base_dir <- file.path(wri_project_root)

# read in canada data
canada_stream_data <- read_csv(file.path(wri_project_root, "data", "water", "int", "canadian-streamflow-data-30-yr-and-recent_2024.csv")) %>%
  drop_na(value_us_units)

canada_stream_sites <- canada_stream_data %>%
  select(site_no = station_number, lat = latitude, lon = longitude) %>% # grab site number, latitude, and longitude for geocoding later
  distinct()

# read in us data
us_stream_data <- read_csv(file.path(wri_project_root, "data", "water", "int", "us-streamflow-data-30-yr-and-recent_2024.csv"))

us_stream_sites <- us_stream_data %>%
  select(site_no, lat = dec_lat_va, lon = dec_long_va) %>% # grab site number, latitude, and longitude for geocoding later
  distinct()

# function to read in hydrobasins shapefiles at desired level, combine them, and make them valid
get_hydrobasins <- function(base_dir, level = "lev08") { # default to level 8
  
  hydrobasins <- file.path(base_dir, "data/multi_domain_data/raw/boundary_layers/hydro_basins/raw")
  lev01_dirs <- list.dirs(hydrobasins, full.names = TRUE, recursive = FALSE)
  lev01_dirs <- lev01_dirs[grepl("lev01", basename(lev01_dirs))]
  
  # select shapefiles based on level
  level_pattern <- paste0(level, "_v1c.shp$")
  hb_shapefile_list <- unlist(lapply(lev01_dirs, function(dir) {
    list.files(dir, pattern = level_pattern, full.names = TRUE)
  }))
  
  # make valid and merge shapefiles
  hb_shapefiles <- do.call(rbind, lapply(hb_shapefile_list, function(shp) {
    # transform to our crs
    st_transform(st_read(shp), "EPSG:5070") %>%
      # make geometries valid
      st_make_valid(st_read(shp)) 
  }))
  
  new_hybas_name <- paste0("id_", level)
  
  hb_shapefiles <- hb_shapefiles %>%
    janitor::clean_names() %>%
    select(hybas_id, pfaf_id) %>%
    rename(!!new_hybas_name := hybas_id)
  
  return(hb_shapefiles)
}

# get hydrobasins for each level as separate objects
for (level in 1:8) {
  print(paste("Processing level", level))
  hydrobasins <- get_hydrobasins(base_dir, paste0("lev0", level))
  
  assign(paste0("hydrobasins_lev", level), hydrobasins)
}

# write out hydrobasin lvl 8 geometries processed for later
st_write(hydrobasins_lev8, file.path(wri_project_root, "data", "water", "int", "hydrobasins_lev8_2024.gpkg"), append = FALSE)

# 
# # use code from Mona's scripts to get hydrobasins
# hydrobasins <- file.path(base_dir, "data/multi-domain-data/hydro-basins/raw")
# # list all directories matching "lev08"
# lev08_dirs <- list.dirs(hydrobasins, full.names = TRUE, recursive = FALSE)
# lev08_dirs <- lev08_dirs[grepl("lev01", basename(lev08_dirs))]
# # list all shapefiles within the selected directories
# hb_shapefile_list <- unlist(lapply(lev08_dirs, function(dir) {
#   list.files(dir, pattern = "\\lev08_v1c.shp$", full.names = TRUE)
# }))
# # read and merge shapefiles
# hb_lev08 <- do.call(rbind, lapply(hb_shapefile_list, st_read))


# do sf object creation separately because of different crs between datasets
us_stream_sites_sf <- st_as_sf(us_stream_sites, coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(., "EPSG:5070")
canada_stream_sites_sf <- st_as_sf(canada_stream_sites, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(., "EPSG:5070")

# combine the site dfs for easier combining with hydrobasins
all_sites_sf <- rbind(us_stream_sites_sf, canada_stream_sites_sf)
# plot(all_sites_sf$geometry)
# plot(study_area$geometry, add = TRUE, border = "red")

# join sites to hydrobasins level 8
all_sites_with_hydrobasins <- st_join(all_sites_sf, hydrobasins_lev8, left = TRUE)
table(all_sites_with_hydrobasins$id_lev08)
nrow(all_sites_with_hydrobasins %>%
       filter(!is.na(id_lev08))) # all sites got assigned a hydrobasin
st_write(all_sites_with_hydrobasins, file.path(wri_project_root, "data", "water", "int", "sites_with_hydrobasins_lvl_8_2024.gpkg"), append = FALSE)


# go through levels 8 to 1 to perform a spatial join with sites
for (level in 8:1) {
  # get hydrobasins for current level
  hydrobasins <- get(paste0("hydrobasins_lev", level))
  
  # join it with sites
  sites_w_hydrobasins <- st_join(all_sites_sf, hydrobasins, left = TRUE)
  
  # save as own object
  assign(paste0("sites_w_hydrobasins_lev", level), sites_w_hydrobasins)
  
  print(paste("Performed spatial join for level", level))
}


# create list to store hydrobasins without geometries
hydrobasins_no_geom <- list()

# go through levels 1 to 8 to drop geoms and save as objs
for (level in 1:8) {
  # get hydrobasins with geometry
  hydrobasins <- get(paste0("sites_w_hydrobasins_lev", level))
  
  # drop geoms while storing result
  hydrobasins_no_geom[[paste0("lev", level)]] <- st_drop_geometry(hydrobasins)
  
  hydrobasins_no_geom[[paste0("lev", level)]] <- hydrobasins_no_geom[[paste0("lev", level)]] %>%
    select(-pfaf_id)
  
  print(paste("Dropped geometry for level", level))
}

# combine site to hydrobasin lvl connections into one df
sites_with_hydrobasins_all_levels <- hydrobasins_no_geom[[1]] # begin with first df

for (level in 2:8) {
  # merge with next df by site_no
  sites_with_hydrobasins_all_levels <- merge(sites_with_hydrobasins_all_levels, hydrobasins_no_geom[[level]], by = "site_no", all = TRUE)
  
  print(paste("Merged level", level))
}

write_csv(sites_with_hydrobasins_all_levels, file.path(wri_project_root, "data", "water", "int", "sites_with_hydrobasins_all_levels_2024.csv"))

length(unique(sites_with_hydrobasins_all_levels$id_lev08))


# the purpose of this part of the script is to assign one or more sites to each hydroshed level 8 in CONUS + AK + Canada
# we started by using sites at level 8, then try filling at the level 7 hydroshed that each level 8 is in, respectively, then level 6, etc
# since each hydroshed will often get assigned more than one sites, we will need to take an average or similar later on
# these are used as a representation for surface water and will be following a similar processing to VPD after sites are assigned hydrosheds

# get a version of hydrobasins with corresponding next level with dropped geometries
for (i in 8:2) {
  # Get finer and coarser hydrobasins
  finer <- get(paste0("hydrobasins_lev", i))
  coarser <- get(paste0("hydrobasins_lev", i - 1))
  
  # Drop geometry to ensure non-spatial joins
  finer <- st_drop_geometry(finer)
  coarser <- st_drop_geometry(coarser)
  
  # Ensure pfaf_id is character
  finer$pfaf_id <- as.character(finer$pfaf_id)
  coarser$pfaf_id <- as.character(coarser$pfaf_id)
  
  # Truncate finer pfaf_id to match coarser level
  finer <- finer %>%
    mutate(parent_pfaf = substr(pfaf_id, 1, nchar(pfaf_id) - 1))
  
  # Rename coarser pfaf_id to avoid column name clash
  coarser <- coarser %>%
    rename_with(~ paste0(., "_lev", i - 1), all_of("pfaf_id"))
  
  # Join by truncated pfaf_id
  joined <- left_join(finer, coarser, by = c("parent_pfaf" = paste0("pfaf_id_lev", i - 1)))
  
  # Drop pfaf_id and parent_pfaf
  joined <- joined %>% select(-pfaf_id, -parent_pfaf)
  
  # Save result
  assign(paste0("level_", i, "_with_level_", i - 1), joined)
  
  print(paste("Joined level", i, "with level", i - 1, "using pfaf_id"))
}


length(unique(level_2_with_level_1$id_lev01)) # 15
length(unique(level_3_with_level_2$id_lev02)) # 48
length(unique(level_4_with_level_3$id_lev03)) # 253
length(unique(level_5_with_level_4$id_lev04)) # 865
length(unique(level_6_with_level_5$id_lev05)) # 2892
length(unique(level_7_with_level_6$id_lev06)) # 9640
length(unique(level_8_with_level_7$id_lev07)) # 31410
# all checks out


# connect sites by pfaf_id instead
# begin with level 8 hydrobasins
hb_hierarchy <- hydrobasins_lev8 %>%
  st_drop_geometry() %>%
  select(id_lev08, pfaf_id) %>%
  mutate(pfaf_id = as.character(pfaf_id))

# go from level 7 to level 1 to join each coarser level by pfaf_id
for (i in 7:1) {
  coarser <- get(paste0("hydrobasins_lev", i)) %>%
    st_drop_geometry() %>%
    select(!!sym(paste0("id_lev0", i)), pfaf_id) %>%
    mutate(pfaf_id = as.character(pfaf_id))
  
  # add pfaf_id to align with coarser level
  hb_hierarchy <- hb_hierarchy %>%
    mutate(!!paste0("pfaf_lev", i) := substr(pfaf_id, 1, i)) %>%
    left_join(coarser, by = setNames("pfaf_id", paste0("pfaf_lev", i))) %>%
    select(-!!sym(paste0("pfaf_lev", i)))
}

# drop pfaf_id column since done using it
hb_hierarchy <- hb_hierarchy %>% select(-pfaf_id)

# check sites that joined spatially with levels match the hierarchy from pfaf_id
sites_with_hydrobasins_all_levels_for_check <- sites_with_hydrobasins_all_levels %>%
  select(-site_no)

not_in <- anti_join(sites_with_hydrobasins_all_levels_for_check, hb_hierarchy)
# all good! 0
# so could do it spatially or by pfaf in the case of site points



# do the site assignments
sites_with_hydrobasins_all_levels <- sites_with_hydrobasins_all_levels %>%
  drop_na(id_lev08) # not interested if site does not have corresponding level 8 hydrobasin, but unlikely to happen; maybe can remove this line now?

hydrobasins_lev8_no_geom <- hydrobasins_lev8 %>%
  st_drop_geometry()

lvl_8_hydrobasins_with_sites_filled_at_lvl_8 <- left_join(hydrobasins_lev8_no_geom, sites_with_hydrobasins_all_levels, by = "id_lev08", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_8 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_8_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_8 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev08) %>%
  mutate(id_filled_at = id_lev08,
         lvl_filled_at = 8) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_8_yes$id_lev08))
lvl_8_hydrobasins_with_sites_filled_at_lvl_8_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_8 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08) %>%
  left_join(level_8_with_level_7, by = "id_lev08")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_8_no$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_7 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_8_no, sites_with_hydrobasins_all_levels, by = "id_lev07", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_7 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_7_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_7 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev07, id_lev08 = id_lev08.x) %>%
  mutate(id_filled_at = id_lev07,
         lvl_filled_at = 7) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_7_yes$id_lev08))
lvl_8_hydrobasins_with_sites_filled_at_lvl_7_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_7 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08 = id_lev08.x, id_lev07) %>%
  left_join(level_7_with_level_6, by = "id_lev07")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_7_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_2 <- sites_with_hydrobasins_all_levels %>%
  select(-id_lev08, -id_lev07)

lvl_8_hydrobasins_with_sites_filled_at_lvl_6 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_7_no, sites_with_hydrobasins_all_levels_minus_2, by = "id_lev06", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_6 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_6_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_6 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev06, id_lev08) %>%
  mutate(id_filled_at = id_lev06,
         lvl_filled_at = 6) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_6_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_6_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_6 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev06) %>%
  left_join(level_6_with_level_5, by = "id_lev06")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_6_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_3 <- sites_with_hydrobasins_all_levels_minus_2 %>%
  select(-id_lev06)

lvl_8_hydrobasins_with_sites_filled_at_lvl_5 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_6_no, sites_with_hydrobasins_all_levels_minus_3, by = "id_lev05", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_5 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_5_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_5 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev05, id_lev08) %>%
  mutate(id_filled_at = id_lev05,
         lvl_filled_at = 5) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_5_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_5_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_5 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev05) %>%
  left_join(level_5_with_level_4, by = "id_lev05")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_5_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_4 <- sites_with_hydrobasins_all_levels_minus_3 %>%
  select(-id_lev05)

lvl_8_hydrobasins_with_sites_filled_at_lvl_4 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_5_no, sites_with_hydrobasins_all_levels_minus_4, by = "id_lev04", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_4 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_4_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_4 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev04, id_lev08) %>%
  mutate(id_filled_at = id_lev04,
         lvl_filled_at = 4) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_4_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_4_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_4 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev04) %>%
  left_join(level_4_with_level_3, by = "id_lev04")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_4_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_5 <- sites_with_hydrobasins_all_levels_minus_4 %>%
  select(-id_lev04)

lvl_8_hydrobasins_with_sites_filled_at_lvl_3 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_4_no, sites_with_hydrobasins_all_levels_minus_5, by = "id_lev03", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_3 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_3_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_3 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev03, id_lev08) %>%
  mutate(id_filled_at = id_lev03,
         lvl_filled_at = 3) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_3_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_3_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_3 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev03) %>%
  left_join(level_3_with_level_2, by = "id_lev03")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_3_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_6 <- sites_with_hydrobasins_all_levels_minus_5 %>%
  select(-id_lev03)

lvl_8_hydrobasins_with_sites_filled_at_lvl_2 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_3_no, sites_with_hydrobasins_all_levels_minus_6, by = "id_lev02", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_2 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_2_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_2 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev02, id_lev08) %>%
  mutate(id_filled_at = id_lev02,
         lvl_filled_at = 2) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_2_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_2_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_2 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev02) %>%
  left_join(level_2_with_level_1, by = "id_lev02")
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_2_no$id_lev08))

sites_with_hydrobasins_all_levels_minus_7 <- sites_with_hydrobasins_all_levels_minus_6 %>%
  select(-id_lev02)

lvl_8_hydrobasins_with_sites_filled_at_lvl_1 <- left_join(lvl_8_hydrobasins_with_sites_filled_at_lvl_2_no, sites_with_hydrobasins_all_levels_minus_7, by = "id_lev01", relationship = "many-to-many")
nrow(lvl_8_hydrobasins_with_sites_filled_at_lvl_1 %>% filter(!is.na(site_no)))

lvl_8_hydrobasins_with_sites_filled_at_lvl_1_yes <- lvl_8_hydrobasins_with_sites_filled_at_lvl_1 %>%
  filter(!is.na(site_no)) %>%
  select(site_no, id_lev01, id_lev08) %>%
  mutate(id_filled_at = id_lev01,
         lvl_filled_at = 1) %>%
  select(id_lev08, id_filled_at, lvl_filled_at, site_no)
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_1_yes$id_lev08))

lvl_8_hydrobasins_with_sites_filled_at_lvl_1_no <- lvl_8_hydrobasins_with_sites_filled_at_lvl_1 %>%
  filter(is.na(site_no)) %>%
  select(id_lev08, id_lev01) 
length(unique(lvl_8_hydrobasins_with_sites_filled_at_lvl_1_no$id_lev08))
# these 75 are ok to not be filled because they're out of our study region

# check where the still unfilled ones are to be sure things worked as expected
lvl_8_hydrobasins_with_sites_filled_at_lvl_1_no_w_geom <- lvl_8_hydrobasins_with_sites_filled_at_lvl_1_no %>%
  left_join(hydrobasins_lev8, by = "id_lev08")

all_filled_hybas_8 <- rbind(lvl_8_hydrobasins_with_sites_filled_at_lvl_8_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_7_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_6_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_5_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_4_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_3_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_2_yes,
                            lvl_8_hydrobasins_with_sites_filled_at_lvl_1_yes) # 355117

all_filled_hybas_8_site_counts <- all_filled_hybas_8 %>%
  group_by(id_lev08, id_filled_at, lvl_filled_at) %>%
  summarize(site_count = n())

all_filled_hybas_8_w_geom <- all_filled_hybas_8 %>%
  left_join(hydrobasins_lev8, by = "id_lev08")

write_csv(all_filled_hybas_8, file.path(wri_project_root, "data", "water", "int", "hydrobasins_lvl8_filled_w_sites_2024.csv"))

# add geometry information back to filled hydrobasin lvl 8s
filled_hybas_8_geom <- left_join(hydrobasins_lev8, all_filled_hybas_8, by = "id_lev08")

# ensure only geometries that touch our study area are kept
study_area <- st_read(file.path(wri_project_root, "data", "multi_domain_data", "int", "boundary_layers", "admin_boundary_layers", "wwri_study_area_admin_0.shp")) %>%
  st_transform(., "EPSG:5070")

filled_hybas_8_geom_study_area <- st_filter(filled_hybas_8_geom, study_area, .predicate = st_intersects)

filled_hybas_8_no_geom_study_area <- filled_hybas_8_geom_study_area %>%
  st_drop_geometry()

# takes long to run
# filled_hybas_8_geom_study_area_geoms_only <- filled_hybas_8_geom_study_area %>%
#   select(id_lev08) %>%
#   distinct()

write_csv(filled_hybas_8_no_geom_study_area, file.path(wri_project_root, "data", "water", "int", "hydrobasins_lvl8_filled_w_sites_study_area_only_2024.csv"))

# double check that the included hydrobasins look correct
# get only one copy of each geometry and add the study area layered on top
filled_hybas_8_geom_study_area_distinct <- filled_hybas_8_geom_study_area %>% 
  select(id_lev08) %>%
  group_by(id_lev08) %>%
  slice(1) %>%
  ungroup()

filled_hybas_8_geom_study_area_distinct %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = study_area, fill = NA, color = "black") +
  theme_minimal() +
  labs(title = "Hydrobasins Level 8 Intersecting Study Area")
# looks good


# i believe all of the below is not needed anymore but keeping for now in case
# # create stat summary for the 30 yr period
# us_streamflow_monthly_30_yr_summary <- us_stream_data %>%
#   filter(year_nu %in% 1991:2020) %>%
#   group_by(site_no, month_nu, parameter_cd) %>%
#   summarise(
#     mean = mean(mean_va, na.rm = TRUE),
#     sd = sd(mean_va, na.rm = TRUE),
#     sd2 = 2 * sd(mean_va, na.rm = TRUE),
#     sd3 = 3 * sd(mean_va, na.rm = TRUE),
#     q1 = quantile(mean_va, 0.25, na.rm = TRUE),
#     q3 = quantile(mean_va, 0.75, na.rm = TRUE),
#     p1 = quantile(mean_va, 0.01, na.rm = TRUE),
#     p5 = quantile(mean_va, 0.05, na.rm = TRUE),
#     p95 = quantile(mean_va, 0.95, na.rm = TRUE),
#     p99 = quantile(mean_va, 0.99, na.rm = TRUE),
#     val_count = n()
#   ) %>%
#   ungroup() %>%
#   filter(parameter_cd == "00060")
# 
# canada_streamflow_monthly_30_yr_summary <- canada_stream_data %>%
#   filter(year %in% 1991:2020) %>%
#   group_by(station_number, month) %>%
#   summarise(
#     mean = mean(value_us_units, na.rm = TRUE),
#     sd = sd(value_us_units, na.rm = TRUE),
#     sd2 = 2 * sd(value_us_units, na.rm = TRUE),
#     sd3 = 3 * sd(value_us_units, na.rm = TRUE),
#     q1 = quantile(value_us_units, 0.25, na.rm = TRUE),
#     q3 = quantile(value_us_units, 0.75, na.rm = TRUE),
#     p1 = quantile(value_us_units, 0.01, na.rm = TRUE),
#     p5 = quantile(value_us_units, 0.05, na.rm = TRUE),
#     p95 = quantile(value_us_units, 0.95, na.rm = TRUE),
#     p99 = quantile(value_us_units, 0.99, na.rm = TRUE),
#     val_count = n(),
#     z_score = (value_us_units - mean) / sd
#   ) %>%
#   ungroup()
# 
# ggplot(canada_streamflow_monthly_30_yr_summary, aes(x = z_score)) +
#   geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
#   facet_wrap(~ month, ncol = 4) +
#   labs(
#     title = "Histograms of Monthly Streamflow Z-Scores (1991–2020)",
#     x = "Z-score",
#     y = "Count"
#   ) +
#   theme_minimal()
# 
# ggplot(canada_streamflow_monthly_30_yr_summary, aes(x = z_score, color = station_number)) +
#   geom_density() +
#   facet_wrap(~ month) +
#   labs(title = "Density of Monthly Z-Scores per Site",
#        x = "Z-score", y = "Density") +
#   theme_minimal()
# 
# # calculate anomaly from year 2023 data for each station
# us_streamflow_monthly_recent_yr_summary <- us_stream_data %>%
#   filter(year_nu == 2023) %>%
#   group_by(site_no, month_nu, parameter_cd) %>%
#   summarise(
#     mean = mean(mean_va, na.rm = TRUE),
#     sd = sd(mean_va, na.rm = TRUE),
#     sd2 = 2 * sd(mean_va, na.rm = TRUE),
#     sd3 = 3 * sd(mean_va, na.rm = TRUE),
#     q1 = quantile(mean_va, 0.25, na.rm = TRUE),
#     q3 = quantile(mean_va, 0.75, na.rm = TRUE),
#     p1 = quantile(mean_va, 0.01, na.rm = TRUE),
#     p5 = quantile(mean_va, 0.05, na.rm = TRUE),
#     p95 = quantile(mean_va, 0.95, na.rm = TRUE),
#     p99 = quantile(mean_va, 0.99, na.rm = TRUE),
#     val_count = n()
#   ) %>%
#   ungroup() %>%
#   filter(parameter_cd == "00060")
# 
# canada_streamflow_monthly_recent_yr_summary <- canada_stream_data %>%
#   filter(year == 2023) %>%
#   group_by(station_number, month) %>%
#   summarise(
#     mean = mean(value_us_units, na.rm = TRUE),
#     sd = sd(value_us_units, na.rm = TRUE),
#     sd2 = 2 * sd(value_us_units, na.rm = TRUE),
#     sd3 = 3 * sd(value_us_units, na.rm = TRUE),
#     q1 = quantile(value_us_units, 0.25, na.rm = TRUE),
#     q3 = quantile(value_us_units, 0.75, na.rm = TRUE),
#     p1 = quantile(value_us_units, 0.01, na.rm = TRUE),
#     p5 = quantile(value_us_units, 0.05, na.rm = TRUE),
#     p95 = quantile(value_us_units, 0.95, na.rm = TRUE),
#     p99 = quantile(value_us_units, 0.99, na.rm = TRUE),
#     val_count = n()
#   ) %>%
#   ungroup()
# 
# # combine the two dataframes and calculate anomalies
# canada_streamflow_anomalies_2023 <- canada_streamflow_monthly_recent_yr_summary %>%
#   select(station_number, month, recent_mean = mean) %>%
#   left_join(
#     canada_streamflow_monthly_30_yr_summary %>%
#       select(station_number, month, clim_mean = mean, clim_sd = sd),
#     by = c("station_number", "month")
#   ) %>%
#   mutate(
#     anomaly = recent_mean - clim_mean, # raw difference
#     z_score = (recent_mean - clim_mean) / clim_sd # standardized anomaly
#   )
# 
# us_streamflow_anomalies_2023 <- us_streamflow_monthly_recent_yr_summary %>%
#   select(site_no, month_nu, recent_mean = mean) %>%
#   left_join(
#     us_streamflow_monthly_30_yr_summary %>%
#       select(site_no, month_nu, clim_mean = mean, clim_sd = sd),
#     by = c("site_no", "month_nu")
#   ) %>%
#   mutate(
#     anomaly = recent_mean - clim_mean, # raw difference
#     z_score = (recent_mean - clim_mean) / clim_sd # standardized anomaly
#   )

# what to do about sites in canada that have NAs for a month the entire 30 year period? leave alone i guess? may need to remove if one of those is a sole site representing a hydrobasin?
# should i filter to sites that have at least one observation for each of the 12 months across the 30 yr period?

# then combine these values with the sites/hydrobasins