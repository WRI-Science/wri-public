library(tidyverse)
library(sf)

# set base dir
base_dir <- "/home/shares/wwri-wildfire"

# read in original data
us_stream_data_full <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us.csv") %>%
  filter(parameter_cd == "00060") %>%
  select(site_no, flow = mean_va, year = year_nu, month = month_nu) #%>%
  #drop_na(flow) # not needed for US
canada_stream_data_full <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/int/canadian-streamflow-data-full.csv") %>%
  filter(sum_stat == "MEAN") %>%
  mutate(value_us_units = value * 35.3147) %>%
  select(site_no = station_number, flow = value_us_units, year, month) #%>%
  #drop_na(flow) # not needed here either anymore
stream_data_full <- rbind(us_stream_data_full, canada_stream_data_full)

# read in canada data
canada_stream_data <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/int/canadian-streamflow-data-30-yr-and-recent.csv") %>%
  drop_na(value_us_units)
canada_stream_sites <- canada_stream_data %>%
  select(site_no = station_number) %>%
  distinct()

# read in us data
us_stream_data <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/int/us-streamflow-data-30-yr-and-recent.csv")
us_stream_sites <- us_stream_data %>%
  select(site_no) %>% 
  distinct()

# combine us and canada sites
sites_full <- rbind(us_stream_sites, canada_stream_sites)

# filter to only sites of interest
stream_data_full_filtered <- stream_data_full %>%
  filter(site_no %in% sites_full$site_no & year %in% 1991:2024)

# function to read in hydrobasins shapefiles at desired level, combine them, and make them valid
get_hydrobasins <- function(base_dir, level = "lev08") { # default to level 8
  
  hydrobasins <- file.path(base_dir, "data/multi-domain-data/hydro-basins/raw")
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
    select(hybas_id) %>%
    rename(!!new_hybas_name := hybas_id)
  
  return(hb_shapefiles)
}

# get hydrobasins for level 8
hydrobasins_lev08 <- get_hydrobasins(base_dir, "lev08")

# ensure only basins that touch study area are kept
study_area <- st_read("/home/shares/wwri-wildfire/data/multi-domain-data/boundary-layers/processed/admin-boundary-layers/wwri_study_area_admin_0.shp") %>%
  st_transform(., "EPSG:5070")

hydrobasins_lev08_study_area <- st_filter(hydrobasins_lev08, study_area, .predicate = st_intersects)

# read in site to hydrobasin lvl 8 alignment
filled_hybas8_study_area <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/int/hydrobasins_lvl8_filled_w_sites.csv") %>%
  filter(id_lev08 %in% hydrobasins_lev08_study_area$id_lev08)

# join site data to alignment with basins
stream_data_full_filtered_with_hydrobasins <- filled_hybas8_study_area %>%
  left_join(., stream_data_full_filtered, by = "site_no", relationship = "many-to-many") %>%
  select(-id_filled_at, -lvl_filled_at)
sum(is.na(stream_data_full_filtered_with_hydrobasins$id_lev08)) # should be 0


# new rescaling stuff
reference_period <- stream_data_full_filtered_with_hydrobasins %>%
  filter(year %in% 1991:2020)

comparison_period <- stream_data_full_filtered_with_hydrobasins %>%
  filter(year %in% 2015:2024)

# calculate reference flow mean and sd by hydrobasin and month
reference_summary <- reference_period %>%
  group_by(id_lev08, month) %>%
  summarize(
    mean_flow_30_yr = mean(flow, na.rm = TRUE),
    sd_flow_30_yr = sd(flow, na.rm = TRUE),
    .groups = "drop"
  )

# calculate comparison period in similar way -- no sd needed since its year over year and not month only
comparison_summary <- comparison_period %>%
  group_by(id_lev08, year, month) %>%
  summarize(
    mean_flow = mean(flow, na.rm = TRUE),
    .groups = "drop"
  )

# i believe this ended up being less what we wanted
# # calculate z-scores and percentiles
# comparison_stats <- comparison_summary %>%
#   left_join(reference_summary, by = c("id_lev08", "month")) %>%
#   mutate(
#     z_score = (mean_flow - mean_flow_30_yr) / sd_flow_30_yr,
#     z_bin = case_when(
#       is.nan(z_score) ~ "0–1 SD", # means all 3 z-score varsity are 0; Inf is when mean_flow is not 0 but the other 2 are. it already gets assigned to 3+ SD for that reason so don't need to handle that separately
#       abs(z_score) < 1 ~ "0–1 SD",
#       abs(z_score) >= 1 & abs(z_score) < 2 ~ "1–2 SD",
#       abs(z_score) >= 2 & abs(z_score) < 3 ~ "2–3 SD",
#       abs(z_score) >= 3 ~ "3+ SD",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   group_by(id_lev08, month) %>%
#   mutate(
#     mean_flow_dif_from_ref = abs(mean_flow - mean_flow_30_yr),
#     percentile = percent_rank(mean_flow_dif_from_ref)
#   ) %>%
#   ungroup()


# calculate mean monthly flow per year for reference period
reference_summary_by_year <- reference_period %>%
  group_by(id_lev08, year, month) %>%
  summarise(
    mean_flow = mean(flow, na.rm = TRUE),
    .groups = "drop"
  )

# make distribution by hydrobasin and month combo from those yearly means
reference_distribution <- reference_summary_by_year %>%
  group_by(id_lev08, month) %>%
  summarise(
    ref_flows = list(mean_flow),
    mean_flow_30_yr = mean(mean_flow, na.rm = TRUE),
    sd_flow_30_yr = sd(mean_flow, na.rm = TRUE),
    .groups = "drop"
  )


# calculate z-score and percentile against historical distribution
get_percentile_smooth <- function(x, dist) {
  dens <- density(dist, na.rm = TRUE)
  approx(dens$x, cumsum(dens$y) / sum(dens$y), xout = x, rule = 2)$y
}


comparison_stats <- comparison_summary %>%
  left_join(reference_distribution, by = c("id_lev08", "month")) %>%
  mutate(
    z_score = (mean_flow - mean_flow_30_yr) / sd_flow_30_yr,
    z_bin = case_when(
      is.nan(z_score) ~ "0–1 SD",
      #is.na(z_score) & mean_flow > 0 ~ "3+ SD",
      abs(z_score) < 1 ~ "0–1 SD",
      abs(z_score) < 2 ~ "1–2 SD",
      abs(z_score) < 3 ~ "2–3 SD",
      abs(z_score) >= 3 ~ "3+ SD",
      TRUE ~ NA_character_
    ),
    # calculate percentile relative to reference period
    percentile = map2_dbl(mean_flow, ref_flows, get_percentile_smooth)
    #percentile = map2_dbl(mean_flow, ref_flows, ~ mean(.x >= .y, na.rm = TRUE)),
    #percentile = map2_dbl(mean_flow, ref_flows, ~ 
                            #rank(c(.y, .x), ties.method = "max")[length(.y) + 1] / (length(.y) + 1)
    )
    #percentile_test = map2_dbl(mean_flow, ref_flows, ~ ecdf(.y)(.x))
    #minmax_scaled = map2_dbl(mean_flow, ref_flows, ~ 
                               #(.x - min(.y, na.rm = TRUE)) / (max(.y, na.rm = TRUE) - min(.y, na.rm = TRUE)))
 #%>%
  #filter(id_lev08 == 7080008510) %>%
  #filter(year == 2021, month == 10)
#%>%
  #select(id_lev08, year, month, mean_flow, z_score, z_bin, percentile)



# create full grid to fill in NAs for clean plotting
full_grid <- expand_grid(
  id_lev08 = unique(filled_hybas8_study_area$id_lev08),
  year = 2015:2024,
  month = 1:12
)

# add full grid to the existing data
comparison_stats_filled <- full_grid %>%
  left_join(comparison_stats, by = c("id_lev08", "year", "month"))


# add spatial info to our df
comparison_stats_sf <- comparison_stats_filled %>%
  left_join(., hydrobasins_lev08, by = "id_lev08") %>%
  st_as_sf()


# make plots
library(patchwork)


for (yr in 2015:2024) {
  z_bin_plots <- list()
  z_score_plots <- list()
  
  # filter to data for the current year
  data_year <- comparison_stats_sf %>%
    filter(year == yr)
  
  # get z-score range across all months in that year
  z_scores_finite <- data_year$z_score[is.finite(data_year$z_score)] # don't use Inf as max, it will get assigned the same color as the highest finite value
  z_min <- min(z_scores_finite, na.rm = TRUE)
  z_max <- max(z_scores_finite, na.rm = TRUE)
  
  for (mo in 1:12) {
    data_month <- data_year %>%
      filter(month == mo)
    
    # z-score bin map
    p1 <- ggplot(data_month) +
      geom_sf(aes(fill = z_bin), color = NA) +
      scale_fill_manual(
        values = c(
          "0–1 SD" = "#fee8c8",
          "1–2 SD" = "#fdbb84",
          "2–3 SD" = "#e34a33",
          "3+ SD" = "#b30000"
        ),
        na.value = "grey90",
        drop = FALSE,
        guide = guide_legend(reverse = TRUE)
      ) +
      labs(title = month.name[mo], fill = "Z Bin") +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = if (mo == 1) "right" else "none"
      )
    
    # z-score continuous map with shared scale
    p2 <- ggplot(data_month) +
      geom_sf(aes(fill = z_score), color = NA) +
      scale_fill_viridis_c(
        option = "plasma", 
        direction = -1, 
        na.value = "grey90",
        limits = c(z_min, z_max)  # consistent scale across all months
      ) +
      labs(title = month.name[mo], fill = "Z-Score") +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = if (mo == 1) "right" else "none"
      )
    
    z_bin_plots[[mo]] <- p1
    z_score_plots[[mo]] <- p2
  }
  
  # combine and save
  z_bin_grid <- wrap_plots(z_bin_plots, ncol = 4, guides = "collect") +
    plot_annotation(title = paste("Streamflow Z-Score Bins –", yr))
  
  z_score_grid <- wrap_plots(z_score_plots, ncol = 4, guides = "collect") +
    plot_annotation(title = paste("Streamflow Z-Scores –", yr))
  
  ggsave(file.path("water/surface-water/plots", paste0("grid_z_bin_", yr, ".png")),
         z_bin_grid, width = 14, height = 10)
  
  ggsave(file.path("water/surface-water/plots", paste0("grid_z_score_", yr, ".png")),
         z_score_grid, width = 14, height = 10)
}