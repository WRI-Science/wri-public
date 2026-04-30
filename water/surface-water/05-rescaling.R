library(tidyverse)
library(terra)
library(sf)

# read in data of interest
# canada
canada_stream_data <- read_csv("/home/shares/wwri-wildfire/data/water/int/canadian-streamflow-data-30-yr-and-recent_2024.csv") %>% 
  #filter(sum_stat == "MEAN") %>%
  #mutate(value_us_units = value * 35.3147) %>%
  select(site_no = station_number, flow = value_us_units, year, month, full_year)

# us
us_stream_data <- read_csv("/home/shares/wwri-wildfire/data/water/int/us-streamflow-data-30-yr-and-recent_2024.csv") %>%
  #filter(parameter_cd == "00060") %>%
  select(site_no, flow = mean_va, year = year_nu, month = month_nu, full_year)

# combine the datasets
stream_data <- bind_rows(canada_stream_data, us_stream_data)

#### quantity
historic_quantity <- stream_data %>%
  filter(year %in% 1991:2020 & full_year == 1) %>% # check the data 2009 to 2020 to see how many obs per site per year; drop any sites if it does not have 12 obs for a year for that year - do not include in reference reconstruction; dont actually drop any sites
  group_by(site_no, year) %>%
  summarize(annual_flow = sum(flow, na.rm = TRUE), .groups = "drop")

# gets stats needed
stats_quantity <- historic_quantity %>%
  group_by(site_no) %>%
  summarize(
    mean_flow = mean(annual_flow),
    sd_flow = sd(annual_flow),
    p1 = quantile(annual_flow, probs = 0.01),
    p99 = quantile(annual_flow, probs = 0.99),
    lower_bound = mean_flow - sd_flow,
    upper_bound = mean_flow + sd_flow,
    .groups = "drop"
  )

# compute 2023 scores
annual_2024 <- stream_data %>%
  filter(year == 2024) %>%
  group_by(site_no) %>%
  summarize(annual_flow = sum(flow, na.rm = TRUE), .groups = "drop")

quantity_scores <- annual_2024 %>%
  left_join(stats_quantity, by = "site_no") %>%
  rowwise() %>%
  mutate(
    m1 = (1 - 0) / (lower_bound - p1),
    b1 = -m1 * p1, # solving 0=m1*p1+b1
    m2 = (0 - 1) / (p99 - upper_bound),
    b2 = 1 - m2 * upper_bound, # solving 1=m2*upper_bound+b2
    
    
    quantity_score = case_when(
      annual_flow <= p1 | annual_flow >= p99 ~ 0,
      annual_flow >= lower_bound & annual_flow <= upper_bound ~ 1,
      annual_flow > p1 & annual_flow < lower_bound ~ m1 * annual_flow + b1,
      annual_flow < p99 & annual_flow > upper_bound ~ m2 * annual_flow + b2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()



#### timing
# only include sites with 1 observation per month for a year in distribution
monthly_stats <- stream_data %>%
  filter(year %in% 1991:2020) %>% # don't need full_year == 1 here
  group_by(site_no, month) %>%
  summarize(
    mean_month = mean(flow, na.rm = TRUE),
    sd_month = sd(flow, na.rm = TRUE),
    n_years = n_distinct(year),
    .groups = "drop"
  )


# calculate historic number of months outside across gauges for each year
historic_monthly <- stream_data %>%
  filter(year %in% 1991:2020 & full_year == 1) %>%
  left_join(monthly_stats, by = c("site_no", "month")) %>%
  mutate(outside = ifelse(flow < (mean_month - sd_month) | flow > (mean_month + sd_month), 1, 0)) %>%
  group_by(site_no, year) %>%
  summarize(num_outside = sum(outside, na.rm = TRUE), .groups = "drop")

# get 1st and 99th percentiles -- don't actually need 1st because we want to set 0 months to score of 1 regardless
timing_distribution <- quantile(historic_monthly$num_outside, probs = c(0.01, 0.99), na.rm = TRUE)
#min_out <- timing_distribution[[1]]
max_out <- timing_distribution[[2]]

# get data year data to calculate scores for and get months outside
monthly_2024 <- stream_data %>%
  filter(year == 2024) %>%
  left_join(monthly_stats, by = c("site_no", "month")) %>%
  mutate(outside = ifelse(flow < (mean_month - sd_month) | flow > (mean_month + sd_month), 1, 0)) %>%
  group_by(site_no) %>%
  summarize(num_outside = sum(outside, na.rm = TRUE), .groups = "drop")


# calculate timing scores for data year
timing_scores <- monthly_2024 %>%
  mutate(timing_score = case_when(
    num_outside == 0 ~ 1,
    num_outside >= max_out ~ 0,
    TRUE ~ (-1 / max_out) * num_outside + 1 # 1 - (num_outside / max_out)  # linear rescale from 1 to 0
  ))

# bind quantity and timing scores together
final_scores <- quantity_scores %>%
  select(site_no, quantity_score) %>%
  left_join(timing_scores %>% select(site_no, timing_score), by = "site_no")

# read in hydrobasins lvl 8s in study area with site assignments
lvl8_hydrobasins_w_sites <- read_csv("/home/shares/wwri-wildfire/data/water/int/hydrobasins_lvl8_filled_w_sites_study_area_only_2024.csv")

# join final scores to hydrobasins
final_scores_hydrobasins <- lvl8_hydrobasins_w_sites %>%
  left_join(final_scores, by = "site_no") %>%
  group_by(id_lev08) %>%
  summarize(
    quantity_score = mean(quantity_score, na.rm = TRUE),
    timing_score = mean(timing_score, na.rm = TRUE),
    status = (quantity_score + timing_score) / 2,
    .groups = "drop"
  )

# add in geometries
hydrobasins_lev8 <- st_read("/home/shares/wwri-wildfire/data/water/int/hydrobasins_lev8_2024.gpkg") %>% 
  select(id_lev08, geom)
final_scores_hydrobasins <- hydrobasins_lev8 %>%
  right_join(final_scores_hydrobasins, by = "id_lev08") %>%
  st_set_geometry("geom")

# fix geoms
st_geometry(final_scores_hydrobasins) <- st_geometry(hydrobasins_lev8)[match(final_scores_hydrobasins$id_lev08, hydrobasins_lev8$id_lev08)]

# do we need to rescale 0 to 1 too? technically the data is already like that. i think no

# intersect with vector study area
study_area <- st_read("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp")
final_scores_hydrobasins_intersected <- final_scores_hydrobasins %>%
  st_intersection(., study_area)


# read in study area raster to rasterize to
study_area_rast <- rast("/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_raster_mask_lvl_0_90m_with_na.tif")

# rasterize status score to 90 m (100 m until new mask is made)
streams_rast <- terra::rasterize(final_scores_hydrobasins_intersected,
                                 study_area_rast,
                                 field = "status",
                                 background = NA,
                                 fun = "mean")

# plot
plot(streams_rast,
     main = "Streamflow Status Scores")

# write raster
writeRaster(streams_rast, "/home/shares/wwri-wildfire/final_layers/2024/water/indicators/streamflow_status_scores_2024.tif", overwrite = TRUE)



# testing
score_quantity <- function(flow, p1, lower_bound, upper_bound, p99) {
  m1 <- 1 / (lower_bound - p1)
  b1 <- -m1 * p1
  m2 <- -1 / (p99 - upper_bound)
  b2 <- 1 - m2 * upper_bound
  
  case_when(
    flow <= p1 | flow >= p99 ~ 0,
    flow >= lower_bound & flow <= upper_bound ~ 1,
    flow > p1 & flow < lower_bound ~ m1 * flow + b1,
    flow < p99 & flow > upper_bound ~ m2 * flow + b2,
    TRUE ~ NA_real_
  )
}
curve(score_quantity(x, p1 = 10, lower_bound = 20, upper_bound = 80, p99 = 90),
      from = 0, to = 100, n = 1000, ylab = "Score", xlab = "Annual Flow", col = "orange")
abline(v = c(10, 20, 80, 90), col = "grey", lty = 2)