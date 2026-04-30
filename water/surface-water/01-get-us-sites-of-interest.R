library(tidyverse)

# between 1991-2020, have at least 20 years that have 12 months

# add nwis pulling code here? instead of separate script?

# site filtering
us_stream_data <- read_csv("/home/shares/wwri-wildfire/data/water/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us_2024.csv") %>%
  filter(parameter_cd == "00060")
# all_hcdn_2009_sites <- read_csv("/home/shares/wwri-wildfire/data/water/int/all_hcdn_2009_sites.csv")

# us_stream_hcdn_2009 <- us_stream_data %>%
#   filter(site_no %in% c(all_hcdn_2009_sites$STAID)) #%>%
# filter(year_nu %in% c(1991, 2020, 2023)) %>%
# group_by(site_no) %>%
# summarise(
#   count_1991 = sum(year_nu == 1991),
#   count_2020 = sum(year_nu == 2020),
#   count_2023 = sum(year_nu == 2023)
# ) %>%
# filter(count_1991 > 0 & count_2020 > 0 & count_2023 > 0) %>%
# pull(site_no)
length(unique(us_stream_data$site_no)) # 717, 677 with commented out filtering

# monthly_data_1991_2020 <- us_stream_data %>%
#   filter(year_nu %in% 1991:2020)

# Keep sites that have at least 20 years that have 12 months between 1991-2020
complete_1991_2020_stations <- us_stream_data %>%
  filter(year_nu %in% 1991:2020) %>%
  count(site_no, year_nu, month_nu, name = "n_obs") %>%           # Count obs per month
  # filter(n_obs >= 1) %>% # this adds 17 sites that meet all reqs, but we don't know exactly why they have more than 1 obs in at least one month
  filter(n_obs == 1) %>%                                          # Keep only months with exactly 1 obs
  count(site_no, year_nu, name = "n_valid_months") %>%            # Count valid months per year
  filter(n_valid_months == 12) %>%                                # Keep only full years
  count(site_no, name = "n_full_years") %>%                       # Count full years per site
  filter(n_full_years >= 20) %>%                                  # Sites with ≥ 20 good years
  pull(site_no)



# filter to year = 2024 (recent year of interest) and get those station numbers
monthly_data_w_site_info_of_interest_recent <- us_stream_data %>%
  filter(year_nu == 2024 & site_no %in% complete_1991_2020_stations) %>%
  count(site_no, month_nu, name = "n_obs") %>%                    # Count obs per month
  filter(n_obs == 1) %>%                                          # Keep only those with exactly 1 obs
  count(site_no, name = "n_valid_months") %>%                     # Count valid months per site
  filter(n_valid_months == 12) %>%
  pull(site_no)


us_stream_data_filtered <- us_stream_data %>%
  filter(site_no %in% monthly_data_w_site_info_of_interest_recent) %>%
  filter(year_nu %in% c(1991:2020, 2024)) %>%
  group_by(site_no, year_nu) %>%
  mutate(full_year = if_else(n_distinct(month_nu) == 12, 1, 0)) %>%
  ungroup()
  

# write out data for use elsewhere
write_csv(us_stream_data_filtered, "/home/shares/wwri-wildfire/data/water/int/us-streamflow-data-30-yr-and-recent_2024.csv")
