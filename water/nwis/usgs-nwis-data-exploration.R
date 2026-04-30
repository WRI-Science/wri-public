wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(tidyverse)

nwis_data <- read_csv(file.path(wri_project_root, "data", "water-domain-data", "int", "study-area-usgs-nwis-data-with-coords-new-source-with-buffer.csv"))
old_nwis_data <- read_csv(file.path(wri_project_root, "data", "water-domain-data", "int", "study-area-usgs-nwis-data-with-coords.csv"))
length(unique(nwis_data$site_no))
sorted_years <- sort(unique(nwis_data$year_nu), decreasing = TRUE)
print(sorted_years)

nwis_data_30_yr <- nwis_data %>%
  filter(year_nu %in% 1991:2020) # == 2024, %in% 2000:2024
length(unique(nwis_data_30_yr$site_no))


check_param_options <- nwis_data_30_yr %>%
  filter(parameter_cd %in% c("00062", "62611", "62614", "62615", "72019", "72150", "72275", "72335",
                             "00010", "70301", "00054", "00060", "00618", "80180", "30208",
                             "80155", "80156", "80225", "99409", "80154", "80297", "80299")) # & month_nu == 2
length(unique(check_param_options$site_no))
table(check_param_options$parameter_cd)

duplicate_sites <- check_param_options %>%
  filter(month_nu == 6 & year_nu == 2020) %>% # just because a lot of sites will have duplicates if i don't just choose one month
  group_by(site_no) %>% 
  filter(n() > 1)

# not every site id has data for every month or every year
# NA a month don't penalize it, don't gapfill

# coverage by site by year
coverage_summary <- nwis_data_30_yr %>%
  filter(is.na(loc_web_ds)) %>% # gets rid of extra time series, do we want that? maybe we want the average?
  group_by(site_no, year_nu, parameter_cd) %>%
  summarise(record_count = n(), .groups = "drop")  # Count records per group

print(head(coverage_summary))

table(coverage_summary$record_count)
table(nwis_data_30_yr$huc_cd)

stream_flow_opts <- check_param_options %>%
  filter(parameter_cd %in% c("00060", "30208")) %>%
  filter(month_nu == 6 & year_nu == 2020) %>% # just because a lot of sites will have duplicates if i don't just choose one month
  group_by(site_no) %>% 
  filter(n() > 1)
# i think 30208 only occurs at sites that already have 00060
level_opts <- check_param_options %>%
  filter(parameter_cd %in% c("00062", "62611", "62614", "62615", "72019", "72150", "72275", "72335")) %>%
  filter(month_nu == 6 & year_nu == 2020) %>% # just because a lot of sites will have duplicates if i don't just choose one month
  group_by(site_no) %>% 
  filter(n() > 1)

sediment_opts <- check_param_options %>%
  filter(parameter_cd %in% c("80180", "80155", "80156", "80225", "99409", "80154", "80297", "80299")) %>%
  filter(month_nu == 6 & year_nu == 2020) %>% # just because a lot of sites will have duplicates if i don't just choose one month
  group_by(site_no) %>% 
  filter(n() > 1)
# 80154 and 80155 have best coverage by far

# look at variety of loc codes
loc_codes <- nwis_data_30_yr %>%
  filter(!is.na(loc_web_ds)) %>%
  filter(site_tp_cd %in% c("LK", "GW", "GW-MW")) %>%
  filter(parameter_cd %in% c("80180", "80155", "80156", "80225", "99409", "80154", "80297", "80299", "00062", "62611", "62614", "62615", "72019", "72150", "72275", "72335", "00060", "30208", "00010", "70301", "00054", "00618"))
unique(loc_codes$loc_web_ds)
test <- loc_codes %>%
  filter(loc_web_ds == "12 ft") # "[Discontinued May 31, 2015]"

# filter to the a site that test gives us
test_site <- loc_codes %>%
  filter(site_no == 295554095093402) # 375327097285402

# compare old vs. new site gathering method
old_nwis_data_30_yr <- old_nwis_data %>%
  filter(year_nu %in% 1991:2020)

setdiff(old_nwis_data_30_yr$site_no, nwis_data_30_yr$site_no)
diff_active_sites <- setdiff(nwis_data_30_yr$site_no, old_nwis_data_30_yr$site_no)

are_these_sites_active <- nwis_data %>%
  filter(site_no %in% diff_active_sites) %>%
  filter(year_nu == 2025) # no 2025 obs in the old dataset. not sure if this confirms these sites aren't active anymore though
unique(are_these_sites_active$year_nu)


# look at groundwater and lakes
gw <- check_param_options %>%
  filter(parameter_cd %in% c("00062", "62611", "62614", "62615", "72019", "72150", "72275", "72335") & site_tp_cd == "GW")
table(gw$parameter_cd)
# mostly 72019

lk <- check_param_options %>%
  filter(parameter_cd %in% c("00062", "62611", "62614", "62615", "72019", "72150", "72275", "72335") & site_tp_cd == "LK")
table(lk$parameter_cd)
# mostly 62614


# start looking how sites align hydrosheds
# not expecting more than 1