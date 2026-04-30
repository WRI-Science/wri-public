#### Goal ####
# the goal of this script is to rescale the perent protected calculation from 
# step 1 to be between 0-1 depending on the IUCN category we want to include up to.

#### Packages ####
library(tidyverse)

#### Setup and File Paths ####
natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_base_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

ecoregion_protection_summary_path <- paste0(
  natural_habitats_base_path,
  "int/cec_protected_areas/ecoregion_protection_summary.csv"
)

# save path for the rescaled data
out_rescaled_csv <- paste0(
  natural_habitats_base_path,
  "int/cec_protected_areas/ecoregion_protection_rescaled.csv"
)

#### Main Processing ####
# read in the ecoregion protection summary
ecoregion_protection_summary <- read_csv(ecoregion_protection_summary_path)

# we want to only include categories Ia, Ib, and II right now
iucn_categories <- c("Ia", "Ib", "II")
# Build a regex that matches only column names ending with one of the desired categories
pattern <- paste0("(_", iucn_categories, ")$")

# Create a logical vector that is TRUE for names that match any of the IUCN categories
cols_to_select <- names(ecoregion_protection_summary) %>%
  str_subset(paste(pattern, collapse = "|"))

# select the columns that contain these categories and the ecoregion NA_L3
ecoregion_protection_rescaled <- ecoregion_protection_summary %>%
  select(NA_L3CODE, NA_L3NAME, NA_L3KEY, all_of(cols_to_select)) %>% 
  # calculate the total percent protected for each ecoregion
  mutate(total_percent_protected = rowSums(across(starts_with("pct_protected_")), na.rm = TRUE)) %>%
  # rescale the total percent protected to be between 0 and 1, 0% = 0 and 30% and greater = 1
  mutate(rescaled_protected = case_when(
    total_percent_protected < 30 ~ total_percent_protected / 30,
    TRUE ~ 1
  ))

# uncomment to look at a simple histogram of the rescaled protected values
# hist(ecoregion_protection_rescaled$rescaled_protected, 
#      main = "Rescaled Protected Areas", 
#      xlab = "Rescaled Protection Level", 
#      breaks = 20, 
#      col = "lightblue")

# save the rescaled data to a new CSV file
write_csv(ecoregion_protection_rescaled, out_rescaled_csv)