library(cancensus)
library(terra)
library(sf)
library(tidyverse)

# canada vars
# poverty = https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810011301 - 98100113-eng
# renter = https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810024301&pickMembers%5B0%5D=1.3&pickMembers%5B1%5D=2.1&pickMembers%5B2%5D=3.1&pickMembers%5B3%5D=4.1
# greater_than_200k = on cancensus v_CA21_943; https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?LANG=E&GENDERlist=1&STATISTIClist=1,4&DGUIDlist=2021A000011124&HEADERlist=9&SearchText=Canada. v_CA21_923 is total.
# age_65_plus =  on cancensus v_CA21_251 total 65 and over. v_CA21_8 is total.
# disability = https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310037401 (province level) - 13100374-eng
# no_vehicle = not available
# total population = included by default with any cancensus pull; also on cancensus v_CA21_1; also in the data Mona downloaded

#### PULL ALL CANCENSUS DATA NEEDED ####
# follow instructions here to get key: https://mountainmath.github.io/cancensus/
# cm_api_key <- Sys.getenv('CM_API_KEY')
# set_cancensus_api_key(cm_api_key, install = TRUE) # only need to do once
# set_cancensus_cache_path("/home/egg/tmp", install = TRUE) # only need to do once
View(list_census_datasets())
View(list_census_regions("CA21")) # CA21
#regions_of_interest <- list_census_regions("CA21") %>%
#filter((PR_UID %in% c(59, 60)) & (level %in% c("CD", "CSD")))
View(list_census_vectors("CA21"))

regions_of_interest <- c("59", "60")
variables_of_interest <- c("v_CA21_943", "v_CA21_923", "v_CA21_251", "v_CA21_8")

census_data_filtered <- get_census(dataset = 'CA21', 
                                   regions = list(PR = regions_of_interest),
                                   vectors = variables_of_interest, 
                                   level = 'CSD', 
                                   geo_format = "sf") %>%
  select(population = Population, geo_id = GeoUID, greater_than_200k = `v_CA21_943: $200,000 and over`, income_total = `v_CA21_923: Number of after-tax income recipients aged 15 years and over in private households in 2019`, age_65_plus = `v_CA21_251: 65 years and over`, age_total = `v_CA21_8: Total - Age`) %>%
  mutate(greater_than_200k = greater_than_200k/income_total,
         age_65_plus = age_65_plus/age_total) %>%
  select(-income_total, -age_total)

# saving to ensure we have the data
st_write(census_data_filtered, "/home/shares/wwri-wildfire/data/communities/raw/2024/canada_census/cancensus_2021_variables_communities.gpkg")

# remove spatial data for now after saving
census_data_filtered <- census_data_filtered %>%
  st_drop_geometry()

census_data_filtered_pop <- census_data_filtered %>%
  select(geo_id, population) # for use in later dataframes

# sum(is.na(census_data_filtered$`v_CA21_943: $200,000 and over`))
# sum(is.na(census_data_filtered$`v_CA21_923: Number of after-tax income recipients aged 15 years and over in private households in 2019`))
# sum(is.na(census_data_filtered$`v_CA21_251: 65 years and over`))
# sum(is.na(census_data_filtered$`v_CA21_8: Total - Age`))
# 391 for first two, 226 for second two

census_data_filtered_gf <- get_census(dataset='CA21', regions=list(PR = regions_of_interest),
                                          vectors=variables_of_interest, level='CD', geo_format = "sf") %>%
  select(population_gf = Population, geo_id_gf = GeoUID, greater_than_200k_gf = `v_CA21_943: $200,000 and over`, income_total_gf = `v_CA21_923: Number of after-tax income recipients aged 15 years and over in private households in 2019`, age_65_plus_gf = `v_CA21_251: 65 years and over`, age_total_gf = `v_CA21_8: Total - Age`) %>%
  mutate(greater_than_200k_gf = greater_than_200k_gf/income_total_gf,
         age_65_plus_gf = age_65_plus_gf/age_total_gf) %>%
  select(-income_total_gf, -age_total_gf)

# saving to ensure we have the data
st_write(census_data_filtered_gf, "/home/shares/wwri-wildfire/data/communities/raw/2024/canada_census/cancensus_variables_communities_gf.gpkg")

# remove spatial data for now after saving
census_data_filtered_gf <- census_data_filtered_gf %>%
  st_drop_geometry()

# sum(is.na(census_data_filtered_gf$`v_CA21_943: $200,000 and over`))
# sum(is.na(census_data_filtered_gf$`v_CA21_923: Number of after-tax income recipients aged 15 years and over in private households in 2019`))
# sum(is.na(census_data_filtered_gf$`v_CA21_251: 65 years and over`))
# sum(is.na(census_data_filtered_gf$`v_CA21_8: Total - Age`))
# all 0, good

cols_to_fill <- c("age_65_plus", "greater_than_200k")

census_data_filtered_full <- census_data_filtered %>%
  mutate(geo_id_gf = substr(geo_id, 1, 4)) %>%
  left_join(census_data_filtered_gf,
            by = c("geo_id_gf")
  ) %>% # add gf (division) data to the tract data
  mutate(across(all_of(cols_to_fill), 
                ~ ifelse(is.na(.) & population > 0, get(paste0(cur_column(), "_gf")), .))
  ) %>% # fill NAs when population > 0 with values from corresponding division level; can also use case_when if there are more cases
  select(-ends_with("_gf")) # get rid of the gf columns that are no longer needed; before removing, check that things filled properly

#### POPULATION ####
# not needed because population is included on cancensus
# prepare overall population data, primarily to determine if data should be gapfilled (only pops > 0 get gapfilled)
# population_data_bc <- read_csv("/home/shares/wwri-wildfire/data/air-quality/vulnerable_populations/98-401-X2021026_eng_CSV_YT_subdivisions_only/98-401-X2021026_English_CSV_data.csv")
# population_data_yk <- read_csv("/home/shares/wwri-wildfire/data/air-quality/vulnerable_populations/98-401-X2021025_eng_CSV_BC_subdivisions_only/98-401-X2021025_English_CSV_data.csv")

# # explore some variables
# View(population_data_bc %>%
#   filter(str_detect(CHARACTERISTIC_NAME, "basket")))

#### RENTERS ####
# read in canada renter vs. owner data
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810024301&pickMembers%5B0%5D=1.3&pickMembers%5B1%5D=2.1&pickMembers%5B2%5D=3.1&pickMembers%5B3%5D=4.1
base_file_path <- "/home/shares/wwri-wildfire"
canadian_housing_path <- file.path(base_file_path, "data/livelihoods/raw/canada-housing-burden/98100243.csv")

# read in canada housing file and select only yukon and BC
canada_owners_raw <- read_csv(canadian_housing_path) %>%
  mutate(prov = substr(DGUID, 10, 11)) %>%
  filter(prov %in% c("59", "60"))

# these are what we are interested in:
# [11] "Tenure including presence of mortgage payments and subsidized housing (8):Owner[2]"
# [17] "Tenure including presence of mortgage payments and subsidized housing (8):Renter[5]"

# get the variables with are interested in and calculate relative proportions of renters vs. owners
# we grab census subdivisions here because that is the finest resolution available -- this does not perfectly match US tracts, but is the best equivalent in canada
canada_owners <- canada_owners_raw %>%
  filter(`Age of primary household maintainer (9)` == "Total - Age of primary household maintainer",
         `Household type including census family structure (9)` == "Total - Household type including family structure", 
         `Statistics (3C)` == "Number of private households",
         `Housing indicators (6)` == "Total - Housing indicators") %>%
  select(prov, geo_id = DGUID, name = GEO, metric = "Housing indicators (6)", owning_households = "Tenure including presence of mortgage payments and subsidized housing (8):Owner[2]", renting_households = "Tenure including presence of mortgage payments and subsidized housing (8):Renter[5]") %>%
  group_by(geo_id, name, prov) %>%
  summarize(owners_count = sum(owning_households),  # na.rm = TRUE
            renters_count = sum(renting_households), # na.rm = TRUE
            total = owners_count + renters_count,
            #owners = owners_count/total, # if wanted
            owners = owners_count/total,
            .groups = 'drop') %>%
  select(geo_id, owners) # name, prov, 
  


#filter(nchar(geo_id) == 16) # get only census subdivisions
#filter(nchar(geo_id) == 13) # get only census divisions

canada_owners_subdiv <- canada_owners %>%
  filter(nchar(geo_id) == 16) %>%
  mutate(geo_id_gf = substr(geo_id, 10, 13)) %>%
  mutate(geo_id = gsub("2021A0005", "", geo_id)) %>%
  left_join(census_data_filtered_pop, by = "geo_id") 

canada_owners_gf <- canada_owners %>%
  filter(nchar(geo_id) == 13) %>%
  rename_with(~ paste0(.x, "_gf")) %>%
  mutate(geo_id_gf = substr(geo_id_gf, 10, 13))



# keep non-NA census subdivision values and assign the census division values to subdivision level if needed (this is more equivalent to US tracts)
# use older data instead? ie. Cape Mudge 10 has 2016 data on this - currently don't want to do this
# nchar(geo_id) == 13 -> division
# nchar(geo_id) == 16 -> subdivision
# chars 12 & 13 correspond to division; this can repeat in a different province. ie. 01 can be in 59 and 60, so the whole code (10 to 13) is important.
cols_to_fill <- c("owners") # can add more if desired

# for these columns, if subdivision is NA, take value from division level
canada_owners_gapfilled <- canada_owners_subdiv %>%
  left_join(canada_owners_gf,
            by = c("geo_id_gf")
  ) %>% # add gf (county) data to the tract data
  mutate(across(all_of(cols_to_fill), 
                ~ ifelse(is.na(.) & population > 0, get(paste0(cur_column(), "_gf")), .))) %>% # fill NAs when population > 0 with values from corresponding county level; can also use case_when if there are more cases
  select(-ends_with("_gf")) # get rid of the gf columns that are no longer needed; before removing, check that things filled properly
########


#### POVERTY ####
base_file_path <- "/home/shares/wwri-wildfire"
canadian_poverty_path <- file.path(base_file_path, "data/communities/raw/2024/canada_census/98100113-eng/98100113.csv")

# read in canada poverty file and select only yukon and BC
canada_poverty_raw <- read_csv(canadian_poverty_path) %>%
  mutate(prov = substr(DGUID, 10, 11)) %>%
  filter(prov %in% c("59", "60"))

# we want this variable: "Individual MBM poverty status (7):Poverty rate (%)[6]"
canada_poverty <- canada_poverty_raw %>%
  filter(`Gender (3a)` == "Total - Gender",
         `Economic family characteristics of persons (13)` == "Total - Household living arrangements for persons not in economic families", 
         `Statistics (6C)` == "2020", # may want 2015 too down the line for backcalculating
         `Age (8)` == "Total - Age") %>%
  select(geo_id = DGUID, poverty_rate = "Individual MBM poverty status (7):Poverty rate (%)[6]") %>%
  mutate(poverty_rate = poverty_rate/100)

canada_poverty_subdiv <- canada_poverty %>%
  filter(nchar(geo_id) == 16) %>%
  mutate(geo_id_gf = substr(geo_id, 10, 13)) %>%
  mutate(geo_id = gsub("2021A0005", "", geo_id)) %>%
  left_join(census_data_filtered_pop, by = "geo_id") 

canada_poverty_gf <- canada_poverty %>%
  filter(nchar(geo_id) == 13) %>%
  rename_with(~ paste0(.x, "_gf")) %>%
  mutate(geo_id_gf = substr(geo_id_gf, 10, 13))
  
# keep non-NA census subdivision values and assign the census division values to subdivision level if needed (this is more equivalent to US tracts)
# use older data instead? ie. Cape Mudge 10 has 2016 data on this - currently don't want to do this
# nchar(geo_id) == 13 -> division
# nchar(geo_id) == 16 -> subdivision
# chars 12 & 13 correspond to division; this can repeat in a different province. ie. 01 can be in 59 and 60, so the whole code (10 to 13) is important.
cols_to_fill <- c("poverty_rate") # can add more if desired

# for these columns, if subdivision is NA, take value from division level
canada_poverty_gapfilled <- canada_poverty_subdiv %>%
  left_join(canada_poverty_gf,
            by = c("geo_id_gf")
  ) %>% # add gf (county) data to the tract data
  mutate(across(all_of(cols_to_fill), 
                ~ ifelse(is.na(.) & population > 0, get(paste0(cur_column(), "_gf")), .))) %>% # fill NAs when population > 0 with values from corresponding county level; can also use case_when if there are more cases
  select(-ends_with("_gf")) %>% # get rid of the gf columns that are no longer needed; before removing, check that things filled properly
  left_join(canada_owners_gapfilled, by = c("geo_id", "population"))
########


#### DISABILITY ####
# read in province level data
base_file_path <- "/home/shares/wwri-wildfire"
canadian_disability_path <- file.path(base_file_path, "data/communities/raw/2024/canada_census/13100374-eng/13100374.csv")

# read in canada poverty file and select only yukon and BC
canada_disability_raw <- read_csv(canadian_disability_path) %>%
  mutate(prov = substr(DGUID, 10, 11)) %>%
  filter(prov %in% c("59", "60"))

# we want this variable: "VALUE"
canada_disability <- canada_disability_raw %>%
  filter(`REF_DATE` == "2022", # can also grab 2017 if wanting to backcalculate
         `Age group` == "Total, 15 years and over",
         `Gender` == "Total, gender", 
         `Disability` == "Persons with disabilities",
         `Estimates` == "Percentage of persons",
         `UOM` == "Percent") %>%
  select(geo_id_gf = prov, disability_rate = "VALUE") %>%
  mutate(disability_rate = disability_rate/100)

# cast province level data onto the subdivisions
canada_disability_gapfilled <- canada_poverty_gapfilled %>%
  mutate(geo_id_gf = substr(geo_id, 1, 2)) %>%
  left_join(canada_disability, by = c("geo_id_gf")) %>%
  mutate(disability_rate = ifelse(population == 0, NA, disability_rate))

# add on final dataframe from the beginning cancensus
can_census_variables_people_full <- canada_disability_gapfilled %>%
  left_join(census_data_filtered_full, by = c("geo_id", "population")) %>%
  select(geo_id, population, greater_than_200k, age_65_plus, owner = owners, poverty = poverty_rate, disability = disability_rate) %>% # final clean up
  mutate(no_vehicle = NA) # for matching with ACS data
  
write_csv(can_census_variables_people_full, "/home/shares/wwri-wildfire/data/communities/int/2024/canada_census/can_census_variables_communities_full.csv") # write out correct direction instead?