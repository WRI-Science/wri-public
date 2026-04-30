# this will retrieve monthly (or a different time step) nwis data for the states specified

library(dataRetrieval) # for usgs nwis data
library(tidyverse)

# read in manually created site list for states of interest
# site_ids <- read_csv("/home/shares/wwri-wildfire/data/water-domain-data/raw/usgs-nwis-site-ids.csv")
# this list might be presently active sites? but we want all that have ever been active

# create list of states we are interested in (add a buffer because of hydrosheds)
interested_states <- c(
  "NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK", "TX", "OK", "KS", "NE", "SD", "ND",
  "AL", "AR", "CT", "DE", "FL", "GA", "IL", "IN", "IA", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
  "MO", "NH", "NJ", "NY", "NC", "OH", "PA", "RI", "SC", "TN", "VT", "VA", "WI", "WV"
) # everything but Hawaii
#interested_states <- c("NM", "AZ", "CA", "NV", "UT", "CO", "MT", "ID", "WY", "WA", "OR", "AK", "TX", "OK", "KS", "NE", "SD", "ND")

# list to store site id results
site_ids_list <- list()

# get site info for the states of interest
for (state in interested_states) {
  # find site ids for state
  state_sites <- whatNWISsites(stateCd = state)
  
  # if data is returned for the state
  if (nrow(state_sites) > 0) {
    # create column with the state code
    state_sites$state_code <- state
    
    # add site results to the list
    site_ids_list[[length(site_ids_list) + 1]] <- state_sites
  }
}

# make all site info one dataframe
site_ids <- bind_rows(site_ids_list) %>%
  select(-colocated, -queryTime, -station_nm, -agency_cd) # remove unneeded columns

# get only the site ids as an object
site_ids_for_loop <- site_ids$site_no
  # %>% site_ids$site_id %>%
  # str_pad(width = 8, side = "left", pad = "0") # google sheets removed 0s in front, so add them back in to any id whose character length is less than 8, if using the google sheet data

# can do max 10 sites per data request
chunk_size <- 10

# create empty list for results
output_list <- list()

# go through all site numbers in intervals of 10
for (i in seq(1, length(site_ids_for_loop), by = chunk_size)) {
  # grab current subset of 10
  site_subset <- site_ids_for_loop[i:min(i + chunk_size - 1, length(site_ids_for_loop))]
  
  # get the desired data for those 10
  data_chunk <- readNWISdata(
    sites = site_subset,
    statReportType = "monthly", # toggle between monthly and annual for example
    service = "stat")
  
  if (nrow(data_chunk) == 0) {
    next
  }
    
    # get extra info for the sites
    site_metadata <- readNWISsite(site_subset)
    
    # grab site number, lat/long, and in case: altitude, and huc code
    site_coords <- site_metadata[, c("site_no", "alt_va", "dec_coord_datum_cd", "huc_cd")]
    
    # add this info to the data subset
    data_chunk_with_coords <- merge(data_chunk, site_coords, by = "site_no", all.x = TRUE) %>% merge(site_ids, by = "site_no", all.x = TRUE)
    
  # add it all to the list
  output_list[[length(output_list) + 1]] <- data_chunk_with_coords
}

# write list as rds to check what's wrong
saveRDS(output_list, "/home/shares/wwri-wildfire/data/water-domain-data/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us.rds")

# make the list one big dataframe
outputData <- do.call(rbind, output_list) # 2274950 vs 2275005 when not doing "all"

# write out the list
write_csv(outputData, "/home/shares/wwri-wildfire/data/water-domain-data/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us.csv")


# ### get parameter code definitions
# # see codes in the data
# unique(outputData$parameter_cd)
# 
# # read in defs for those codes
# param_code_defs <- read_tsv("/home/shares/wwri-wildfire/data/water-domain-data/raw/parameter_cd_query.txt", skip = 7) %>%
#   filter(parm_cd %in% unique(outputData$parameter_cd))
# 
# # write out the parameter code information for codes present in the data pulled
# write_csv(param_code_defs, "/home/shares/wwri-wildfire/data/water-domain-data/int/param_code_defs_in_data.csv")