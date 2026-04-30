# this will retrieve monthly (or a different time step) nwis data for the states specified, with parallelization

library(dataRetrieval) # for usgs nwis data
library(tidyverse)
library(foreach)
library(doParallel)

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

# split site ids into chunks of 10 (necessary for API)
chunk_size <- 10
site_id_chunks <- split(site_ids_for_loop, ceiling(seq_along(site_ids_for_loop) / chunk_size))

# start parallel backend
n_cores <- 25
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# run parallelization w/ foreach
output_list <- foreach(site_subset = site_id_chunks, .packages = c("dataRetrieval", "dplyr")) %dopar% {
  data_chunk <- readNWISdata(
    sites = site_subset,
    statReportType = "monthly", # can switch to annual, daily, etc
    service = "stat"
  )
  
  if (nrow(data_chunk) == 0) return(NULL) # remove NULLs later
  
  # get extra info for the sites
  site_metadata <- readNWISsite(site_subset)
  
  # grab site number, lat/long, and in case: altitude, and huc code
  site_coords <- site_metadata[, c("site_no", "alt_va", "dec_coord_datum_cd", "huc_cd")]
  
  # make all site info one dataframe
  data_chunk_with_coords <- merge(data_chunk, site_coords, by = "site_no", all.x = TRUE) %>%
    merge(site_ids, by = "site_no", all.x = TRUE)
  
  return(data_chunk_with_coords)
}

# stop cluster
stopCluster(cl)

# remove any null results if needed
output_list <- output_list[!sapply(output_list, is.null)]

# save as rds because it's a list of dfs (need to analyze before combining due to column name diffs)
saveRDS(output_list, "/home/shares/wwri-wildfire/data/water/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us-parallel-new_2024.rds")

# check out results
output_list <- readRDS("/home/shares/wwri-wildfire/data/water/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us-parallel-new_2024.rds")

# get list of the different colnames and coltypes in each df
colnames_list <- lapply(output_list, names)
coltypes_list <- lapply(output_list, function(df) sapply(df, class))
unique_structures <- unique(lapply(colnames_list, sort))
length(unique_structures)
unique_types <- unique(lapply(coltypes_list, function(x) {
  # sort by column name to help normalize structure
  x[order(names(x))]
}))

for (i in seq_along(unique_structures)) {
  cat(paste0("Structure ", i, ":\n"))
  print(unique_structures[[i]])
  cat("\n")
} # site 06765000 is responsible for the unique structure; it is odd because the data received for that site appear to daily statistics. it will get dropped out in our analyses since it does not have year_nu
for (i in seq_along(unique_types)) {
  cat(paste0("Structure ", i, " (column types):\n"))
  print(unique_types[[i]])
  cat("\n")
}

output_list_fixed <- lapply(output_list, function(df) {
  if ("loc_web_ds" %in% names(df)) {
    df$loc_web_ds <- as.character(df$loc_web_ds) # make all of these columns character because most flexible
  }
  return(df)
})

output_df <- bind_rows(output_list_fixed)

# write out the df
write_csv(output_df, "/home/shares/wwri-wildfire/data/water/int/study-area-usgs-nwis-data-with-coords-new-source-entire-us_2024.csv") # problems are all with columns we don't use, so can ignore for now