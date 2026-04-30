wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

# Load the necessary libraries
library(natserv)
library(tibble)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Set base directories
data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species")
raw_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "raw")
intermediate_data_file_path <- file.path(wri_project_root, "data", "sense_of_place", "iconic_species", "intermediate")
final_layers_file_path <- file.path(wri_project_root, "final_layers", "2024", "sense_of_place", "iconic_species")
multi_domain_data_file_path <- file.path(wri_project_root, "data", "multi_domain_data")

#### Data Layers ####
iconic_species_list <- read_csv(file.path(raw_data_file_path, "iconic_species_list - Sheet1 (16).csv"))

wwri_subnations <- c("AK", "CA", "OR", "WA", "NV", "AZ", "NM", "CO", "UT", "ID", "MT", "WY", "BC", "YT")

#### Functions ####

# Extract species data function from NatureServe API  
# Identify species synonyms if species cannot be found
# ClassificationStatus == "Standard" (see Github issue regarding Standard vs. Provisional classification)
extract_species_data <- function(species_list) {
  # Create an empty list to store the results
  result_list <- list()
  
  # Loop through each species in the list
  for (species in species_list) {
    # Create a new export job for the species
    export_job_id <- tryCatch({
      ns_export(text = species)
    }, error = function(e) {
      print(paste("Error creating export job for species:", species, ":", e$message))
      return(NULL)
    })
    
    if (is.null(export_job_id)) {
      next  # Skip to the next species if the job creation fails
    }
    
    # Poll the status until the job is finished or times out
    max_attempts <- 10
    attempt <- 1
    job_finished <- FALSE
    
    while (attempt <= max_attempts) {
      Sys.sleep(2)  # Wait before checking status again
      res <- tryCatch({
        ns_export_status(export_job_id)
      }, error = function(e) {
        print(paste("Error checking export status for species:", species, ":", e$message))
        return(NULL)
      })
      
      if (is.null(res)) {
        break
      }
      
      if (res$state == "Finished") {
        job_finished <- TRUE
        break
      } else {
        print(paste("Export job for species", species, "is still in progress. Attempt", attempt, "of", max_attempts))
        attempt <- attempt + 1
      }
    }
    
    if (!job_finished) {
      print(paste("Export job for species", species, "did not complete successfully."))
      next
    }
    
    # Download and read the JSON data
    json_url <- res$data$url
    species_data <- tryCatch({
      tibble::as_tibble(jsonlite::fromJSON(json_url))
    }, error = function(e) {
      print(paste("Error downloading JSON for species:", species, ":", e$message))
      return(NULL)
    })
    
    if (is.null(species_data)) {
      next  # Skip to the next species if JSON download fails
    }
    
    # Find rows where scientificName matches exactly and classificationStatus is "Standard"
    matching_rows <- species_data[species_data$scientificName == species & species_data$classificationStatus == "Standard", ]
    
    if (nrow(matching_rows) == 0) {
      print(paste("No exact match found for species:", species, "with classificationStatus 'Standard'"))
      
      # Check if the species is listed in the synonyms column
      synonym_matches <- species_data[sapply(species_data$speciesGlobal$synonyms, function(syn_list) species %in% syn_list), ]
      
      if (nrow(synonym_matches) > 0) {
        print(paste("Found", species, "in speciesGlobal$synonyms. Adding corresponding record."))
        matching_rows <- synonym_matches
      } else {
        next  # Skip to the next species if no matches are found
      }
    }
    
    # Append all matching rows to the result list
    for (row_index in 1:nrow(matching_rows)) {
      matching_row <- matching_rows[row_index, ]
      result_list <- append(result_list, list(as.data.frame(matching_row)))
    }
  }
  
  # Combine the list into a data frame and return it
  result_df <- do.call(dplyr::bind_rows, result_list)
  return(result_df)
}


#### TEST for: individual search for species ####

# # Create a new export job for the species
# export_job_id <- ns_export(text = "Cervus canadensis")
# print(export_job_id)
# 
# # Check the status of the export job
# res <- ns_export_status(export_job_id)
# 
# # Check if the export job is finished
# if (res$state == "Finished") {
#   print("The export job is finished.")
# } else {
#   print("The export job is still in progress.")
# }
# print(res$data$url)
# 
# json_url <- res$data$url
# 
# # Download and read the JSON data
# species_data <- tibble::as_tibble(jsonlite::fromJSON(json_url))
# species_data



#### Function to obtain data for full list of species through natserv API ####

# Define the list of species to process from the NatureServe column
isl <- iconic_species_list$ns_sci_name # 120 species 

# Check for duplicates in the list and remove them
isl_unique <- unique(isl) # 103  

# Extract the data for the given list of species
# June 10, 2025
natserv_species_result_df <- extract_species_data(isl_unique) # 103 obs. 

# check that unique count of species is the same 
length(unique(natserv_species_result_df$scientificName)) # returns 104 bc for some reason adds Myosotis asiatica, so need to remove it

# Remove the Myosotis asiatica species
natserv_species_result_df <- natserv_species_result_df %>%
  filter(scientificName != "Myosotis asiatica") # 103 obs.

# Find species in natserv_species_result_df but not in isl_unique
missing_in_isl <- setdiff(natserv_species_result_df$scientificName, isl_unique)
print("Species in natserv_species_result_df but not in isl_unique:")
print(missing_in_isl)

# Find species in isl_unique but not in natserv_species_result_df
extra_in_isl <- setdiff(isl_unique, natserv_species_result_df$scientificName)
print("Species in isl_unique but not in natserv_species_result_df:")
print(extra_in_isl)

# Unnest the 'nations' column
wwri1 <- unnest(natserv_species_result_df, cols = c("nations"))

# Ensure 'subnations' is a data frame for further processing
wwri1 <- wwri1 %>%
  mutate(subnations = purrr::map(subnations, function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      as.data.frame(x, stringsAsFactors = FALSE)
    } else {
      x
    }
  }))

# Unnest the 'subnations' column with name separation
wwri2 <- unnest(wwri1, cols = "subnations", names_sep = "_subnation")

#### remove entries with subnation outside of wwri ####

# species observations
wwri_iconic_species_filtered <- wwri2 %>%
  filter(subnations_subnationsubnationCode %in% wwri_subnations)

# unnest additional columns to write into a csv 
wwri_iconic_species_filtered <- wwri_iconic_species_filtered %>%
  unnest(cols = c(speciesGlobal, ecosystemGlobal))  # Replace 'column_20' and 'column_22' with the actual column names

# Remove columns that contain only NA values
wwri_iconic_species_filtered_cleaned <- wwri_iconic_species_filtered %>%
  dplyr::select(where(~ !all(is.na(.))))

# Create a lookup table for nation codes
nation_lookup <- tibble(
  nationCode = c("CA", "US"),
  nationFull = c("Canada", "United States")
)

# Map nation codes to full names
wwri_iconic_species_filtered_cleaned <- wwri_iconic_species_filtered_cleaned %>%
  left_join(nation_lookup, by = "nationCode") %>%
  mutate(nationCode = nationFull) %>%
  dplyr::select(-nationFull)  # Drop the intermediate column

# Create a lookup table for state and territory abbreviations
state_lookup <- tibble(
  state_abbreviation = c("YT", "BC", "MT", "AK", "WY", "CO", "WA", "ID", "OR", "AZ", "CA", "NM", "NV", "UT"),
  state_full = c("Yukon", "British Columbia", "Montana", "Alaska", "Wyoming", 
                 "Colorado", "Washington", "Idaho", "Oregon", "Arizona", 
                 "California", "New Mexico", "Nevada", "Utah")
)

# Map state abbreviations to full names
wwri_iconic_species_filtered_cleaned <- wwri_iconic_species_filtered_cleaned %>%
  left_join(state_lookup, by = c("subnations_subnationsubnationCode" = "state_abbreviation")) %>%
  mutate(subnations_subnationsubnationCode = state_full) %>%
  dplyr::select(-state_full)  # Drop intermediate column if unnecessary

# Separate scores for subnational and national ranks
wwri_iconic_species_filtered_cleaned_seperate_scores <- wwri_iconic_species_filtered_cleaned %>%
  separate(
    subnations_subnationroundedSRank, 
    into = c("Sscore1", "Sscore2"), 
    sep = ",", 
    fill = "right" # Fill missing values with NA
  ) %>%
  separate(
    roundedNRank, 
    into = c("Nscore1", "Nscore2", "Nscore3"), 
    sep = ",", 
    fill = "right" # Fill missing values with NA
  )

#### Create NS status WRI scoring chart ####

# Extend the NatureServe mapping table with S (subnational) and N (national) ranks and G (Global) rounded rank
conservation_rank_mapping <- c(
  # "S" ranks
  "SX" = 0, "SH" = 0,
  "S1" = 0.2, "S1B" = 0.2, "S1N" = 0.2,
  "S2" = 0.4, "S2B" = 0.4, "S2N" = 0.4,
  "S3" = 0.6, "S3B" = 0.6, "S3N" = 0.6,
  "S4" = 0.8, "S4B" = 0.8, "S4N" = 0.8,
  "S5" = 1, "S5B" = 1, "S5N" = 1,
  "SU" = NA, "SUN" = NA, "SUB" = NA,
  "SNR" = NA, "SNRN" = NA, "SNRB" = NA,
  "SNA" = NA, "SNAN" = NA, "SNAB" = NA,
  
  # "N" ranks
  "N1" = 0.2, "N2" = 0.4, "N3" = 0.6, "N4" = 0.8, "N5" = 1,
  "N1B" = 0.2, "N1N" = 0.2, "N2B" = 0.4, "N2N" = 0.4,
  "N3B" = 0.6, "N3N" = 0.6, "N4B" = 0.8, "N4N" = 0.8,
  "N5B" = 1, "N5N" = 1,
  "N5B,N5N" = 1, "N4B,N4N" = 0.8, "N3B,N2N,N3M" = 0.6, "N5B,N4N" = 1,
  "N5B,NUN" = NA, "N4B,N5N" = 1, "N5B,N5N,N5M" = 1,
  "N5B,N2N" = 1, "N3B,NUM" = 0.6, "N5B,NNRN" = NA, "NU" = NA, "NNR" = NA, "NNA" = NA,
  
  # "G" (Global) ranks
  "G1" = 0.2, "G2" = 0.4, "G3" = 0.6, "G4" = 0.8, "G5" = 1,
  "T1" = 0.2, "T2" = 0.4, "T3" = 0.6, "T4" = 0.8, "T5" = 1,
  "GNR" = NA, "TNR" = NA
)


#### Attach numerical WRI score to NS letter score ####

# Combine assignments for Sscore and Nscore and roundedGscore in a single pipeline
wwri_iconic_species_threat_scores <- wwri_iconic_species_filtered_cleaned_seperate_scores %>%
  mutate(
    # Assign numerical values for Score1 and Score2
    Score1_numeric = if_else(is.na(Sscore1), NA_real_, conservation_rank_mapping[Sscore1]),
    Score2_numeric = if_else(is.na(Sscore2), NA_real_, conservation_rank_mapping[Sscore2]),
    # Assign numerical values for Nscore1, Nscore2, and Nscore3
    Nscore1_numeric = if_else(is.na(Nscore1), NA_real_, conservation_rank_mapping[Nscore1]),
    Nscore2_numeric = if_else(is.na(Nscore2), NA_real_, conservation_rank_mapping[Nscore2]),
    Nscore3_numeric = if_else(is.na(Nscore3), NA_real_, conservation_rank_mapping[Nscore3]), 
    # Assign numerical values for roundedGRank
    roundedGRank_numeric = if_else(is.na(roundedGRank), NA_real_, conservation_rank_mapping[roundedGRank])
  )

# Calculate the average of S scores and N scores
wwri_iconic_species_threat_score_averages <- wwri_iconic_species_threat_scores %>%
  mutate(
    # Average S scores
    average_S_score = rowMeans(
      cbind(Score1_numeric, Score2_numeric), na.rm = TRUE
    ),
    # Average N scores
    average_N_score = rowMeans(
      cbind(Nscore1_numeric, Nscore2_numeric, Nscore3_numeric), na.rm = TRUE
    )
  )

# Create a new column status_score with S > N > G infilling logic
wwri_iconic_species_status_score_total_avg <- wwri_iconic_species_threat_score_averages %>%
  mutate(
    # Create status_score by prioritizing S score, then N score, then G score
    status_score = if_else(
      is.nan(average_S_score), 
      if_else(
        is.nan(average_N_score),
        roundedGRank_numeric,  # Infill with G score if S and N are missing
        average_N_score        # Infill with N score if only S is missing
      ), 
      average_S_score          # Use S score if available
    )
  )


#### duplicate rows to get US and CAN species and apply a score for United States and Canada species ####

# Create a new dataframe with Canada and US rows for each species
additional_rows <- wwri_iconic_species_status_score_total_avg %>%
  # Select relevant columns
  dplyr::select(scientificName, primaryCommonName, roundedGRank_numeric, average_N_score) %>%
  # Ensure unique scientificName and roundedGRank_numeric combinations
  distinct() %>%
  # Add rows for both nation codes
  tidyr::expand_grid(subnations_subnationsubnationCode = c("Canada", "United States")) %>% 
  # Remove duplicate scientificName entries
  distinct(scientificName, subnations_subnationsubnationCode, .keep_all = TRUE) %>%
  # Fill NA values in average_N_score with roundedGRank_numeric
  mutate(average_N_score = if_else(is.na(average_N_score), roundedGRank_numeric, average_N_score)) %>%
  # Duplicate average_N_score column as status_score
  mutate(status_score = average_N_score)


# Combine the additional country specific rows with the original data
# 1037 rows
wwri_iconic_species_status_score_final <- bind_rows(
  wwri_iconic_species_status_score_total_avg,
  additional_rows
)


#### Create df with iconic species names, state, country and NS threat scores ####
# some species naming differences exist in how NS names and states iconic scientific naming conventions need to identify them and change them and clean up scoring

# Identify unique names in ns treat score dataframe that are not in iconic_species_list
unique_to_ns <- setdiff(
  unique(wwri_iconic_species_status_score_final$scientificName),
  unique(iconic_species_list$ns_sci_name)
)

# Unique names in iconic_species_list not in ns treat score dataframe
unique_to_iconic <- setdiff(
  unique(iconic_species_list$ns_sci_name),
  unique(wwri_iconic_species_status_score_final$scientificName))

# Inspect the results
unique_to_ns
unique_to_iconic

# Rename and select relevant columns
wwri_species_status_scores_df <- wwri_iconic_species_status_score_final %>%
  rename(
    ns_sci_name = scientificName,
    state = subnations_subnationsubnationCode,
    country = nationCode, 
  ) %>%
  dplyr::select(ns_sci_name, state, country, status_score)

# Create vector of known name corrections to align naming conventions
species_name_replacements <- c(
  "Aquilegia caerulea" = "Aquilegia coerulea",
  "Myosotis alpestris" = "Myosotis alpestris ssp. asiatica",
  "Oncorhynchus henshawi henshawi" = "Oncorhynchus clarkii henshawi",
  "Oncorhynchus lewisi" = "Oncorhynchus clarkii lewisi",
  "Oncorhynchus virginalis ssp. 1" = "Oncorhynchus clarkii stomias",
  "Oncorhynchus virginalis utah" = "Oncorhynchus clarkii utah",
  "Oncorhynchus virginalis virginalis" = "Oncorhynchus clarkii virginalis"
)

# Apply name corrections
wwri_species_status_scores_df <- wwri_species_status_scores_df %>%
  mutate(ns_sci_name = recode(ns_sci_name, !!!species_name_replacements))

# if state == "Canada" or the "United States" infill those into the country column
wwri_species_status_scores_df <- wwri_species_status_scores_df %>%
  mutate(
    country = if_else(state == "Canada", "Canada", country),
    country = if_else(state == "United States", "United States", country)
  )

# Join back with Iconic Species List to add threat scores and retain relevant identifiers
iconic_species_state_scores <- iconic_species_list %>%
  dplyr::select(rgbif_mol_sci_name, ns_sci_name, state) %>%
  left_join(wwri_species_status_scores_df, by = c("ns_sci_name", "state"))

# Identify which rows in the final joined df are missing status scores
missing_status_scores <- iconic_species_state_scores %>%
  filter(is.na(status_score))

# 8 NA's
View(missing_status_scores)

# Add country information for US states
us_states <- c("Utah", "California", "Oregon", "Washington", "Idaho", "Wyoming", "New Mexico", "Montana", "Alaska", "Colorado", "Nevada", "Arizona")
can_states <- c("British Colombia", "Yukon")

missing_status_scores <- missing_status_scores %>%
  mutate(
    country = case_when(
      state %in% us_states ~ "United States",
      TRUE ~ country  # Keep existing if not matched
    )
  )

# Join to get the correct roundedGRank_numeric from wwri_iconic_species_status_score_final for the two fish because they do have a Grank score
missing_scores_filled <- missing_status_scores %>%
  left_join(
    wwri_iconic_species_status_score_final %>%
      rename(
        ns_sci_name = scientificName,
        country = nationCode
      ) %>%
      dplyr::select(ns_sci_name, country, roundedGRank_numeric) %>%
      distinct(),
    by = c("ns_sci_name", "country")
  ) %>%
  mutate(
    # Fill in status_score with roundedGRank_numeric if available
    status_score = coalesce(status_score, roundedGRank_numeric)
  ) %>%
  dplyr::select(rgbif_mol_sci_name, ns_sci_name, state, country, status_score)


# Now merge these filled scores back into the final dataframe
final_wwri_iconic_species_scores <- iconic_species_state_scores %>%
  rows_update(missing_scores_filled, by = c("rgbif_mol_sci_name", "ns_sci_name", "state"))

# 5 NA score species left (3 mushrooms, 2 insects)
View(final_wwri_iconic_species_scores %>% filter(is.na(status_score)))

#### write out final iconic species status score with state and country csv ####
write_csv(final_wwri_iconic_species_scores, file.path(intermediate_data_file_path, "2024/status/iconic_species_status_scores.csv"))
