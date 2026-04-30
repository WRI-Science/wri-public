wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

#### Goal #### 
# The goal of this script is to generate a merged dataframe from the csvs made in step 2.
# This was crashing python and was possible using data.table syntax in R.
# The output from this script will be used in the traits, density, and diversity indicators.
# Presently this takes about 45 minutes to run.

#### Packages ####
library(tidyverse)
library(data.table)

#### File paths and Setup ####
scanfi_dfs_path <- file.path(wri_project_root, "data", "natural_habitats", "int", "scanfi", "individual_csvs_to_join")
closure_df_path <- paste0(scanfi_dfs_path, "closure.csv")
save_path <- file.path(wri_project_root, "data", "natural_habitats", "int", "scanfi", "scanfi_merged_closure_all_species.csv")

# list of csvs to join
csvs <- list(
  "balsam_fir" = "balsam_fir.csv",
  "black_spruce" = "black_spruce.csv",
  "douglas_fir" = "douglas_fir.csv",
  "jack_pine" = "jack_pine.csv",
  "lodge_pole" = "lodge_pole.csv",
  "ponderosa_pine" = "ponderosa_pine.csv",
  "tamarack" = "tamarack.csv",
  "white_red_pine" = "white_red_pine.csv",
  "broadleaf_tree_prcB" = "broadleaf_tree_prcB.csv",
  "other_coniferous_prcC" = "other_coniferous_prcC.csv"
)

#### Main Processing ####
## Read in the closure csv that will serve as the base
closure_dt <- fread(closure_df_path)

# filter for closure values greater than 0 using dt syntax
closure_dt <- closure_dt[closure > 0, ]

# loop through the csvs and join them to the closure df
# close each species data frame once joined
for (csv in csvs) {
  # print the current csv being processed
  print(paste("Processing:", csv))
  # read in the csv
  df <- fread(paste0(scanfi_dfs_path, csv))
  
  # join the df to the closure df using dt syntax
  closure_dt <- merge(closure_dt, df, by = c("x", "y"), all.x = TRUE)
  
  # close the df
  rm(df)
  
  # save the csv after each loop to not lose
  # the progress in case of an error
  fwrite(closure_dt, save_path)
  print(paste("Saved intermediate closure_dt to:", save_path))
}

# Final print message of the merged dataframe
print("Merged full dataframe created successfully.")