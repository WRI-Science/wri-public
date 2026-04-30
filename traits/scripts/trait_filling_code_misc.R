# Code to fill in species traits for WWRI 

# Set Up -----------------------------------------------------------------------

#load packages
library(tidyverse)
library(rfishbase)
library(janitor)

# set working directory 

setwd("/home/shares/wwri-wildfire/data/multi-domain-data/traits")

## load data --------------------------------------------------------------

# wwri species lists
spp_list  <- read_csv("data/spp_lists/total_spp_for_traits_no_subpops_subspecies_for_rachel.csv")

# databases
amphibio  <- read_csv("data/external_data/AmphiBIO_v1/AmphiBIO_v1.csv")
amn_db    <- read_csv("data/external_data/ECOL_96_269/Data_Files/Amniote_Database_Aug_2015.csv")
fish_df   <- readxl::read_xls("data/external_data/FishTraits_14.3.xls")
rep_trait <- readxl::read_xlsx("data/external_data/ReptTraits dataset v1-2.xlsx", sheet = 2)

rep_trait <- clean_names(rep_trait)

# split by groups for trait matching
wwri_fish <- spp_list %>% 
  filter(class == "Actinopterygii")

rep_spp <- spp_list %>% 
  filter(class == "Reptilia")

bird_spp <- spp_list %>%
  filter(class == "Aves")

# Animal Traits -------------------------------------------------------------------


# relevant columns for the amniote database
amn_db_cols <- c(
  "class",
  "order",
  "family",
  "genus",
  "species",
  "subspecies",
  "female_maturity_d",
  "male_maturity_d", 
  "litter_or_clutch_size_n",
  "litters_or_clutches_per_y",
  "adult_body_mass_g",
  "maximum_longevity_y",
  "longevity_y",
  "female_body_mass_g",
  "male_body_mass_g",
  "no_sex_body_mass_g",
  "adult_svl_cm",
  "male_svl_cm",
  "female_svl_cm",
  "no_sex_svl_cm",
  "no_sex_maturity_d"
)

animal_spp <- spp_list %>%
  filter(kingdom == "Animalia")

spp_list_check <- name_backbone_checklist(animal_spp$sci_name)

spp_list_sub <- spp_list_check %>%
  arrange(class, verbatim_name) %>%
  select("iucn_name" = verbatim_name, "acc_name" = species, "full_name" = scientificName, class, order)

write_csv(spp_list_sub, "data/spp_lists/updated_animal_taxonomy_list.csv")

## Mammals --------------------------------------------------------------------

mam_spp <- spp_list %>%
  filter(class == "Mammalia")

mam_spp_check <- name_backbone_checklist(mam_spp$sci_name)
mam_spp_check <- mam_spp_check %>%
  select("iucn_name" = verbatim_name, "acc_name" = species) %>%
  right_join(mam_spp, by = c("iucn_name" = "sci_name"))

mam_db <- amn_db %>%
  select(all_of(amn_db_cols)) %>%
  filter(class == "Mammalia")

mam_db_check <- name_backbone_checklist(paste0(mam_db$genus, " ", mam_db$species))
mam_db_check <- mam_db_check %>%
  select("iucn_name" = verbatim_name, "acc_name" = species)

mam_db <- mam_db %>%
  mutate(sci_name = paste0(genus, " ", species)) %>%
  left_join(mam_db_check, by = c("sci_name" = "iucn_name"))

mam_db <- mam_db %>%
  select(-c(class, order, family, genus, species))

# genus neotamias may not be accepted ?? 

mam_spp_check <- mam_spp_check %>%
  mutate(acc_name = ifelse(
    genus == "Neotamias", 
    paste0("Tamias", " ", species), 
    acc_name)
    )

mam_spp_db <- mam_spp_check %>%
  left_join(mam_db, by = c("acc_name"))

mam_spp_db <- mam_spp_db %>%
  select(-sci_name) %>% 
  distinct()

# maturity
# summarize maturity by family and genus to fill in missing data
maturity_sum <- mam_spp_db %>%
  filter(female_maturity_d > 0) %>%
  group_by(family, genus) %>%
  mutate(mean_maturity_gen = mean(female_maturity_d, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(family) %>%
  mutate(mean_maturity_fam = mean(female_maturity_d, na.rm =  TRUE)) %>%
  select(family, genus, mean_maturity_fam, mean_maturity_gen) %>%
  distinct()

mam_spp_db <- mam_spp_db %>%
  select(-mean_maturity_fam, -mean_maturity_gen) %>%
  left_join(maturity_sum) 

# find family level means for annual repro output
mam_spp_db %>%
  mutate(annual_repro = litter_or_clutch_size_n * litters_or_clutches_per_y) %>%
  filter(litter_or_clutch_size_n > 0 & litters_or_clutches_per_y > 0) %>%
  group_by(family) %>%
  summarize(mean_repro = mean(annual_repro, na.rm = TRUE)) %>%
  print(n=28)

## Iconic Spp ------------------------------------------------------------------
head(icon_spp)

icon_spp_check <- name_backbone_checklist(icon_spp$rgbif_mol_sci_name)

icon_spp_check$species[icon_spp_check$species %in% mam_spp_check$acc_name]


## Birds ------------------------------------------------------------------------

bird_spp_db <- amn_db %>% 
  filter(class == "Aves") %>%
  select(all_of(amn_db_cols)) %>%
  mutate(sci_name = paste0(genus, " ", species))

# check species names 
bird_spp_check <- name_backbone_checklist(bird_spp$sci_name)
bird_spp_db_check <- name_backbone_checklist(bird_spp_db$sci_name)

# examine species without matches or doubtful matches
bird_spp_db_check %>%
  filter(is.na(status)) %>%
  select(verbatim_name)

bird_spp_db_check %>%
  filter(status == "DOUBTFUL") %>%
  select(verbatim_name)

# join in checked names to bird database
bird_spp_db <- bird_spp_db_check %>%
  select(status, "acc_name" = species, verbatim_name) %>%
  right_join(bird_spp_db, by = c("verbatim_name" = "sci_name"))

# only one bird species not found 
bird_spp_check %>%
  group_by(status) %>%
  count()

# a few duplicates now because of taxonomy changes, summarize to remove dups
bird_spp_db_sum <- bird_spp_db %>% 
  group_by(genus, family, acc_name) %>%
  summarize(
    across(where(is.numeric), 
           .fns = ~mean(.x, na.rm = TRUE), 
           .names = "{.col}")
    )

# join in with birds for study area
wwri_bird_spp_db <- bird_spp_check %>%
  select(status, family, genus, species, "iucn_name" = verbatim_name) %>%
  left_join(bird_spp_db_sum, by = c("species" = "acc_name"))

wwri_bird_spp_db %>%
  arrange(iucn_name) %>%
  write.table()

# summarize by family 
bird_spp_db_sum %>% 
  group_by(family) %>% 
  filter(female_maturity_d > 0) %>%
  summarize(mean_mat = mean(female_maturity_d, na.rm = TRUE)) %>%
  mutate(mean_mat = mean_mat/365) %>%
  print(n = 116)

wwri_bird_spp_db %>%
  group_by(family) %>% 
  filter(female_maturity_d > 0) %>%
  summarize(mean_mat = mean(female_maturity_d, na.rm = TRUE)/365) %>%
  print(n = 56)

wwri_bird_spp_db %>%
  filter(litter_or_clutch_size_n > 0 & litters_or_clutches_per_y > 0) %>%
  mutate(annual_repro = litter_or_clutch_size_n * litters_or_clutches_per_y) %>%
  summarize(med_repro_output = median(annual_repro, na.rm = TRUE))
  group_by(family) %>%
  summarize(mean_repro_output = mean(annual_repro, na.rm = TRUE)) %>%
  print(n = 58)

wwri_bird_spp_db %>%
  group_by(family) %>%
  summarize(mean_body_mass = mean(adult_body_mass_g, na.rm = TRUE),
            med_body_mass = median(adult_body_mass_g, na.rm = TRUE)) %>%
  print(n = 63)

wwri_bird_spp_db %>%
  filter(maximum_longevity_y > 0) %>%
  group_by(family) %>%
  summarize(mean_long = mean(maximum_longevity_y, na.rm = TRUE),
            med_long = median(maximum_longevity_y, na.rm = TRUE)) %>%
  print(n = 63)

## Reptiles --------------------------------------------------------------------

# relevant columns from ReptTrait dataset
rep_colnames <- c(
  "species",
  "family",
  "genus",
  "order",
  "maximum_longevity_years",
  "maximum_body_mass_g",
  "maximum_total_length_tl_mm", 
  "maximum_length_svl_mm_straight_carapace_length_for_turtles_scl_mm",
  "mean_number_of_offspring_per_litter_or_number_of_eggs_per_clutch",
  "number_of_litters_or_clutches_produced_per_year"
  )

rep_trait_sub <- rep_trait %>%
  select(all_of(rep_colnames))

# make shorter column names 
colnames(rep_trait_sub) <- c(
  "species",
  "family",
  "genus",
  "order",
  "max_long_yrs",
  "max_body_mass_g",
  "max_tot_length_mm",
  "max_svl_length_mm",
  "mean_off_per_litter",
  "num_litt_yr"
)

# calculate means by genus and family
rep_trait_sub <- rep_trait_sub %>%
  mutate(across(
    .cols = c(
      "max_long_yrs",
      "max_body_mass_g",
      "max_tot_length_mm",
      "max_svl_length_mm",
      "mean_off_per_litter",
      "num_litt_yr"
    ),
    .fns = as.numeric
  )) %>%
  group_by(genus) %>%
  mutate(across(
    .cols = c(
      "max_long_yrs",
      "max_body_mass_g",
      "max_tot_length_mm",
      "max_svl_length_mm",
      "mean_off_per_litter",
      "num_litt_yr"
    ),
    .fns = ~mean(.x, na.rm = TRUE),
    .names = "{col}_gen_mean"
  )) %>%
  ungroup() %>%
  group_by(family) %>%
  mutate(across(
    .cols = c(
      "max_long_yrs",
      "max_body_mass_g",
      "max_tot_length_mm",
      "max_svl_length_mm",
      "mean_off_per_litter",
      "num_litt_yr"
    ),
    .fns = ~mean(.x, na.rm = TRUE),
    .names = "{col}_fam_mean")
    )

# need to check taxonomy ..
rep_spp_check <- name_backbone_checklist(rep_spp$sci_name)
rep_name_check <- name_backbone_checklist(rep_trait_sub$species)

rep_name_sub <- rep_name_check %>% 
  select(matchType, status, "acc_name" = species, "orig_name" = verbatim_name)

# rep_name_sub <- rep_name_sub %>%
#   mutate(sci_name = ifelse(matchType %in% c("EXACT", "FUZZY"), canonicalName, verbatim_name)) %>%
#   mutate(sci_name = ifelse(status == "SYNONYM", species, sci_name)) %>%
#   select(-species)

rep_trait_sub <- rep_trait_sub %>%
  left_join(rep_name_sub, by = c("species" = "orig_name")) %>%
  select(-matchType, -status) %>%
  distinct()

# some duplicates based on subspeices? 

rep_trait_sum <- rep_trait_sub %>%
  group_by(acc_name, family, genus) %>%
  summarize(across(.cols = where(is.numeric), .fns = ~mean(.x, na.rm=TRUE), .names = "{.col}"))

rep_spp_check <- rep_spp_check %>% 
  select(matchType, status, "acc_name" = species, "iucn_name" = verbatim_name)

rep_spp_check %>% 
  group_by(acc_name) %>%
  count() %>%
  filter(n > 1)

wwri_rep_traits <- rep_spp_check %>%
  select(acc_name, iucn_name) %>% 
  distinct() %>%
  left_join(rep_trait_sum) 

# manually checked and the few sepcies with NA values were either
# a weird typo that the gbif function didn't catch, hybrids, or weren't
# in the ReptTrait database (or maybe those species were combined - rena dissecta)

# read in new spp list to match
rep_spp_new <- read_csv("data/spp_lists/reptile_spp_updated.csv")

rep_trait_wwri <- rep_spp_new %>%
  left_join(rep_trait_sub, by = c("sci_name" = "species"))

rep_trait_wwri %>% 
  write.table()

### maturity ----------------------------------------------------------------------

amn_db_sub <- amn_db %>%
  select(all_of(amn_db_cols))

# filter to reptiles
rep_db_sub <- amn_db_sub %>% 
  filter(class == "Reptilia")

# create sci name for joining dataframes
rep_db_sub <- rep_db_sub %>%
  mutate(acc_name = paste0(genus, " ", species))

# join data frames
rep_amn_db <- rep_spp_check %>%
  left_join(rep_db_sub, by = c("acc_name")) %>%
  select(-subspecies)

# calculate means by genus and family for species without data
maturity_fam <- rep_db_sub %>%
  filter(female_maturity_d > 0) %>%
  group_by(family) %>%
  summarize(mean_matu_y = mean(female_maturity_d, na.rm = TRUE)/365,
            min_matu_y = min(female_maturity_d, na.rm = TRUE)/365,
            max_matu_y = max(female_maturity_d, na.rm = TRUE)/365)

maturity_genus <- rep_db_sub %>%
  filter(female_maturity_d > 0) %>%
  group_by(genus) %>%
  summarize(mean_matu_y_gen = mean(female_maturity_d, na.rm = TRUE)/365,
            min_matu_y_gen = min(female_maturity_d, na.rm = TRUE)/365,
            max_matu_y_gen = max(female_maturity_d, na.rm = TRUE)/365)

rep_amn_db %>%
  left_join(maturity_fam, by = "family") %>%
  left_join(maturity_genus, by = "genus") %>%
  write.table()

## Amphibians ------------------------------------------------------------------

str(amphibio)
head(spp_list)
colnames(amphibio)

# select relevant column names
amphi_cols <- c(
  "Order",
  "Family",
  "Species",
  "Body_mass_g",
  "Age_at_maturity_min_y",
  "Age_at_maturity_max_y",
  "Longevity_max_y",
  "Reproductive_output_y",
  "Dir",
  "Lar",
  "Viv",
  "Fos",
  "Arb",
  "Ter",
  "Aqu",
  "Litter_size_min_n",
  "Litter_size_max_n",
  "Body_size_mm"
)

amphibio_sub <- amphibio %>% 
  select(all_of(amphi_cols))

amph_spp <- spp_list %>%
  filter(class == "Amphibia")

amph_spp <- amph_spp %>% 
  left_join(amphibio_sub, by = c("sci_name" = "Species"))

amph_spp <- amph_spp %>% 
  mutate(mean_litter_size = (Litter_size_min_n + Litter_size_max_n)/2) %>%
  group_by(genus) %>%
  mutate(
    repro_out_gen = ifelse(is.na(Reproductive_output_y), mean(Reproductive_output_y, na.rm = TRUE), Reproductive_output_y),
    body_mass_gen = ifelse(is.na(Body_mass_g), mean(Body_mass_g, na.rm = TRUE), Body_mass_g),
    longevity_gen = ifelse(is.na(Longevity_max_y), mean(Longevity_max_y, na.rm = TRUE), Longevity_max_y),
    litter_size_gen = ifelse(is.na(mean_litter_size), mean(mean_litter_size, na.rm = TRUE), mean_litter_size)
    ) %>%
  group_by(family) %>%
  mutate(
    repro_out_fam   = ifelse(is.na(repro_out_gen), mean(Reproductive_output_y, na.rm = TRUE), repro_out_gen),
    longevity_fam   = ifelse(is.na(longevity_gen), mean(Longevity_max_y, na.rm = TRUE), longevity_gen),
    body_mass_fam   = ifelse(is.na(body_mass_gen), mean(Body_mass_g, na.rm = TRUE), body_mass_gen),
    litter_size_fam = ifelse(is.na(litter_size_gen), mean(mean_litter_size, na.rm = TRUE), litter_size_gen)
  )  %>%
  group_by(family) %>%
  mutate(
    repro_out_fam2 = ifelse(is.na(Reproductive_output_y), mean(Reproductive_output_y, na.rm = TRUE), repro_out_gen),
    longevity_fam2 = ifelse(is.na(Longevity_max_y), mean(Longevity_max_y, na.rm = TRUE), longevity_gen),
    body_mass_fam2 = ifelse(is.na(Body_mass_g), mean(Body_mass_g, na.rm = TRUE), body_mass_gen)
  ) %>%
  ungroup() %>%
  mutate(annual_repro = repro_out_fam * litter_size_fam)

amph_spp %>% 
  arrange(sci_name) %>%
  write_csv("data/traits/amphibian_spp_traits.csv")

## Fish Traits ------------------------------------------------------------------

fish_colnames <- c("FID", "GENUS", "SPECIES", "MAXTL", "MATUAGE", "LONGEVITY", "FECUNDITY")

fish_sub_df <- fish_df %>%
  select(all_of(fish_colnames))

colnames(fish_sub_df) <- tolower(colnames(fish_sub_df))

### maturity -----------------------------------------------------------------------

fish_sub_df %>% 
  filter(matuage > 0) %>%
  group_by(genus) %>%
  mutate(mean_matu = mean(matuage, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(wwri_fish) %>%
  mutate(matuage = ifelse(is.na(matuage) | matuage < 0, mean_matu, matuage)) %>%
  write.table()

### length to mass -------------------------------------------------------------------

str(fish_df)

fish_iucn_df <- fish_sub_df %>%
  inner_join(spp_list, by = c("genus", "species"))

# helper functions

# Function to extract length-weight coefficients from FishBase
get_lw_coefficients <- function(species_list) {
  # Retrieve length-weight data for all species in the list
  lw_data <- length_weight(species_list)
  
  # Select relevant columns
  lw_data <- lw_data[, c("Species", "a", "b", "Type")]
  
  return(lw_data)
}

# function to calculate weight from length-weight coefs 
fish_l2m <- function(a, b, l) {
  weight <- a * (l ^ b)
  return(weight)
}

# get coefficients
lw_results <- get_lw_coefficients(fish_iucn_df$sci_name)

lw_sum <- lw_results %>%
  group_by(Species, Type) %>%
  summarize(a = mean(a, na.rm = TRUE),
            b = mean(b, na.rm = TRUE))

fish_iucn_df <- fish_iucn_df %>%
  left_join(lw_sum, by = c("sci_name" = "Species"))

fish_iucn_df <- fish_iucn_df %>%
  mutate(weight = fish_l2m(a, b, maxtl)/1000)  %>% # weight in kg
  group_by(sci_name, genus, species, maxtl, longevity, fecundity, order, family) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) 

# export dataframew with fish traits
spp_list %>% 
  filter(class == "Actinopterygii") %>%
  select(sci_name) %>%
  left_join(fish_iucn_df, by = c("sci_name")) %>%
  write_csv("data/traits/fish_traits.csv")

fish_spp <- spp_list %>% 
  filter(class == "Actinopterygii") %>%
  left_join(lw_sum, by = c("sci_name" = "Species")) %>%
  arrange(sci_name)

# filling in fish length, initially similar to some above code
# but then do more for family level length 2 mass
fish_length <- fish_spp %>%
  left_join(fish_sub_df, by = c("genus", "species"))

fish_length %>%
  mutate(weight = fish_l2m(a, b, maxtl)/1000) %>%
  select(-id_no, -phylum, -kingdom, -genus, -species) %>%
  group_by(sci_name, maxtl, longevity, fecundity, family) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) 

fam_coef <- fish_length %>%
  group_by(family) %>% 
  summarize(mean_a = mean(a, na.rm = TRUE),
            mean_b = mean(b, na.rm = TRUE))

fish_length %>%
  left_join(fam_coef) %>%
  mutate(fam_weight = fish_l2m(mean_a, mean_b, maxtl)) %>%
  group_by(sci_name,maxtl, longevity, fecundity, family) %>%
  summarize(mean_fam_weight = mean(fam_weight, na.rm = TRUE)/1000) 


# function used to calculate mass for species with a length but no 
# family level equation
l2m <- function(l){
  mass_log <- 3.1*log10(l)-5.01
  mass <- 10^mass_log
  return(mass)
}


