# Code for filling in plant traits 
# Trees and Iconic Species
# trait summary 

# Set Up -----------------------------------------------------------------------

#load packages
library(tidyverse)
library(BIEN)
library(rtry)
library(rgbif)

# set working directory 

setwd("/home/shares/wwri-wildfire/data/multi-domain-data/traits")

## load data --------------------------------------------------------------

# tree list from tree map
tree_spp <- read_csv("data/spp_lists/Contigous US Species - unique_species_contiguous.csv")
icon_spp <- read_csv("data/spp_lists/iconic_species_list.csv")

# databases
try_spp    <- rtry_import(input = "data/spp_lists/TryAccSpecies.txt")
serot_spp  <- readxl::read_xlsx("data/external_data/serotinous_spp_genera_lamont_etal_2020.xlsx", col_names = "genera", sheet = 1)
bbb_db     <- read_csv("data/external_data/BBBdb_2017.11.csv")
try_data   <- rtry_import(input = "data/external_data/39205_11022025075902/39205.txt")

# old try data for iucn plants
# try_spp_id <- read_csv("data/spp_lists/study_area_spp_in_try.csv")

## Data Processing -------------------------------------------------------------

# getting species ID to use for requesting TRY data
tree_spp %>%
  left_join(try_spp, by = c("SCIENTIFIC_NAME" = "AccSpeciesName")) %>%
  filter(!is.na(AccSpeciesID)) %>% 
  summarize(AccSpeciesID = paste0(AccSpeciesID, collapse = ", ")) %>% 
  pull(AccSpeciesID)
  

# Plant Traits -------------------------------------------------------------------

# combine iconic spp and trees 

icon_spp <- icon_spp %>% 
  select(type, rgbif_mol_sci_name) %>%
  mutate(kingdom = case_when(
    type %in% c("grass", "flower", "cactus", "tree") ~ "Plantae",
    type %in% c("insect", "slug", "fish", "reptile", "bird", "mammal", "amphibian") ~ "Animalia",
    type == "mushroom" ~ "Fungi"
  ))

plant_icon_spp <- icon_spp %>%
  filter(kingdom == "Plantae" | kingdom == "Fungi") %>%
  distinct()

# create joint dataset for iconic plants and trees
wwri_plant <- c(tree_spp$SCIENTIFIC_NAME, plant_icon_spp$rgbif_mol_sci_name) 
wwri_plant <- wwri_plant %>% unique()
wwri_plant_df <- data.frame(sci_name = wwri_plant)

# join in TRY trait data
wwri_plant_df <- wwri_plant_df %>% 
  left_join(try_spp, by = c("sci_name" = "AccSpeciesName"))

# this might duplicate the try data spp filtering above
wwri_plant_df %>% 
  select(sci_name, AccSpeciesID) %>% 
  filter(!is.na(AccSpeciesID)) %>% 
  summarize(species_id = paste0(AccSpeciesID, collapse = ", "))

head(try_spp)

# not sure why but many trait names are blank - filter out for now
try_data <- try_data %>% 
  filter(!is.na(TraitID))

## harmonize taxonomy ----------------------------------------------------

bbb_list <- bbb_db %>% pull(Taxa)
bbb_gbif <- name_backbone_checklist(bbb_list)

# see summary of species taxonomy matches
bbb_gbif %>% 
  group_by(matchType, status) %>% 
  count()

# examine fuzzy or higher rank matches
bbb_gbif %>%
  filter(matchType != "EXACT") %>%
  select(verbatim_name, species, canonicalName, status, matchType) %>%
  filter(is.na(species))

bbb_spp_toupdate <- bbb_gbif %>%
  filter(matchType != "EXACT") %>%
  select(verbatim_name, species, canonicalName, status, matchType) %>%
  filter(is.na(species)) %>% 
  pull(verbatim_name)

bbb_name_updates <- data.frame(
  bbb_name = bbb_spp_toupdate,
  gbif_name = c("Hyacinthus muscari", "Terminalia elliptica", "Erica discolor", 
                "Vaccinium pallidum", ),
  acc_name = 
)

wwri_gbif <- name_backbone_checklist(wwri_plant)

wwri_gbif %>% 
  group_by(matchType, status) %>% 
  count()

# look for mismatches
wwri_gbif %>%
  filter(matchType == "NONE") %>%
  select(verbatim_name)

# i don't think there are actually any problems with these names, but weird things
# in gbif lookup (e.g. similar genera from different kingdoms)

wwri_gbif %>%
  filter(matchType == "HIGHERRANK") %>%
  select(verbatim_name)

# these two species had name changes 

wwri_gbif <- wwri_gbif %>%
  select("orig_name" = verbatim_name, "gbif_name" = canonicalName, species)
  
wwri_gbif$gbif_name[wwri_gbif$orig_name == "Quercus prinus"] <- "Quercus montana"
wwri_gbif$gbif_name[wwri_gbif$orig_name == "Epilobium angustifolium"] <- "Chamaenerion angustifolium"

write_csv(wwri_gbif, "data/spp_lists/plant_spp_comb.csv")

## serotiny --------------------------------------------------------------------

# there is a better dataset for this now - see he et al 2012
wwri_plant_genera <- str_split_i(wwri_plant, pattern = " ", i = 1) %>% 
  unique() 

wwri_plant_genera[wwri_plant_genera %in% serot_spp$genera]

## Try Data --------------------------------------------------------------------

try_data %>% 
  select(TraitName, TraitID) %>% 
  distinct() %>%
  filter(str_detect(TraitName, "Bark"))

try_data %>% 
  select(SpeciesName, TraitName, StdValue, OrigValueStr, UnitName, OrigUnitStr) 

### bark data -----------

try_data %>% 
  select(AccSpeciesName, TraitName, StdValue, UnitName) %>%
  filter(str_detect(TraitName, "Bark")) %>%
  group_by(AccSpeciesName, TraitName, UnitName) %>%
  summarize(mean_bark_thick = mean(StdValue, na.rm = TRUE),
            num_reps = n()) %>%
  print(n=149)

try_data %>%
  filter(TraitID == 3355 | TraitID == 3356 | TraitID == 24) %>% 
  select(AccSpeciesID, AccSpeciesName, OrigValueStr, OrigUnitStr, TraitName) %>%
  arrange(AccSpeciesName)


### longevity --------------------------

plant_lifespan <- try_data %>%
  select(AccSpeciesName, TraitName, StdValue, UnitName) %>%
  filter(str_detect(TraitName, "lifespan")) %>%
  group_by(AccSpeciesName, TraitName, UnitName) %>%
  summarize(mean_long = mean(StdValue, na.rm = TRUE),
            num_reps = n()) 

wwri_plant_df %>%
  left_join(plant_lifespan, by = c("sci_name" = "AccSpeciesName")) %>%
  write.table()

### Resin ------------------------

plant_resin <- try_data %>%
  select(AccSpeciesName, TraitName, OrigValueStr) %>%
  filter(str_detect(TraitName, "resin")) %>%
  group_by(AccSpeciesName, TraitName) %>%
  summarize(resin = paste0(OrigValueStr, collapse = ", "),
            num_reps = n()) 

wwri_plant_df %>%
  left_join(plant_resin, by = c("sci_name" = "AccSpeciesName")) %>%
  write.table()


### Budbank ------------------

wwri_plant_df <- wwri_plant_df %>% 
  left_join(bbb_db, by = c("sci_name" = "Taxa"))

bud_data <- try_data %>% 
  filter(TraitID == 4080)

bud_data %>% 
  select(AccSpeciesID, AccSpeciesName, OrigValueStr, OrigUnitStr) %>%
  arrange(AccSpeciesName)


## BIEN Traits -----------------------------------------------------------------

trait_list <- c(
  "longest whole plant longevity",
  "maximum whole plant height",
  "maximum whole plant longevity",
  "minimum whole plant height",
  "seed mass",
  "whole plant height"
)

plant_df <- data.frame(
  sci_name = rep(wwri_plant, each = length(trait_list)),
  trait    = rep(trait_list, length(wwri_plant))
)

# this takes a LONG time (3ish hours)
# pulls in each trait for each species 
# can now instead load the datafile 
bien_plant_traits <- map2(
  .x = plant_df$sci_name, 
  .y = plant_df$trait,
  .f = BIEN_trait_traitbyspecies
  )

bien_trait_df <- bien_plant_traits %>% 
  bind_rows()

# select most relevant columns
bien_trait_df <- bien_trait_df %>%
  select("sci_name" = scrubbed_species_binomial, trait_name, trait_value, unit) %>%
  arrange(sci_name)

# write_csv(bien_trait_df, file = "data/traits/bien_trait_data.csv")

bien_trait_df <- read_csv("data/traits/bien_trait_data.csv")

### Plant Height ----------------------------------------------------------------
# select height variables
bien_plant_height <- bien_trait_df %>% 
  filter(trait_name %in% c("whole plant height", "maximum whole plant height", "minimum whole plant height"))

# summarize by species 
bien_height_sum <- bien_plant_height %>% 
  group_by(sci_name, trait_name, unit) %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  summarize(mean_height = mean(trait_value, na.rm = TRUE),
            num_reps = n(),
            sd = sd(trait_value, na.rm = TRUE))

# put in wide format so each spp has one row
bien_height_wide <- bien_height_sum %>% 
  ungroup() %>%
  mutate(trait_name = case_when(
    trait_name == "whole plant height" ~ "plant",
    trait_name == "maximum whole plant height" ~ "max_plant",
    trait_name == "minimum whole plant height" ~ "min_plant"
  )) %>%
  pivot_longer(cols = c("mean_height", "num_reps", "sd")) %>%
  mutate(name = paste0(trait_name, "_", name)) %>%
  select(-trait_name) %>%
  pivot_wider(names_from = name, values_from = value)

### Seed Mass -------------------------------------------------------------------
bien_seed_mass <- bien_trait_df %>% 
  filter(trait_name %in% c("seed mass"))

bien_seed_mass_sum <- bien_seed_mass %>% 
  group_by(sci_name, trait_name, unit) %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  summarize(mean_mass = mean(trait_value, na.rm = TRUE),
            num_reps = n(),
            sd = sd(trait_value, na.rm = TRUE))

# find species not in BIEN 
seed_mass_genus <- wwri_plant[!wwri_plant %in% bien_seed_mass_sum$sci_name]

# get means at genus or fam level
bien_seed_genus <- BIEN_trait_mean(species = seed_mass_genus, trait = "seed mass")

bien_seed_genus <- bien_seed_genus %>% 
  select(species, "seed_mass" = mean_value, unit, level_used)

### Longevity -------------------------------------------------------------------
bien_longev <- bien_trait_df %>% 
  filter(trait_name %in% c("longest whole plant longevity", "maximum whole plant longevity")) 

bien_longev_sum <- bien_longev %>%
  group_by(sci_name, trait_name, unit) %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  summarize(mean_long = mean(trait_value, na.rm = TRUE),
            num_reps = n(),
            sd = sd(trait_value, na.rm = TRUE))

### Growth Form -----------------------------------------------------------------
bien_growth_form <- BIEN_trait_traitbyspecies(species = plant_df$sci_name, trait = "whole plant growth form")

bien_growth_form %>% 
  select("sci_name" = scrubbed_species_binomial, trait_name, trait_value, unit) %>%
  mutate(trait_value = tolower(trait_value)) %>%
  distinct() %>%
  group_by(sci_name) %>%
  summarize(growth_form = paste0(trait_value, collapse = ", "))

