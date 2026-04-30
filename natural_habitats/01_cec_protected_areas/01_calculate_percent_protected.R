#### Load Packages ####
library(tidyverse)
library(sf)
library(purrr)

#### Setup and File Paths ####
natural_habitats_base_path <- "/home/shares/wwri-wildfire/data/natural_habitats/"
multi_domain_data_base_path <- "/home/shares/wwri-wildfire/data/multi_domain_data/"

intersecting_ecoregions_path <- paste0(
  multi_domain_data_base_path,
  "int/boundary_layers/epa_ecoregions_north_america_level_iii/",
  "intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp"
)

protected_areas_path <- paste0(
  natural_habitats_base_path,
  "raw/cec_protected_areas/protectedareas_2025_geodatabase/",
  "NorthAmerica_Protected_areas_2025/NorthAmerica_Protected_areas_2025.gdb"
)

protected_area_layer_name <- "CEC_NA_2025_terrestrial_IUCN_categories"

moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

# Save Path
out_csv <- paste0(natural_habitats_base_path, "int/cec_protected_areas/ecoregion_protection_summary.csv")

#### 1) Convert FileGDB layer to GeoPackage with pure MULTIPOLYGON WKB ####
tmp_gpkg <- tempfile(fileext = ".gpkg")
ogr_args <- c(
  "-f", "GPKG",
  tmp_gpkg,
  protected_areas_path,
  protected_area_layer_name,
  "-nln", "pa_polygons",
  "-nlt", "MULTIPOLYGON"
)
system2("ogr2ogr", ogr_args, stdout = TRUE, stderr = TRUE)

#### 2) Read cleaned protected areas, ensure validity and reproject ####
protected_areas <- st_read(tmp_gpkg, layer = "pa_polygons", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(moll_crs)

#### 3) Read and prepare ecoregions ####
ecoregions <- st_read(intersecting_ecoregions_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_transform(moll_crs)

eco_union <- ecoregions %>%
  group_by(NA_L3CODE, NA_L3NAME, NA_L3KEY) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(ecoregion_area_m2 = as.numeric(st_area(geometry)))

#### 4) Filter protected areas to bbox of ecoregions ####
# this is to test the process with one ecoregion
one_eco <- eco_union %>% filter(NA_L3CODE == "10.1.1")
pa_subset <- st_filter(protected_areas, one_eco, .predicate = st_intersects)
message("PA rows intersecting 10.1.1: ", nrow(pa_subset))

eco_bbox <- st_union(eco_union) %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(moll_crs)
protected_areas <- st_filter(protected_areas, eco_bbox, .predicate = st_intersects)

#### 5) Function to compute protected area stats per ecoregion ####
compute_for_one <- function(eco_row) {
  eco_code <- eco_row$NA_L3CODE
  
  # 1) Filter PAs that intersect this ecoregion
  pa_subset <- st_filter(protected_areas, eco_row, .predicate = st_intersects)
  message("Eco ", eco_code, ": PA features after filter = ", nrow(pa_subset))
  if (nrow(pa_subset) == 0) return(NULL)
  
  # 2) Fix any invalid geometries
  if (any(!st_is_valid(eco_row)))   eco_row   <- st_make_valid(eco_row)   %>% st_buffer(0)
  if (any(!st_is_valid(pa_subset))) pa_subset <- st_make_valid(pa_subset) %>% st_buffer(0)
  
  # 3) Intersection (crop to ecoregion)
  pa_clip <- tryCatch(
    suppressWarnings(st_intersection(pa_subset, eco_row)),
    error = function(e) return(NULL)
  )
  if (is.null(pa_clip) || nrow(pa_clip) == 0) return(NULL)
  message("  ✔ Intersection succeeded for ", eco_code, " — ", nrow(pa_clip), " features")
  
  # 4) Expose geometry as list-column
  pa_clip <- pa_clip %>% mutate(.geom = sf::st_geometry(.))
  
  # 5) Dissolve per category
  dissolved <- pa_clip %>%
    group_by(IUCN_CAT) %>%
    summarise(.groups = "drop", geom_cat = sf::st_union(.geom)) %>%
    sf::st_as_sf(sf_column_name = "geom_cat")
  
  # 6) Priority carving
  cat_priority <- c("Ia","Ib","II","III","IV","V","VI")
  claimed        <- NULL
  exclusive_list <- list()
  
  for (cat in cat_priority) {
    this_sf <- dissolved %>% filter(IUCN_CAT == cat)
    if (nrow(this_sf) == 0) next
    
    this_geom <- this_sf$geom_cat[[1]]
    if (!is.null(claimed)) this_geom <- sf::st_difference(this_geom, claimed)
    
    area_exc_m2 <- sum(as.numeric(sf::st_area(this_geom)))
    
    # wrap geometry in list() so it becomes a list-column
    exclusive_list[[cat]] <- tibble(
      IUCN_CAT    = cat,
      geom_exc    = list(this_geom),
      area_exc_m2 = area_exc_m2
    )
    
    claimed <- if (is.null(claimed)) this_geom else sf::st_union(claimed, this_geom)
  }
  
  # 7) Build the sf summary
  pa_summary <- map_dfr(exclusive_list, identity) %>%
    sf::st_as_sf(sf_column_name = "geom_exc") %>%
    mutate(
      NA_L3CODE         = eco_row$NA_L3CODE,
      NA_L3NAME         = eco_row$NA_L3NAME,
      NA_L3KEY          = eco_row$NA_L3KEY,
      ecoregion_area_m2 = as.numeric(sf::st_area(eco_row)),
      pa_area_m2        = area_exc_m2,
      pct_protected     = 100 * area_exc_m2 / ecoregion_area_m2
    ) %>%
    select(NA_L3CODE, NA_L3NAME, NA_L3KEY, IUCN_CAT,
           ecoregion_area_m2, pa_area_m2, pct_protected, geometry = geom_exc)
  
  return(pa_summary)
}


# use this to make sure it works for an ecoregion!
#compute_for_one(eco_union %>% filter(NA_L3CODE == "10.1.1"))

#### 6) Loop over all ecoregions ####
results_list <- map(
  seq_len(nrow(eco_union)),
  ~ compute_for_one(eco_union[.x, ])
)

eco_pa_int <- bind_rows(results_list)

# If eco_pa_int still has geometry, drop it:
eco_pa_int_df <- eco_pa_int %>% st_drop_geometry()

#### 7) Pivot ####
# Pivot to wide, filling missing pa_area_m2 and pct_protected with zero
eco_pa_wide <- eco_pa_int_df %>%
  pivot_wider(
    names_from  = IUCN_CAT,
    values_from = c(pa_area_m2, pct_protected),
    names_sep   = "_",
    values_fill = list(
      pa_area_m2     = 0,
      pct_protected  = 0
    )
  )

#### 8) Data Checks ####
# make sure none of the pa_area_m2 columns are greater than ecoregion_area_m2
# make sure none of the pct_protected columns are greater than 100
if (any(eco_pa_wide$ecoregion_area_m2 < rowSums(select(eco_pa_wide, starts_with("pa_area_m2_"))))) {
  # compute the total PA area per row
  pa_totals <- rowSums(select(eco_pa_wide, starts_with("pa_area_m2_")))
  
  # which rows exceed?
  bad_idx <- which(eco_pa_wide$ecoregion_area_m2 < pa_totals)
  
  if (length(bad_idx)) {
    # print a small diagnostic table
    diag <- eco_pa_wide[bad_idx, ] %>%
      transmute(
        NA_L3CODE,
        ecoregion_area_m2,
        pa_total_m2 = pa_totals[bad_idx],
        diff_m2     = pa_total_m2 - ecoregion_area_m2
      )
    message("⚠️ These ecoregions have PA > ecoregion:")
    print(diag)
    stop("Some PA areas exceed ecoregion area!")
  }
  stop("Some PA areas exceed ecoregion area!")
}
if (any(eco_pa_wide %>% select(starts_with("pct_protected_")) > 100)) {
  stop("Some pct_protected values exceed 100%!")
}

# No NAs or negatives
if (any(is.na(eco_pa_wide$ecoregion_area_m2))) stop("Missing ecoregion area!")
if (any(select(eco_pa_wide, starts_with("pa_area_m2_")) < 0)) stop("Negative PA area found!")

# One row per ecoregion
if (nrow(eco_pa_wide) != nrow(eco_union)) {
  stop("Row count mismatch: ", nrow(eco_pa_wide), " vs. ", nrow(eco_union))
}

# Expected IUCN categories
expected_cats <- paste0("pa_area_m2_", c("Ia","Ib","II","III","IV","V","VI"))
missing <- setdiff(expected_cats, names(eco_pa_wide))
if (length(missing)) {
  stop("Missing IUCN category columns: ", paste(missing, collapse = ", "))
}

# Floating‐point tolerance
tol <- 1e-6
if (any(eco_pa_wide$ecoregion_area_m2 + tol < rowSums(select(eco_pa_wide, starts_with("pa_area_m2_"))))) {
  stop("Some PA areas exceed ecoregion area beyond tolerance!")
}

#### 9) Write CSV ####
# if output path does not exist, make it
if (!dir.exists(dirname(out_csv))) {
  dir.create(dirname(out_csv), recursive = TRUE)
}

write_csv(eco_pa_wide, out_csv)
message("Done! Summary written to: ", out_csv)
