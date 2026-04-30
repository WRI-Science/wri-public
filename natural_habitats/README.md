# Natural Habitats Domain

## Overview

The Natural Habitats domain is the most structurally complex in the WRI. It assesses the ecological condition of natural vegetation across the western landscape and its capacity to resist and recover from wildfire disturbance. Indicators span protected area coverage, vegetation diversity, stand density, tree traits (seed dispersal, shade tolerance, resprouting), drought stress, NDVI variability, and net primary productivity.

All processing occurs at 90 m resolution and is organized by ecoregion to account for the wide ecological variation across the study area. The pipeline is structured into six numbered stages, each building on outputs from the previous.

---

## Pipeline structure

```
natural_habitats/
├── 01_cec_protected_areas/        # Protected area coverage
├── 01_historical_landcover/       # Historical natural land cover baseline
├── 01_SCANFI/                     # Canadian forest inventory (SCANFI) traits and structure
├── 01_treemap/                    # US forest inventory (TreeMap) traits and structure
├── 02_present_landcover/          # Present-day land cover and masking
├── 03_diversity/                  # Combined diversity indicator
├── 03_NDVI/                       # NDVI variability (vegetation condition)
├── 03_NPP/                        # Net primary productivity
├── 03_precipitation/              # 30-year precipitation trends
├── 03_stand_density/              # Forest stand density
├── 03_tree_traits_recovery/       # Tree traits contributing to recovery
├── 03_tree_traits_resistance/     # Tree traits contributing to resistance
├── 03_VPD/                        # Vapor pressure deficit (drought stress)
├── 04_recovery/                   # Assembled recovery sub-score
├── 04_resistance/                 # Assembled resistance sub-score
├── 04_status/                     # Assembled status sub-score
├── 05_resilience/                 # Resilience score
└── 06_final_score/                # Final domain score
```

---

## Stage 1 — Raw data ingestion and indicator preparation

### Protected areas — `01_cec_protected_areas/`
- **`01_calculate_percent_protected.R`** — Intersects the CEC North America Protected Areas 2025 geodatabase with EPA Level III ecoregions to compute percent of each ecoregion that is under formal protection. Uses `sf` and GDAL for geometry repair.
- **`02_rescale_for_status_calc.R`** — Rescales protection percentages to a 0–1 status indicator by ecoregion.

### Historical land cover — `01_historical_landcover/`
Establishes a pre-development natural land cover baseline used to compute how much natural habitat remains in the present day.
- **`01_determine_needed_files_from_raw.R`** — Scans raw NLCD/CCRS tiles to identify which are needed for the study area.
- **`02_make_merged_raw_files.R`** — Mosaics raw tiles into study-area-wide rasters.
- **`03_calculate_ecoregion_landcover_counts.R`** — Tabulates land cover type pixel counts by ecoregion and historical time period.
- **`04_calculate_historical_percent_natural.R`** — Derives the historical percent-natural reference value per ecoregion.

### SCANFI (Canadian forest inventory) — `01_SCANFI/`
Processes the Spatially Continuous Annual Forest Inventory (SCANFI) data for British Columbia and Yukon to extract tree diversity, stand density, and tree traits.
- **`01_download_scanfi_data.py`** — Downloads SCANFI tiles from the Canadian Forest Service.
- **`02_convert_raw_rasters_to_df.py`** — Converts raster tiles to tabular data frames for processing.
- **`03_merge_coverage_dfs.R`** — Merges coverage data frames by ecoregion.
- **`04_generate_xy_ecoregion_df.R`** — Joins spatial coordinates to ecoregion assignments.
- **`05_generate_and_rescale_tree_traits.R`** — Extracts and rescales trait values (seed dispersal, shade tolerance, resprouting capacity) for SCANFI pixels.
- **`05_rescale_scanfi_diversity.R`** / **`05_rescale_scanfi_stand_density.R`** — Rescales diversity and stand density metrics by ecoregion.

### TreeMap (US forest inventory) — `01_treemap/`
Processes the USFS TreeMap 2016 product for US western states to extract equivalent tree traits and structure metrics.
- **`01_extract_xy_and_tm_id_to_df.py`** — Extracts XY coordinates and TreeMap IDs from raster data.
- **`02_make_various_df_for_indicator_processing.R`** — Prepares species-level data frames for each indicator type.
- **`03_generate_xy_ecoregion_df.R`** — Assigns TreeMap pixels to EPA ecoregions.
- **`04_rescale_treemap_diversity.R`** / **`04_rescale_treemap_stand_density.R`** / **`04_rescale_treemap_tree_traits.R`** — Rescales diversity, stand density, and tree trait values by ecoregion.

---

## Stage 2 — Present land cover and masking

### `02_present_landcover/`
Processes ESRI 10 m annual land cover (2024) to classify the current landscape and create agricultural, urban, and natural masks used throughout the domain.
- **`01_download_and_reproject_landcover_tiles_by_ecoregion.py`** — Downloads ESRI 10 m land cover tiles by ecoregion.
- **`02_merge_landcover_tiles_into_ecoregions.py`** — Mosaics tiles into ecoregion-level rasters.
- **`03_calculate_ecoregion_zonal_stats.py`** — Computes pixel-level zonal statistics by ecoregion.
- **`04_make_ag_urban_rangeland_and_bare_10m_masks.py`** — Creates 10 m binary masks for agricultural, urban, rangeland, and bare cover classes.
- **`05_make_90m_ecoregion_masks.R`** — Aggregates 10 m masks to 90 m.
- **`06_full_90_masks.R`** — Assembles the full study-area-wide 90 m masks.
- **`07_calculate_percent_natural_change.R`** — Computes the change from historical to present natural cover, forming the status indicator.

---

## Stage 3 — Continuous environmental indicators

### NDVI variability — `03_NDVI/`
Measures inter-annual NDVI variability (rolling standard deviation) as a proxy for vegetation stress and stability.
- **`01_calculate_NDVI_AppEEARS_Rolling_sd.R`** — Processes NASA AppEEARS NDVI time series (~every 16 days) using parallel computation; calculates rolling standard deviations.
- **`02_rescale_NDVI_using_annual_mean_and_min.R`** — Rescales NDVI variability to a 0–1 indicator by ecoregion.

### Net primary productivity — `03_NPP/`
- **`01_rescale_npp_by_ecoregion.R`** — Processes MODIS MOD17A3HGF annual NPP data (via AppEEARS) and rescales to a 0–1 productivity indicator by ecoregion.

### Vapor pressure deficit — `03_VPD/`
- **`01_vpd_rescaling_convert_to_90m.R`** — Processes gridded VPD data (drought stress indicator) and rescales to 90 m.

### Diversity — `03_diversity/`
- **`01_join_treemap_and_scanfi_diversity_data.R`** — Combines TreeMap (US) and SCANFI (Canada) tree species diversity values into a single cross-border diversity indicator raster.

### Precipitation — `03_precipitation/`
- **`01_make_annual_summary_precip_files.R`** — Summarizes annual precipitation from gridded climate data.
- **`02_make_30_year_stack_and_summaries.R`** — Creates a 30-year climatological stack and summary statistics.
- **`03_rescale_w_stats_and_make_90m_raster.R`** — Rescales the precipitation indicator by ecoregion.

### Stand density — `03_stand_density/`
- **`01_join_treemap_and_scanfi_density_data.R`** — Merges US and Canadian stand density values into a single indicator layer.

### Tree traits (resistance and recovery) — `03_tree_traits_resistance/` and `03_tree_traits_recovery/`
- **`01_join_treemap_and_scanfi_resistance_tree_traits.R`** — Combines resistance-relevant tree traits (e.g., bark thickness, fire tolerance) from TreeMap and SCANFI.
- **`01_join_treemap_and_scanfi_recovery_tree_traits.R`** — Combines recovery-relevant tree traits (e.g., resprouting capacity, seed dispersal mode).

---

## Stages 4–6 — Score assembly

### `04_status/`, `04_resistance/`, `04_recovery/`
- Each contains a single script that joins the relevant stage-3 indicators into a composite sub-score raster.

### `05_resilience/`
- **`01_calculate_resilience.R`** — Computes the resilience score from resistance and recovery using the WRI formula.

### `06_final_score/`
- **`01_generate_final_score.R`** — Combines status and resilience scores into the final Natural Habitats domain score. Runs with 12 parallel threads; takes ~11 minutes on the Aurora server.

---

## QA / Validation
- **`natural_habitats_background_classify.R`** (in parent `domains/` repo — not copied here) — NA classification.
- **`natural_habitats_checks.R`** — Final layer validation.

---

## Data sources
- CEC North America Protected Areas 2025
- NLCD (National Land Cover Database) / CCRS (Canada) — historical land cover
- ESRI 10 m Annual Land Cover 2024
- NASA MODIS AppEEARS — NDVI (MOD13Q1) and NPP (MOD17A3HGF)
- PRISM / Daymet — gridded precipitation and VPD
- USFS TreeMap 2016 — US forest inventory
- SCANFI — Canadian Spatially Continuous Annual Forest Inventory
- EPA Level III Ecoregions of North America

---

## Output
Final raster layers are written to `/home/shares/wwri-wildfire/final_layers/2024/natural_habitats/`.
