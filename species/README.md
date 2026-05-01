# Species Domain

## Overview

The Species domain measures the resilience of wildlife communities to wildfire at the landscape scale. Rather than focusing on a curated list of iconic species (that is covered by Sense of Place), this domain takes a broad, richness-based approach: it assesses the threat status of all vertebrate and plant species present in each 90 m cell, and scores each cell based on the extinction risk and range characteristics of species found there.

Status reflects aggregate threat level; resistance and recovery reflect how species' biological traits (range size, habitat specificity, traits related to fire response) position them to withstand and bounce back from fire disturbance.

---

## Pipeline

### Shared functions — `00_species_custom_functions.R`
Defines reusable functions used across the pipeline:
- `valid_check()` — Validates and repairs `sf` geometries; logs any species whose shapes cannot be made valid.
- `prepare_iucn()` — Standardizes column names in IUCN range data.
- `prepare_birdlife()` — Combines BirdLife International range shapefiles with AVONET trait data; repairs multisurface geometries.
- `process_species_for_status_or_resilience()` — Parallelized processing pipeline: transforms CRS, intersects with the study area, and checks geometry validity for each species.
- `get_spp_info()` — Queries the IUCN Red List API for threat status and habitat information for species present in the study area.

### Step 1 — Raw species data preparation
- **`01_prep_raw_spp_data.R`** — Reads IUCN spatial range shapefiles and BirdLife data; filters to potential species of interest (terrestrial, non-marine); performs initial geometry cleaning and CRS standardization.

### Step 2 — Status preparation — `02_prep_status/`
| Script | Description |
|--------|-------------|
| `01_filter_spp_data_for_status.R` | Filters the cleaned species dataset to those relevant for status scoring (present, extant populations) |
| `02_rasterize_spp_for_status.R` | Rasterizes species ranges to 90 m cells; records which species are present in each cell |
| `03_prep_spp_threat_statuses.R` | Joins IUCN Red List threat categories (LC, NT, VU, EN, CR) to each species; converts categories to numeric extinction risk scores |

### Step 3 — Resilience preparation — `03_prep_resilience/`
| Script | Description |
|--------|-------------|
| `01_filter_spp_data_for_resilience.R` | Filters the dataset to species used for resistance and recovery scoring |
| `02_intersect_spp_for_resilience.R` | Intersects species ranges with the study area at fine resolution for accurate range-size computation |
| `03_prep_spp_range_sizes.R` | Calculates each species' range size within the study area — smaller ranges indicate higher vulnerability (lower resilience) |
| `04_prep_traits_data.R` | Joins species trait data (from TreeMap, SCANFI, and supplemental sources) used to score fire-response capacity |

### Step 4 — Indicator score calculation
- **`04_calculate_status_resistance_recovery_indicator_scores.R`** — Joins status (threat category × presence) with resilience (range size × traits) data at the species level. Creates raster files for status (rescaled to 0–1 based on 75th percentile extinction risk per cell), resistance, and recovery. Writes unaligned indicator rasters.

### Step 5 — Final layer assembly
- **`05_assemble_final_layers.R`** — Aligns all status, resistance, and recovery indicator rasters to the 90 m study area template; calculates final resilience and domain score rasters.

### Step 6 — QA checks
- **`06_species_final_layer_checks.R`** — Validates the final output layers: checks that resistance-only scenarios show no positive differences, classifies NA types by their cause (outside study area, no range data, masked).

---

## Data sources
- IUCN Red List — species range shapefiles and threat status (via Red List API)
- BirdLife International — bird species range polygons
- AVONET — bird trait data (habitat type, foraging strategy — used to filter marine birds)
- USFS TreeMap / SCANFI — tree species trait data (used in `04_prep_traits_data.R`)

---

## Notes
- This domain was originally developed under the folder name `biodiversity/` in the internal repository. All public-facing code and documentation use `species/` to match the WRI's published terminology.
- Processing is highly parallelized. Running `01_prep_raw_spp_data.R` and the rasterization steps on large IUCN datasets requires substantial RAM (>100 GB recommended).

---

## Output
Final raster layers are written under **`{WRI_PROJECT_ROOT}/final_layers/<year>/biodiversity/`**. That folder name is intentional: it matches the legacy internal layout on disk even though this public repo uses the **`species/`** directory for code and docs. Do **not** use a `species/` subdirectory under `final_layers/` unless you have reorganized storage yourself.

Configure **`WRI_PROJECT_ROOT`** as described in the repository root [README](../README.md).
