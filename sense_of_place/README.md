# Sense of Place Domain

## Overview

The Sense of Place domain captures the cultural and ecological value that people attach to specific places and species in the western landscape. It recognizes that wildfire resilience is not only economic or physical — communities also care deeply about protecting the landmarks, parks, and animals that define their identity and connection to the land.

The domain is divided into two sub-components, each with its own scoring pipeline:

- **`iconic_places/`** — National parks, protected areas, and nationally significant historic structures
- **`iconic_species/`** — NatureServe-ranked at-risk species with strong cultural salience in the western US and Canada

---

## Sub-component 1: Iconic Places — `iconic_places/`

### Indicators
Iconic places are scored on their wildfire exposure, egress accessibility, proximity to fire resources, and WUI classification — reflecting both how much communities value these places and how well-protected they are against wildfire.

| Script | Description |
|--------|-------------|
| `01_national_park_polygons.R` | Reads and harmonizes US National Park boundary polygons (NPS) and Canadian national/provincial park polygons; reprojects to EPSG:5070 |
| `01_historic_structures.R` | Processes the National Register of Historic Places (NRHP) data for significant historic structures |
| `01-reproject_wui_raster_5070.R` | Reprojects the WUI raster from Infrastructure domain to EPSG:5070 for use in this domain |
| `02_places_presence_status.R` | Rasterizes park and historic structure presence to 90 m; computes the status indicator (presence/absence weighted by designation type) |
| `03_structures_egress.R` | Joins egress road network scores from the Infrastructure domain to iconic place locations |
| `03_structures_fire_resource_density.R` | Joins fire resource density scores to iconic place locations |
| `03_structures_wui.R` | Joins WUI classification to iconic place locations |
| `04_recovery_structures_degree_of_protection.R` | Computes a recovery indicator based on the degree of formal protection (National Park vs. state park vs. historic register) |
| `05_parks_resistance_recovery.R` | Assembles resistance and recovery scores for iconic places using egress, fire resource density, WUI, and degree-of-protection indicators |
| `06_score_calculation_iconic_places.R` | Computes the final iconic places sub-score (status, resistance, recovery, resilience) |

### QA scripts
- **`places_background_classify.R`** — Classifies NA types in iconic places output rasters.
- **`places_checks.R`** — Validates output layer extents and value ranges.

---

## Sub-component 2: Iconic Species — `iconic_species/`

### Indicators
Iconic species are scored based on their threat status (NatureServe global rank) and the spatial overlap of their ranges with fire-adapted landscapes, egress road quality, and fire resource density.

| Script | Description |
|--------|-------------|
| `01_nature_serve_species_threat_status.R` | Queries the NatureServe API for G-rank threat status of a curated list of iconic western species |
| `02_obtaining_species_ranges.R` | Coordinates range acquisition strategy across IUCN, GBIF, and manual sources |
| `02_gbif_species_concave_hull_ranges.R` | Derives species range polygons from GBIF occurrence data using concave hulls |
| `02_gbif_fish_species_hydrobasin_ranges.R` | Derives fish species ranges using HydroBASINS watershed polygons instead of concave hulls (more ecologically appropriate for aquatic species) |
| `03_status_species_range_scoring.R` | Scores each species' threat status within its range; rescales to a 0–1 indicator |
| `04_species_area_rasterize.R` | Rasterizes species ranges to 90 m; computes per-cell richness and status scores |
| `04_species_traits_rasterize.R` | Rasterizes species trait data (used for resistance/recovery scoring) to 90 m |
| `05_iconic_species_score_calculation.R` | Computes the final iconic species sub-score (status, resistance, recovery, resilience) |

### QA scripts
- **`species_background_classify.R`** — Classifies NA types in iconic species output rasters.
- **`species_checks.R`** — Validates output layer extents and value ranges.

---

## Data sources
- National Park Service (NPS) — park administrative boundaries
- Parks Canada — national and provincial park boundaries
- National Register of Historic Places (NRHP)
- USFS Wildland-Urban Interface dataset
- NatureServe — species G-rank threat status (via NatureServe API)
- GBIF — species occurrence records for range derivation
- IUCN — species range polygons where available
- HydroBASINS — hydrobasin polygons for fish range delineation

---

## Output
Final raster layers are written to `/home/shares/wwri-wildfire/final_layers/2024/sense_of_place/`.
