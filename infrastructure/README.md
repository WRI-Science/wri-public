# Infrastructure Domain

## Overview

The Infrastructure domain assesses the physical built environment's capacity to reduce wildfire ignition and support evacuation, firefighting, and recovery. It encompasses egress road network quality, defensible space around structures, wildland-urban interface (WUI) exposure, fire resource density, and building code adoption.

Scores are computed at 90 m resolution for the full WRI study area and are organized into **resistance** (capacity to withstand fire) and **recovery** (capacity to rebuild) sub-components.

---

## Structure

```
infrastructure/
├── resistance/
│   ├── 01_building_codes.R
│   ├── 1-road_networks/          # Egress road network analysis
│   ├── defensible_space/         # Building footprint and vegetation analysis
│   ├── fire_resource_density/    # Fire station and resource proximity
│   └── wildland_urban_interface/ # WUI classification and rescaling
├── recovery/
│   └── home_ownership.R
├── infrastructure_background_classify.R
├── infrastructure_checks.R
└── infrastructure_score_calculation.R
```

---

## Resistance indicators

### Building codes — `resistance/01_building_codes.R`
Processes state- and provincial-level building code adoption data (e.g., adoption year, version, wildfire-specific standards) and converts it to a scored raster indicator reflecting the degree to which local codes protect structures from ignition.

### Egress road networks — `resistance/1-road_networks/`
This is the most computationally intensive component of the Infrastructure domain. It models evacuation capacity using road network graphs built from OpenStreetMap.

| Script | Description |
|--------|-------------|
| `1-canada-designated-places.ipynb` | Identifies and assigns Canadian communities (designated places) as evacuation origin nodes |
| `2-us-designated-places.ipynb` | Same for US designated places using Census TIGER data |
| `3-download-road-networks.ipynb` | Downloads road networks for each community via OSMnx; saves graph files |
| `4-road-network-calculation.ipynb` | Computes egress metrics (road network density, accessibility, connectivity) for each community |
| `roads_rescale.R` | Rescales raw egress metrics to 0–1 for indicator scoring |

### Defensible space — `resistance/defensible_space/`
Measures the ratio of vegetation to impervious/built surface within a 10 m buffer of building footprints — a direct proxy for structure ignitability.

| Script | Description |
|--------|-------------|
| `01_download_building_polygon_tiles.ipynb` | Downloads Microsoft building footprint polygon tiles for the study area |
| `02_download-esri-landcover.ipynb` | Downloads ESRI 10 m land cover tiles to classify vegetation vs. built surface |
| `03_create_defensible_space_polygons.ipynb` | Buffers building footprints and intersects with land cover |
| `04_calculate_difensible_space.ipynb` | Calculates vegetation percentage within the 10 m buffer per building |
| `05_calculate_percent_trees_and_rescale.R` | Computes final percent-tree defensible space indicator and rescales to 0–1 |

### Fire resource density — `resistance/fire_resource_density/`
Estimates proximity to firefighting resources (fire stations, air tanker bases) using kernel density estimation.

| Script | Description |
|--------|-------------|
| `01_yt_fire_resources.ipynb` | Collects Yukon fire resource location data |
| `02_fire_resource_proximity_kde_rescaled.R` | Applies KDE across fire resource locations and rescales the density surface |

### Wildland-urban interface — `resistance/wildland_urban_interface/`
Classifies and rescales the USFS WUI raster to reflect community exposure at the wildland-urban interface.

| Script | Description |
|--------|-------------|
| `01_wildland-urban-interface.ipynb` | Downloads and processes the USFS WUI classification data |
| `02_wui_rescaled.ipynb` | Rescales WUI classes to a 0–1 score for the indicator layer |

---

## Recovery indicators

### Home ownership — `recovery/home_ownership.R`
Processes US ACS and Canadian Census homeownership rates at census-tract / CSD level. Homeownership is used as a proxy for financial capacity to rebuild after a wildfire.

---

## Final score
- **`infrastructure_score_calculation.R`** — Reads all processed resistance and recovery indicator rasters (building codes, egress, fire resource density, defensible space, WUI, homeownership) and computes the domain-level resistance, recovery, resilience, and final score rasters.

---

## QA / Validation scripts
- **`infrastructure_background_classify.R`** — Classifies NA types in output rasters.
- **`infrastructure_checks.R`** — Validates final layer extents, CRS, and value ranges.

---

## Data sources
- OpenStreetMap (road networks, via OSMnx)
- Microsoft Building Footprints
- ESRI 10 m Land Cover
- USFS Wildland-Urban Interface dataset
- US Fire Administration / provincial fire agency directories
- US ACS / Statistics Canada (homeownership)
- State and provincial building code adoption records

---

## Output
Final raster layers are written under **`{WRI_PROJECT_ROOT}/final_layers/<year>/infrastructure/`** (see the repository root [README](../README.md) for configuring `WRI_PROJECT_ROOT`).
