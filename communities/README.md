# Communities Domain

## Overview

The Communities domain measures the social and institutional capacity of communities to withstand and recover from wildfires. It captures formal preparedness infrastructure (community wildfire protection plans, firewise programs, fire station density), social vulnerability (age, disability, vehicle access), and broader measures of civic incorporation.

Scores are computed at 90 m resolution across the WRI study area (12 western US states plus British Columbia and Yukon) using harmonized US Census and Canadian Census data combined with spatial program registries.

---

## Pipeline

### Step 0 — Study area raster
- **`00_90m_study_area_raster.R`** — Generates the 90 m Albers Equal Area (EPSG:5070) and Mollweide study area raster templates used throughout this domain.

### Step 1–3 — Census data ingestion and harmonization
- **`01_acs_variables_of_interest.R`** — Pulls US ACS (American Community Survey) variables at census-tract level for the western states: age 65+, disability status, no-vehicle households, and social vulnerability indicators.
- **`02_can_census_variables_of_interest.R`** — Pulls equivalent Canadian Census variables at the census subdivision (CSD) level for BC and Yukon.
- **`03_combine_us_can_census_variables.R`** — Harmonizes and merges the US and Canadian census tables into a single cross-border dataset.

### Step 4 — Resistance indicators
- **`04_volunteer_fire_departments.R`** — Geocodes volunteer fire department locations from USFA and Canadian provincial directories; computes a kernel density estimate (KDE) of station proximity rasterized to 90 m.
- **`05_firewise_communities.R`** — Processes the NFPA Firewise USA community registry (US and Canada); rasterizes enrolled community boundaries to produce a binary/scored resistance layer.
- **`06_cwpps.R`** — Processes Community Wildfire Protection Plans (CWPPs) from CONUS spatial data and a supplemental tabular source; rasterizes CWPP coverage areas.
- **`07_incorporation.R`** — Encodes municipal incorporation status as a proxy for community governance capacity.

### Step 4 — Final raster assembly (final score)
- **`04_calculate_final_rasters.R`** — Reads all processed indicator rasters (CWPPs, Firewise, volunteer fire stations, age, disability, vehicle access, egress roads from Infrastructure domain) and assembles the composite Communities domain resistance score.

### Step 5 — Layer checks
- **`05_communities_final_layer_checks.R`** — Validates output rasters for extent, CRS alignment, and value range.

### Visualization and QA
- **`08_viz_for_rescaling.R`** — Plots indicator distributions to guide rescaling decisions.
- **`13_scores_viz.R`** — Visualizes final domain score outputs for QA review.
- **`conus_cwwp_viz.R`** — Generates a map of CWPP coverage across CONUS for verification.

---

## Data sources
- US Census Bureau / American Community Survey (ACS)
- Statistics Canada — Census of Population
- US Fire Administration (USFA) — volunteer fire department registry
- NFPA Firewise USA — community enrollment data
- USFS / NFPA — CWPP boundary and attribute data
- Census TIGER shapefiles (US); Statistics Canada boundary files (Canada)

---

## Output
Final raster layers are written under **`{WRI_PROJECT_ROOT}/final_layers/<year>/communities/`** (see the repository root [README](../README.md) for configuring `WRI_PROJECT_ROOT`).
