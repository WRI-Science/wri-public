# Wildfire Resilience Index (WRI) — 2024 Snapshot

**Publication:** "An index to assess wildfire resilience across the North American West"

**Organization:** National Center for Ecological Analysis and Synthesis (NCEAS), UC Santa Barbara

**Website:** [wildfireindex.org](https://wildfireindex.org)

---

## Overview

This repository contains the code used to generate the 2024 snapshot of the Wildfire Resilience Index (WRI) — the first open-access, holistic tool to measure wildfire resilience across communities in the western United States and Canada.

The WRI is modeled after the Ocean Health Index and calculates scores across 8 domains for communities throughout 12 western US states plus British Columbia and Yukon. Scores reflect status, resistance, and recovery dimensions of wildfire resilience.

---

## Domains

Each domain has its own `README.md` with a full script-by-script breakdown. Below is a summary of what each domain measures and how it is structured.

---

### `air_quality/`
Measures wildfire smoke exposure and community vulnerability to poor air quality. The status indicator captures the number of days per year that AQI exceeds 100 and 300 across western US and Canadian monitoring sites, spatially interpolated to 90 m using inverse distance weighting. The resistance indicator characterizes vulnerable population exposure — combining age (65+), disability, no-vehicle households, asthma and COPD prevalence, and outdoor agricultural worker density. Data sources include EPA AQI monitoring records, US ACS / Statistics Canada census data, and BRFSS health surveillance.

---

### `communities/`
Measures the social and institutional capacity of communities to prepare for and withstand wildfires. Resistance indicators include volunteer fire station density (kernel density), Firewise USA enrollment, Community Wildfire Protection Plan (CWPP) coverage, and civic incorporation status. Vulnerability indicators (age, disability, vehicle access) from US ACS and Canadian Census are incorporated. All census variables are harmonized across the US–Canada border into a single cross-national dataset.

---

### `infrastructure/`
Assesses the built environment's capacity to support evacuation, suppress ignition, and enable recovery. This is the most technically complex domain, structured into `resistance/` and `recovery/` components:

- **Egress road networks** (`resistance/1-road_networks/`) — Models evacuation capacity using OpenStreetMap road graphs (OSMnx) for thousands of US and Canadian communities. Network connectivity and accessibility metrics are computed and rescaled.
- **Defensible space** (`resistance/defensible_space/`) — Measures vegetation density within 10 m of building footprints using Microsoft Building Footprints and ESRI 10 m land cover.
- **Wildland-urban interface** (`resistance/wildland_urban_interface/`) — Rescales the USFS WUI classification to reflect exposure risk.
- **Fire resource density** (`resistance/fire_resource_density/`) — KDE of fire station and air tanker base locations.
- **Building codes** (`resistance/01_building_codes.R`) — Scores adoption of wildfire-specific building standards by state and province.
- **Home ownership** (`recovery/home_ownership.R`) — US and Canadian homeownership rates as a proxy for financial capacity to rebuild.

---

### `livelihoods/`
Captures the economic vulnerability of households to wildfire disruption. Three status indicators — housing cost burden, median income, and unemployment — are drawn from US ACS and Statistics Canada at census-tract / census-subdivision level, harmonized across the border, and rasterized to 90 m. Resistance and recovery scores are derived from these indicators alongside a Shannon diversity index of local industry employment (NAICS codes), which measures economic diversification as a buffer against wildfire-driven job loss.

---

### `natural_habitats/`
The most structurally complex domain, measuring the ecological condition and fire resilience of natural vegetation across the western landscape. The pipeline runs in six stages:

1. **Raw data** — Protected area coverage (CEC North America database), historical land cover baselines (NLCD/CCRS), and tree species data from SCANFI (Canada) and TreeMap (US).
2. **Present land cover** — ESRI 10 m annual land cover (2024) processed to produce agricultural/urban masks and percent-natural-change status indicators.
3. **Continuous indicators** — NDVI variability (NASA AppEEARS), net primary productivity (MODIS MOD17A3HGF), vapor pressure deficit (drought stress), precipitation trends, forest diversity, stand density, and tree traits (resistance and recovery).
4. **Sub-scores** — Status, resistance, and recovery assembled by joining stage-3 indicators.
5. **Resilience** — Computed from resistance and recovery.
6. **Final score** — Domain score generated using 12 parallel threads (~11 min on the Aurora server).

All processing is by EPA Level III ecoregion to account for ecological variation.

---

### `sense_of_place/`
Captures the cultural and ecological value attached to specific places and species in the western landscape. Divided into two sub-components:

- **`iconic_places/`** — Scores US National Parks, Canadian national/provincial parks, and nationally significant historic structures based on their wildfire exposure, egress accessibility, fire resource proximity, WUI classification, and degree of formal protection. Data from NPS, Parks Canada, and the National Register of Historic Places.
- **`iconic_species/`** — Scores a curated list of culturally salient western species (vertebrates, fish, plants) using NatureServe threat status, GBIF occurrence-derived ranges, and concave hull / HydroBASINS range derivation methods. Fish species ranges use hydrobasin polygons rather than concave hulls.

---

### `species/`
Takes a landscape-wide, richness-based approach to species resilience — assessing aggregate extinction risk and biological fire-response capacity across all vertebrate and plant species present in each 90 m cell. Unlike Sense of Place (which uses a curated iconic species list), this domain covers all species with IUCN or BirdLife range data that intersect the study area.

The pipeline: (1) cleans and filters IUCN / BirdLife range shapefiles, (2) rasterizes ranges and joins IUCN Red List threat categories, (3) computes range-size and trait-based resilience scores, (4) generates per-cell status (weighted by extinction risk), resistance, and recovery rasters, and (5) assembles the final domain score. Processing is heavily parallelized and memory-intensive (>100 GB RAM recommended for rasterization steps).

*Note: This domain was called `biodiversity/` in the internal repository. All public-facing code uses `species/` to match WRI published terminology.*

---

### `water/`
Assesses water resource condition and water management preparedness. Structured around four sub-components:

- **Surface water** (`surface-water/`) — Measures inter-annual streamflow stability using 30-year USGS NWIS and Canadian hydrometric records assigned to HydroBASINS Level 8 watersheds. More stable flow = higher resistance.
- **Stream gauge retrieval** (`nwis/`) — Handles bulk data download from the USGS NWIS API (sequential and parallelized versions).
- **Drought planning** (`drought-plans/`) — Scores US states and Canadian provinces on the quality of formal drought preparedness plans; rasterized to 90 m.
- **Drinking water systems** (`epa-water-treatment/`) — Scores county-level drinking water system reliability using EPA SDWIS violation records and number of community water systems.

The final score is the mean of the drought plan and water treatment indicators.

---

## Shared utilities

- **`templates_and_functions/`** — Shared R utilities used across all domains: `wri_paths.R` (project root via `WRI_PROJECT_ROOT`), `align_raster_to_template.R` (reprojects/resamples any raster to the 90 m study area grid), `score_calculation_template.R` (standard formulas for status, resistance, recovery, resilience, and domain scores), and `base_script_template.R` (blank starting template for new indicator scripts).
- **`traits/`** — Compiles and gap-fills fire-relevant biological trait data (bark thickness, serotiny, resprouting, seed dispersal, shade tolerance) for trees and vertebrates. Sources include TRY, BIEN, BBBdb, AmphiBIO, and FishBase. Outputs feed into both the Natural Habitats and Species domain pipelines.

---

## System Requirements

The analysis was run on a Dell PowerEdge R7625 server:
- **OS:** Ubuntu Server 22.04 LTS
- **CPU:** 2 × AMD EPYC 9634 (168 physical / 336 logical cores)
- **Memory:** 2.25 TB DDR5-4800 ECC RAM
- **GPU:** NVIDIA A40 (48 GB)

Scripts are a mix of R and Python. Processing data at 90m resolution across 12 western states plus BC and Yukon is computationally intensive. Users looking to rerun portions of the analysis will need a machine of comparable capability. Reported runtimes in scripts reflect the above hardware.

---

## Data

The input data for this analysis exceeds 10 terabytes and will be made available upon request or once the article has been accepted for publication. Final output layers (Cloud-Optimized GeoTIFFs) are available on [KNB](https://knb.ecoinformatics.org/data/wri-data-processing/cogs/) and [Source Cooperative](https://source.coop).

---

## Configuring data paths

Analysis scripts expect a single **project root** directory that contains at least:

- `data/` — domain inputs and intermediates (for example `data/air_quality/`, `data/multi_domain_data/`).
- `final_layers/` — processed indicators and scores (for example `final_layers/2024/air_quality/`).

Set the environment variable **`WRI_PROJECT_ROOT`** to that directory. If it is not set, scripts default to the historical internal path `/home/shares/wwri-wildfire` so existing deployments keep working without changes.

Copy [`.Renviron.example`](.Renviron.example) to `.Renviron` in the repo root (see also [.gitignore](.gitignore); `.Renviron` is ignored by git) or export `WRI_PROJECT_ROOT` in your shell before running R or Python.

Optional shared helpers live in [`templates_and_functions/wri_paths.R`](templates_and_functions/wri_paths.R).

Some notebooks use older folder spellings under `data/` (for example `multi-domain-data` with hyphens) while many R scripts use underscores (`multi_domain_data`). Keep the same relative structure as on your storage machine, or use symlinks so both layouts resolve.

---

## Citation

If you use this work, please cite this project — either by linking back to this repository or by acknowledging the Wildfire Resilience Index in your references. Structured citation metadata for GitHub’s “Cite this repository” button is in [`CITATION.cff`](CITATION.cff).

---

## Contact

[NCEAS](https://www.nceas.ucsb.edu) | [Wildfire Resilience Index](https://wildfireindex.org)
