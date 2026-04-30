# Wildfire Resilience Index (WRI) — 2024 Snapshot

**Publication:** "An index to assess wildfire resilience across the North American West"

**Organization:** National Center for Ecological Analysis and Synthesis (NCEAS), UC Santa Barbara

**Website:** [wildfireindex.org](https://wildfireindex.org)

---

## Overview

This repository contains the code used to generate the 2024 snapshot of the Western Wildfire Resilience Index (WRI) — the first open-access, holistic tool to measure wildfire resilience across communities in the western United States and Canada.

The WRI is modeled after the Ocean Health Index and calculates scores across 8 domains for communities throughout 12 western US states plus British Columbia and Yukon. Scores reflect status, resistance, and recovery dimensions of wildfire resilience.

---

## Domains

| Domain | Description |
|---|---|
| `air_quality/` | Days above AQI thresholds; vulnerable population exposure |
| `communities/` | Social cohesion, community preparedness programs, fire station density |
| `infrastructure/` | Egress road networks, defensible space, WUI, fire resource density |
| `livelihoods/` | Income, unemployment, housing burden |
| `natural_habitats/` | Protected area coverage, vegetation diversity, drought stress, productivity |
| `sense_of_place/` | Iconic species and places — national parks, historic structures |
| `species/` | Species richness, threat status, range-based resilience |
| `water/` | Surface water quantity and timing, drought planning |

---

## Shared utilities

- `templates_and_functions/` — Shared R and Python functions used across domains (gapfilling, raster alignment, score calculation templates)
- `traits/` — Code used to generate tree trait data used in Natural Habitats and Species domains

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

## Citation

If you use this work, please cite this project — either by linking back to this repository or by acknowledging the Western Wildfire Resilience Index in your references. Structured citation metadata for GitHub’s “Cite this repository” button is in [`CITATION.cff`](CITATION.cff).

---

## Contact

[NCEAS](https://www.nceas.ucsb.edu) | [Wildfire Resilience Index](https://wildfireindex.org)
