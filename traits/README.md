# Traits

## Overview

This folder contains the code used to compile and gap-fill species trait data for use in the Natural Habitats and Species domains of the WRI. Trait data describe fire-relevant biological characteristics of trees and other species (e.g., serotiny, resprouting capacity, seed dispersal mode, bark thickness, shade tolerance) that determine how vegetation communities resist and recover from wildfire.

The trait compilation draws from multiple global trait databases and is cross-walked to the species lists derived from TreeMap (US), SCANFI (Canada), and the IUCN/BirdLife range datasets used in the Species domain.

---

## Scripts — `scripts/`

### `plant_trait_filling.R`
The primary trait compilation script for trees and iconic plant species. Workflow:
1. Loads species lists from TreeMap (contiguous US) and the iconic species list.
2. Queries and joins trait data from multiple databases:
   - **TRY Plant Trait Database** (`rtry`) — broad trait coverage across plant species
   - **BIEN** — trait data for plant species in the Americas
   - **GBIF** — supplemental occurrence and taxonomic data
   - **BBBdb** — bark thickness and fire-resistance traits
   - **Serotinous species list** (Lamont et al. 2020) — cone serotiny by genus
3. Performs gap-filling using taxonomic hierarchy (genus → family → functional group) when species-level data are unavailable.
4. Outputs a trait table with one row per species and columns for each fire-relevant trait.

### `trait_filling_code_misc.R`
Supplemental trait filling for non-tree species (amphibians, fish, and other vertebrates) used in the Species domain. Workflow:
1. Loads the full WRI species list (all taxa, no subspecies/subpopulations).
2. Queries **AmphiBIO** for amphibian traits.
3. Queries **FishBase** (via `rfishbase`) for fish traits.
4. Performs gap-filling and outputs species-level trait tables for downstream use.

---

## Trait databases referenced

| Database | Taxa | Key traits |
|----------|------|------------|
| TRY Plant Trait Database | Plants | Leaf area, bark thickness, seed mass, height, wood density |
| BIEN | Plants (Americas) | Trait coverage for western species |
| BBBdb (2017) | Plants | Bark thickness, cambium protection |
| Lamont et al. 2020 | Plants | Serotinous genera list |
| AmphiBIO | Amphibians | Habitat type, reproduction, desiccation tolerance |
| FishBase | Fish | Habitat, body size, reproduction |

---

## Notes
- Set **`WRI_PROJECT_ROOT`** so that `file.path(wri_project_root, "data", "multi-domain-data", "traits")` resolves to your traits working directory (see the repository root [README](../README.md)). The scripts call `setwd()` into that folder because downstream paths are expressed relative to `data/spp_lists`, etc.
- This code is upstream of the Natural Habitats and Species domain pipelines. Outputs feed into `natural_habitats/01_treemap/04_rescale_treemap_tree_traits.R` and `species/03_prep_resilience/04_prep_traits_data.R`.
- Trait gap-filling is intentionally conservative: only fills when a clear taxonomic match exists; unfilled values are left as NA rather than imputed with global means.
