# Templates and Functions

## Overview

This folder contains shared R and Python utilities used across multiple WRI domains. Every domain sources one or more of these scripts rather than reimplementing common operations. Keeping this logic centralized ensures consistency in raster alignment, gapfilling methods, and score calculation formulas across all 8 domains.

---

## Contents

### `wri_paths.R`
Defines `wri_project_root` from the environment variable `WRI_PROJECT_ROOT`, falling back to the historical team server layout when unset. Helpers `wri_data(...)` and `wri_final_layers(...)` build paths under `{root}/data/` and `{root}/final_layers/`.

Scripts may source this file (often via `here::here("templates_and_functions", "wri_paths.R")`) or set `wri_project_root` inline with the same `Sys.getenv` pattern — see the root **README** section *Configuring data paths*.

### `align_raster_to_template.R`
The most widely used utility in the entire codebase. Defines a function that reprojects, resamples, and crops any input raster to match the 90 m EPSG:5070 study area template raster. All final indicator layers must pass through this function before being used in score calculations.

**Used by:** every domain's score calculation script.

### `base_script_template.R`
A blank-slate starting template for new domain indicator scripts. Pre-populates the standard header structure (goal, packages, file paths, boundary layers, functions, data ingestion, processing, output) to enforce consistency across the codebase.

### `score_calculation_template.R`
A template for the final score calculation step in each domain. Provides the standard formulas and structure for computing:
- **Status score**
- **Resistance score**
- **Recovery score**
- **Resilience score** = `1 - (1 - resistance) * (1 - recovery)`
- **Domain score** = geometric mean of status and resilience

All domain score calculation scripts derive from this template.

---

## Notes
- All scripts in this folder are sourced using `here::here("templates_and_functions", "<script_name>")` from within domain scripts.
- Do not modify these files without verifying that changes are backward compatible with all domains that source them.
