# Livelihoods Domain

## Overview

The Livelihoods domain measures the economic vulnerability of communities to wildfire disruption. It focuses on three household-level economic indicators — housing cost burden, median income, and unemployment — and combines them into status, resistance, and recovery scores.

Data are sourced from the US Census Bureau and Statistics Canada, harmonized across the international border, and rasterized to 90 m resolution across the full WRI study area.

---

## Pipeline

### Status indicators
- **`01_status_housing_burden.R`** — Calculates the percentage of housing units spending 30%+ of income on housing costs. In the US this is derived from ACS "rent-burdened" housing tenure variables; in Canada from Statistics Canada's housing cost burden statistics. Data are pulled at census-tract (US) and census subdivision (Canada) level, then rasterized to 90 m.
- **`01_status_median_income.R`** — Processes median household income from ACS (US) and the Canadian Census. Income is normalized and rescaled to form a status indicator.
- **`01_status_unemployment.R`** — Calculates unemployment rates for both countries from census sources and rescales them.

### Resistance and recovery
- **`02_livelihoods_resistance_recovery.R`** — Computes resistance and recovery sub-scores from the status indicators. Resistance reflects economic capacity to withstand income disruption; recovery reflects economic diversity (measured via Shannon diversity of industry employment types using NAICS codes) and potential to rebuild household finances. US data come from the Census API; Canadian data from Statistics Canada.

### Final score
- **`03_livelihoods_score_calculation.R`** — Reads the processed indicator rasters and assembles the final Livelihoods domain score (status, resistance, recovery, resilience) at 90 m resolution.

---

## QA / Validation scripts
- **`livelihoods_background_classify.R`** — Classifies NA types in output rasters for QA review.
- **`livelihoods_checks.R`** — Validates final layer extents, CRS alignment, and value ranges.

---

## Data sources
- US Census Bureau / American Community Survey (ACS) — income, housing burden, unemployment
- Statistics Canada — Census of Population, housing cost burden
- US Census API (NAICS industry employment for economic diversity)
- Statistics Canada — Labour Force Survey / Census industry tables

---

## Output
Final raster layers are written under **`{WRI_PROJECT_ROOT}/final_layers/<year>/livelihoods/`** (see the repository root [README](../README.md) for configuring `WRI_PROJECT_ROOT`).
