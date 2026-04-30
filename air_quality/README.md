# Air Quality Domain

## Overview

The Air Quality domain measures the degree to which communities in the western United States and Canada are exposed to poor air quality — particularly from wildfire smoke — and captures the vulnerability of populations who are most sensitive to that exposure.

Scores are computed at 90 m resolution across the full WRI study area (12 western US states plus British Columbia and Yukon) and reflect both the **status** of air quality conditions and the **resistance** of communities to air-quality-related harm.

---

## Indicators

### Status — Air quality exceedance days
- **`01_us_calculate_days_above_aqi.R`** — Reads 2024 EPA daily AQI monitoring data for western US sites, joins observations to site coordinates, and counts the number of days per site exceeding AQI thresholds of 100 and 300.
- **`01_canada_calculate_days_above_100_aqi.R`** — Performs the same exceedance-day calculation for Canadian monitoring stations.
- **`02_aqi_100_idw.R`** / **`02_aqi_300_idw.R`** — Spatially interpolates the point-based exceedance-day counts across the study area using inverse distance weighting (IDW), producing continuous raster surfaces at both AQI thresholds.

### Resistance — Vulnerable population exposure
- **`03_ncfh_us_farm_workers.R`** — Processes National Center for Farmworker Health (NCFH) data to estimate the spatial distribution of agricultural/H-2A workers, a population with high outdoor smoke exposure and limited access to protection.
- **`04_asthma_adults_prevalence.R`** / **`04_copd_adult_prevalence.R`** — Derives county- and census-tract-level prevalence of asthma and COPD (chronic obstructive pulmonary disease) for US communities, then rasterizes to 90 m.
- **`04_hospital_kde.R`** — Estimates healthcare access using kernel density estimation (KDE) of hospital locations, representing proximity to acute care as a protective factor.
- **`04_vulnerable_populations.R`** — Assembles the composite vulnerable population resistance indicator by combining age (65+), disability status, no-vehicle households, asthma/COPD prevalence, and farmworker density.
- **`04_vulnerable_workers_naics_farm_h2a.R`** — Additional processing of agricultural worker vulnerability using NAICS industry codes and H-2A visa worker data.

### Final score
- **`05_air_quality_score_calculation.R`** — Reads all processed indicator rasters and computes the final Air Quality domain score (status, resistance, resilience) aligned to the 90 m study area template.

---

## QA / Validation scripts
- **`air_background_classify.R`** — Classifies NA types in the output rasters (e.g., outside study area, no data, masked) for QA review.
- **`air_checks.R`** — Runs final layer checks to confirm raster validity, extent alignment, and expected value ranges.

---

## Data sources
- EPA AQI daily data (US)
- Environment and Climate Change Canada (ECCC) monitoring data
- US Census Bureau / American Community Survey (ACS)
- National Center for Farmworker Health (NCFH)
- Behavioral Risk Factor Surveillance System (BRFSS) — asthma/COPD prevalence

---

## Output
Final raster layers are written to `/home/shares/wwri-wildfire/final_layers/2024/air_quality/`.
