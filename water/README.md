# Water Domain

## Overview

The Water domain assesses the resilience of water resources and water management systems to wildfire disturbance. Wildfires degrade watersheds, spike sediment loads, and threaten drinking water infrastructure — this domain captures both the hydrological condition of western streams and the institutional preparedness of water managers.

Scores are computed at 90 m resolution across the WRI study area and are primarily structured around **resistance** indicators (capacity to maintain water supply and quality) with no separate recovery sub-score in the 2024 snapshot.

---

## Structure

```
water/
├── surface-water/           # Streamflow timing and quantity
├── nwis/                    # USGS stream gauge data retrieval
├── drought-plans/           # State/provincial drought planning scores
├── epa-water-treatment/     # EPA drinking water system violations
└── calculate_domain_score.R # Final domain score assembly
```

---

## Indicators

### Surface water quantity and timing — `surface-water/`
Assesses inter-annual variability in streamflow using long-term USGS NWIS gauge records. Sites with more stable, predictable flow regimes score higher — they are better positioned to maintain water supply through wildfire-related watershed disruption.

| Script | Description |
|--------|-------------|
| `01-get-us-sites-of-interest.R` | Filters USGS NWIS stream gauge sites to those in the study area with sufficient data coverage (1991–2020, ≥20 years with 12 months of data) |
| `02-get-canada-sites-of-interest.R` | Performs equivalent site filtering for Canadian hydrometric stations |
| `03-canada-and-us-site-assigning-to-hydrobasins.R` | Spatially assigns US and Canadian gauge sites to HydroBASINS Level 8 watersheds for regional aggregation |
| `04-rescaling-plots-30-yr.R` | Generates diagnostic plots of 30-year flow statistics to inform rescaling decisions |
| `05-rescaling.R` | Rescales flow variability metrics to 0–1 resistance indicators by hydrobasin |

### Stream gauge data retrieval — `nwis/`
Handles bulk data download from the USGS National Water Information System (NWIS) API.

| Script | Description |
|--------|-------------|
| `01-usgs-nwis-site-by-state-retrieval.R` | Sequential retrieval of annual summary statistics by state |
| `01-usgs-nwis-site-by-state-retrieval-parallel.R` | Parallelized version for faster retrieval across many states |
| `usgs-nwis-data-exploration.R` | Exploratory analysis of retrieved gauge records; informs site filtering thresholds |

### Drought planning — `drought-plans/`
Scores states and provinces on the quality and adoption of formal drought preparedness plans — an institutional indicator of water system resilience.

- **`01_calc_drought_plan_indicator.R`** — Reads drought plan scores for US states and Canadian provinces, rescales to 0–1, and rasterizes to 90 m using state/province boundary polygons.

### Drinking water system quality — `epa-water-treatment/`
Uses EPA Safe Drinking Water Information System (SDWIS) violation records to score community water system reliability.

- **`01_calc_water_treatment_indicator.R`** — Reads EPA SDWIS data, computes a composite score combining number of water sources (community water systems) and total violations per county, rescales, and rasterizes to 90 m.
- **`explore-file-diffs.R`** — Exploratory script comparing SDWIS file versions across years to understand data changes.

---

## Final score
- **`calculate_domain_score.R`** — Reads the processed drought plan and water treatment indicator rasters, takes their mean to form a resistance score, computes resilience (= resistance for this domain), and aligns all outputs to the 90 m study area template.

---

## Data sources
- USGS NWIS — streamflow annual statistics (US)
- Environment and Climate Change Canada — hydrometric station data (Canada)
- HydroBASINS — watershed boundary polygons (Level 8)
- Western States Water Council / state agencies — drought plan scoring
- EPA Safe Drinking Water Information System (SDWIS) — water system violation records

---

## Output
Final raster layers are written to `/home/shares/wwri-wildfire/final_layers/2024/water/`.
