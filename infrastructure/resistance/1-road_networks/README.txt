README: Road Network Data Processing Workflows
==============================================

Overview
--------
This folder contains four sequential Jupyter notebooks that process road network
data for U.S. Census-designated places and Canadian municipal polygons: data
preparation, OSMnx-based network extraction, computation of network statistics,
and diagnostic outputs.

Paths below are relative to this directory (`infrastructure/resistance/1-road_networks/`).


Notebook details
----------------

1) Canada municipal polygons — `1-canada-designated-places.ipynb`
   Downloads and processes Canadian Designated Places (DPLs) and Population
   Centers from Statistics Canada; removes overlaps; simplifies geometries;
   optional Folium maps.
   Data: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21

2) U.S. designated places + ACS — `2-us-designated-places.ipynb`
   Downloads 2023 TIGER/Line place boundaries, merges 2021 ACS 5-year
   demographics (Census API), filters to population < 50k.
   Data: https://www2.census.gov/geo/tiger/TIGER2023/PLACE/
         https://api.census.gov/data/2021/acs/acs5

3) Road network extraction — `3-download-road-networks.ipynb`
   For each place polygon, buffers and downloads driving networks with OSMnx;
   saves GraphML for downstream steps.

4) Network metrics — `4-road-network-calculation.ipynb`
   Loads GraphML per place; computes density, degree, boundary-crossing counts
   by highway type; aggregates CSV + diagnostic plots.
