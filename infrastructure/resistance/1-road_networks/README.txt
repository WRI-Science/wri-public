# README: Road Network Data Processing Workflows

## Overview

This repository contains **four sequential notebooks** designed to process and analyze road network data for U.S. Census-designated places and Canadian municipal polygons. The workflows cover data preparation, extraction of road networks using OSMnx, computation of network statistics, and generation of diagnostic outputs.

---

## Notebook Details

### 1. Canada Municipal Polygons Processing
- **Location:**  
  `/home/cbroderick/domains/built-environment/resistance/1-road-networks/1-canada-designated-places.ipynb`
- **Purpose:**  
  Downloads and processes Canadian Designated Places (DPLs) and Population Centers shapefiles from Statistics Canada. Overlapping regions are removed from DPLs to create a “Designated Places minus overlap” dataset. The script simplifies geometries and generates an interactive Folium map for visualization.
- **Data Source:**  
  [Statistics Canada Shapefiles](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21)

---

### 2. US Census Designated Places and Demographic Data Processing
- **Location:**  
  `/home/cbroderick/domains/built-environment/resistance/1-road-networks/2-us-designated-places.ipynb`
- **Purpose:**  
  Automates the downloading, extraction, and filtering of US Census-designated places shapefiles (from the 2023 TIGER/Line repository) and merges them with 2021 ACS 5-year demographic data using the Census API. The workflow filters places to those with a total population below 50,000 and saves the processed shapefiles and demographic CSV files.
- **Data Sources:**  
  - [2023 TIGER/Line Shapefiles for Designated Places](https://www2.census.gov/geo/tiger/TIGER2023/PLACE/)  
  - [US Census ACS 5-Year Data](https://api.census.gov/data/2021/acs/acs5)

---

### 3. Road Network Extraction for Designated Places
- **Location:**  
  `/home/cbroderick/domains/built-environment/resistance/1-road-networks/3-download-road-networks.ipynb`
- **Purpose:**  
  Extracts road networks for each designated place (both U.S. and Canadian) using OSMnx. The process involves buffering each place’s polygon, querying OpenStreetMap for driving networks, and saving the resulting road network graphs in GraphML format. This notebook employs parallel processing and detailed logging to efficiently handle large-scale data extraction.

---

### 4. U.S. and Canadian Census-Designated Places Road Network Analysis
- **Location:**  
  `/home/cbroderick/domains/built-environment/resistance/1-road-networks/4-road-network-calculation.ipynb`
- **Purpose:**  
  Associates each place’s geographic boundary with its corresponding pre-saved GraphML road network. It computes various network statistics such as graph density, average node degree, and counts of boundary-crossing roads (broken down by highway type). The results are aggregated into a CSV file, and diagnostic visualizations (e.g., histograms and network overlays) are generated. The workflow includes robust error handling and timeout mechanisms to ensure scalability and reliability.

---
