#### Goal ####
# The goal of this script is to generate the zonal stats of each ecoregion to calculate the percent of each that we consider natural.
# The script will process the ecoreiogn rasters from step 2. Once this step is complete the output will be used to calculate 
# the final status layer while using the historical extent data

#### Packages ####
import os
import gc
import pandas as pd
import numpy as np
import geopandas as gpd
import rasterio
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

#### File Paths and Setup ####
# Year to process (for naming outputs)
year = 2023

# Base directory for all natural habitats data
data_root = "/home/shares/wwri-wildfire/data/natural_habitats/"

# File paths
# Path to the ecoregions shapefile (intersecting study area)
multi_domain = "/home/shares/wwri-wildfire/data/multi_domain_data"
ecoregion_shapefile = os.path.join(
    multi_domain,
    "int/epa_ecoregions_north_america_level_iii",
    "intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp"
)
masked_rasters_dir = os.path.join(
    data_root, "int", "esri_present_landcover", str(year),
    "ecoregion_masked_rasters_moll/"
)
output_dir = os.path.join(
    data_root, "int", "esri_present_landcover", str(year),
    "percent_natural_calculation/"
)

# Define target CRS: Mollweide projection (global-area-preserving)
target_crs = (
    "+proj=moll +lon_0=0 +x_0=0 +y_0=0 "
    "+ellps=WGS84 +datum=WGS84 +units=m"
)

# Parallelization setting: maximum workers for ProcessPoolExecutor
max_workers = 30  # adjust this value based on your system's CPU cores

# Classification lookup table and filters
CLASSIFICATION_NAMES = {
    0: 'No Data 1', 1: 'Water', 2: 'Trees', 4: 'Flooded vegetation',
    5: 'Crops', 7: 'Built area', 8: 'Bare ground', 9: 'Snow/ice',
    10: 'Clouds', 11: 'Rangeland', 255: 'No Data 2'
}
excluded_classes = ['No Data 1', 'No Data 2', 'Water']
natural_classes = ['Trees', 'Flooded vegetation', 'Bare ground', 'Snow/ice', 'Rangeland']

# Ensure output directory exists
os.makedirs(output_dir, exist_ok=True)
print(f"Output directory ready: {output_dir}")


#### Functions ####

def make_ecoregion_shape(code: str, ecoregions_gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """
    Create a single-polygon GeoDataFrame for the given ecoregion code.
    """
    subset = ecoregions_gdf[ecoregions_gdf['NA_L3CODE'] == code]
    # Use union_all() on the geometry series to merge parts into one polygon
    merged_geom = subset.geometry.union_all()
    gdf = gpd.GeoDataFrame(
        {'Ecoregion Code': [code]},
        geometry=[merged_geom],
        crs=ecoregions_gdf.crs
    )
    print(f"-> Created geometry for ecoregion {code}")
    return gdf


def process_ecoregion_percents(code: str) -> pd.DataFrame:
    """
    Read the masked raster for a given ecoregion, compute counts,
    calculate percent-natural metrics, and return a small DataFrame.
    """
    raster_path = os.path.join(masked_rasters_dir, f"{code}.tif")
    print(f"-> Reading raster for ecoregion {code} from {raster_path}")

    with rasterio.open(raster_path) as src:
        data = src.read(1)

    unique, counts = np.unique(data, return_counts=True)
    counts_map = dict(zip(unique, counts))

    # Build a DataFrame of counts per classification
    records = []
    for val, name in CLASSIFICATION_NAMES.items():
        cnt = counts_map.get(val, 0)
        records.append({
            'Ecoregion Code': code,
            'Classification Number': val,
            'Classification': name,
            'Count': cnt
        })
    df = pd.DataFrame(records)
    df.sort_values('Classification Number', inplace=True)

    # Calculate percentages, excluding no-data and water
    total = df.loc[~df['Classification'].isin(excluded_classes), 'Count'].sum()
    df['Percentage'] = df.apply(
        lambda row: (row['Count'] / total * 100)
        if row['Classification'] not in excluded_classes else 0,
        axis=1
    )

    # Summarize natural classes
    pct_nat = df.loc[df['Classification'].isin(natural_classes), 'Percentage'].sum()
    norm_pct = pct_nat / 100
    print(f"-> Calculated Percent Natural for {code}: {pct_nat:.2f}% (normalized {norm_pct:.2f})")

    # Return the summary metrics
    return pd.DataFrame([{
        'Ecoregion Code': code,
        'Percent Natural': pct_nat,
        'Normalized Percent Natural': norm_pct
    }])


def process_single_ecoregion(code: str, ecoregions_gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """
    Wrapper to create geometry, compute metrics, and merge into a GeoDataFrame.
    """
    print(f"Processing ecoregion {code}")
    shape_gdf = make_ecoregion_shape(code, ecoregions_gdf)
    percent_df = process_ecoregion_percents(code)

    # Merge metrics with geometry
    merged = shape_gdf.merge(
        percent_df,
        on='Ecoregion Code'
    )
    merged = gpd.GeoDataFrame(
        merged,
        geometry='geometry',
        crs=target_crs
    )
    print(f"Finished ecoregion {code}")
    return merged

#### Main Processing ####
# 1. Load raw ecoregion polygons
print("Loading raw ecoregion shapefile...")
ecoregions_gdf = gpd.read_file(ecoregion_shapefile)

# 2. Reproject to Mollweide (target_crs)
ecoregions_gdf = ecoregions_gdf.to_crs(target_crs)
print(f"Reprojected ecoregions to target CRS: {target_crs}")

# 3. Get list of unique ecoregion codes
codes = ecoregions_gdf['NA_L3CODE'].drop_duplicates().tolist()
print(f"Found {len(codes)} unique ecoregion codes to process.")

# 4. Parallel processing of each ecoregion
results = []
print(f"Starting parallel processing with max_workers={max_workers}...")
with ProcessPoolExecutor(max_workers=max_workers) as executor:
    # Submit all tasks
    futures = {
        executor.submit(process_single_ecoregion, code, ecoregions_gdf): code
        for code in codes
    }
    for future in tqdm(
        as_completed(futures),
        total=len(futures),
        desc="Processing ecoregions"
    ):
        code = futures[future]
        try:
            result = future.result()
            if result is not None:
                results.append(result)
        except Exception as e:
            print(f"Error with ecoregion {code}: {e}")
        gc.collect()

# 5. Combine all results into a single GeoDataFrame
print("Combining results into final GeoDataFrame...")
final_gdf = pd.concat(results, ignore_index=True)
final_gdf = gpd.GeoDataFrame(
    final_gdf,
    geometry='geometry',
    crs=target_crs
)

# 6. Add CRS column
final_gdf['geometry_crs'] = target_crs

# 7. Save to CSV
output_csv = os.path.join(output_dir, f"ecoregion_natural_extent_pct_{year}.csv")
print(f"Saving final GeoDataFrame to CSV: {output_csv}")
final_gdf.to_csv(output_csv, index=False)
print("Script completed successfully.")
