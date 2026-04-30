import os

WRI_PROJECT_ROOT = os.environ.get("WRI_PROJECT_ROOT", "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to take the landcover tiles saved and reprojected then merge and mask them to each ecoregion so each ecoregion can be processed individually and then spatial stats can be calculated after.

#### Packages ####
# Core and system libraries
import os            # file path operations
import time          # timing and sleep
import gc            # garbage collection
import numpy as np   # numeric operations
import pandas as pd  # data manipulation (if needed)

# Geospatial libraries
import geopandas as gpd                              # vector data handling
from shapely.geometry import mapping, shape, box     # geometry creation and conversion
from shapely.ops import unary_union, transform       # merging and coordinate transforms
from pyproj import Transformer, CRS                  # CRS transformations

# Raster I/O and processing
import rasterio                                        # raster file I/O
from rasterio.merge import merge                       # merge multiple rasters
from rasterio.mask import mask                         # crop rasters by geometry
from rasterio.io import MemoryFile                    # in-memory raster

# Concurrency for faster cropping
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm                                  # progress bars

# STAC client for querying Planetary Computer
import pystac_client
import planetary_computer

#### File Paths and Setup ####
# Year to process
year = 2023

# Base directory for all natural habitats data
data_root = os.path.join(WRI_PROJECT_ROOT, "data", "natural_habitats")

# Directory containing raw, reprojected STAC tiles for this year
reprojected_dir = os.path.join(
    data_root, "int", "esri_present_landcover", str(year), "reprojected_raw_tiles"
)

# Path to the ecoregions shapefile (intersecting study area)
multi_domain = os.path.join(WRI_PROJECT_ROOT, "data", "multi_domain_data")
ecoregion_shapefile = os.path.join(
    multi_domain,
    "int/epa_ecoregions_north_america_level_iii",
    "intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp"
)

# Output directory for final, cropped & merged rasters in Mollweide
output_dir = os.path.join(
    data_root, "int", "esri_present_landcover", str(year),
    "ecoregion_masked_rasters_moll"
)
# Ensure it exists
os.makedirs(output_dir, exist_ok=True)

# Define target CRS: Mollweide projection (global-area-preserving)
target_crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

#### Functions ####

def query_stac_items(bbox_geojson, year, collection="io-lulc-annual-v02",
                     max_retries=4, base_delay=5):
    """
    Query the Planetary Computer STAC API for LULC tiles within the given bounding box and year.
    - Splits bounding box into quarters on "Entity is too large" errors.
    - Retries on Timeout with exponential backoff.
    Returns a list of STAC items whose start_datetime matches the year.
    """
    catalog = pystac_client.Client.open(
        "https://planetarycomputer.microsoft.com/api/stac/v1",
        modifier=planetary_computer.sign_inplace,
    )
    attempt = 0

    while attempt < max_retries:
        try:
            search = catalog.search(
                collections=[collection],
                intersects=bbox_geojson,
                datetime=f"{year}-01-01/{year}-12-31"
            )
            items = list(search.items())
            filtered = [it for it in items
                        if it.properties.get('start_datetime', '').startswith(str(year))]
            print(f"STAC query returned {len(filtered)} items for {year}.")
            return filtered

        except Exception as e:
            msg = str(e)
            if "Entity is too large" in msg:
                print("Entity too large; splitting bounding box into quarters and retrying...")
                geom = shape(bbox_geojson)
                minx, miny, maxx, maxy = geom.bounds
                midx, midy = (minx + maxx) / 2, (miny + maxy) / 2
                quarters = [
                    box(minx, miny, midx, midy),
                    box(midx, miny, maxx, midy),
                    box(minx, midy, midx, maxy),
                    box(midx, midy, maxx, maxy)
                ]
                results = []
                for q in quarters:
                    results.extend(query_stac_items(mapping(q), year, collection,
                                                   max_retries, base_delay))
                return results

            elif "Timeout" in msg or isinstance(e, TimeoutError):
                attempt += 1
                wait = base_delay * (2 ** (attempt - 1))
                print(f"Timeout (attempt {attempt}/{max_retries}), retrying in {wait}s...")
                time.sleep(wait)

            else:
                print(f"Error during STAC query: {e}")
                raise


def reproject_geojson(aoi_geojson, target_crs):
    """
    Reproject an input GeoJSON geometry to the specified target CRS.
    Defaults from EPSG:4326 if no original CRS is set.
    """
    geom = shape(aoi_geojson)
    input_crs = aoi_geojson.get('crs', {}).get('properties', {}).get('name', 'EPSG:4326')
    if input_crs == target_crs:
        return aoi_geojson

    transformer = Transformer.from_crs(CRS(input_crs), CRS(target_crs), always_xy=True)
    reproj_geom = transform(transformer.transform, geom)
    out_geojson = mapping(reproj_geom)
    out_geojson['crs'] = {'type': 'name', 'properties': {'name': target_crs}}
    return out_geojson


def crop_raster(raster_path, aoi_mask_geojson):
    """
    Crop a single raster to the given AOI mask. Returns (data, transform, crs).
    """
    with rasterio.open(raster_path) as src:
        data, transform = mask(src, [aoi_mask_geojson], crop=True)
        return data, transform, src.crs


def process_crop_save_rasters(stac_items, aoi_geojson, raster_dir,
                              ecoregion_code, output_dir):
    """
    For a given ecoregion:
      1. Reproject AOI to target CRS
      2. Find existing reprojected STAC tiles
      3. Crop each tile (in parallel)
      4. Merge cropped tiles if more than one
      5. Save the final raster to disk
    """
    print(f"Processing ecoregion {ecoregion_code}...")
    aoi_mask = reproject_geojson(aoi_geojson, target_crs)
    raster_paths = [
        os.path.join(raster_dir, f"moll_{item.id}.tif")
        for item in stac_items
        if os.path.exists(os.path.join(raster_dir, f"moll_{item.id}.tif"))
    ]
    print(f"Found {len(raster_paths)} raster(s) for cropping.")

    crop_results = []
    with ThreadPoolExecutor(max_workers=6) as executor:
        future_to_path = {executor.submit(crop_raster, p, aoi_mask): p for p in raster_paths}
        for future in as_completed(future_to_path):
            try:
                crop_results.append(future.result())
            except Exception as e:
                print(f"Error cropping {future_to_path[future]}: {e}")

    if not crop_results:
        print("No valid rasters to merge. Skipping.")
        return None

    if len(crop_results) > 1:
        print("Merging cropped rasters...")
        sources = []
        for data, tf, crs in crop_results:
            memfile = MemoryFile()
            mem = memfile.open(
                driver='GTiff', count=data.shape[0], height=data.shape[1],
                width=data.shape[2], transform=tf, crs=crs, dtype=data.dtype
            )
            mem.write(data)
            sources.append(mem)
        merged, merged_tf = merge(sources)
        for src in sources:
            src.close()
    else:
        print("Single raster—no merge needed.")
        merged, merged_tf = crop_results[0][0], crop_results[0][1]
        crs = crop_results[0][2]

    out_path = os.path.join(output_dir, f"{ecoregion_code}.tif")
    with rasterio.open(
        out_path, 'w', driver='GTiff', height=merged.shape[1],
        width=merged.shape[2], count=merged.shape[0], dtype=merged.dtype,
        crs=crs, transform=merged_tf
    ) as dst:
        for i in range(merged.shape[0]):
            dst.write(merged[i], i + 1)
    print(f"Saved masked raster: {out_path}")
    return out_path

#### Main Processing ####
print("Loading ecoregion shapefile...")
ec_data = gpd.read_file(ecoregion_shapefile)

# Ensure WGS84 for STAC queries
dest_crs = 'EPSG:4326'
if ec_data.crs != dest_crs:
    print(f"Reprojecting ecoregions to {dest_crs} for queries...")
    ec_data = ec_data.to_crs(dest_crs)

codes = ec_data['NA_L3CODE'].unique()
chunks = np.array_split(codes, 10)

for idx, chunk in enumerate(chunks, start=1):
    print(f"=== Processing batch {idx} of {len(chunks)} ===")
    for code in tqdm(chunk, desc=f"Batch {idx}"):
        target_file = os.path.join(output_dir, f"{code}.tif")
        if os.path.exists(target_file):
            continue

        region = ec_data[ec_data['NA_L3CODE'] == code]
        merged_geom = unary_union(region.geometry)
        aoi_geojson = mapping(merged_geom)
        bbox_geojson = mapping(box(*region.geometry.total_bounds))

        items = query_stac_items(bbox_geojson, year)
        process_crop_save_rasters(items, aoi_geojson, reprojected_dir, code, output_dir)
    print(f"=== Completed batch {idx} ===")
