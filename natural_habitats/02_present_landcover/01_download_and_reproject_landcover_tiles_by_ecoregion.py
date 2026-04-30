import os

WRI_PROJECT_ROOT = os.environ.get("WRI_PROJECT_ROOT", "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to download all of the landcover tiles that 
# intersect with our study area ecoregions. Once downloaded the tiles will be 
# reprojected to the mollweid projection and saved as the tiles. The next step 
# will make ecoregion specific landcover data from the reprojected rasters.

#### packages ####
import os  # File and directory operations
import time  # Sleep for retry delays
import gc  # Manual garbage collection
import logging  # Logging errors and info
from concurrent.futures import ProcessPoolExecutor  # Parallel processing

import geopandas as gpd  # Vector data operations
from shapely.geometry import mapping, shape, box  # Geometry conversion and creation

import requests  # HTTP requests
from requests.exceptions import Timeout  # Download timeout exception

import rasterio  # Raster I/O
from rasterio.warp import calculate_default_transform, reproject, Resampling  # Reprojection tools

import pystac_client  # STAC API client
import planetary_computer  # Signing Planetary Computer URLs
from tqdm import tqdm   # progress bars

#### file paths and setup ####
# year of interest
year = 2023 #2023 is the most recent data as of 5/15/2025

# base directories
base_dir = os.path.join(WRI_PROJECT_ROOT, "data", "natural_habitats")
raw_dir = os.path.join(base_dir, "raw/esri_present_landcover", str(year))
reprojected_dir = os.path.join(base_dir, "int/esri_present_landcover", str(year), "reprojected_raw_tiles")

# ecoregion intersection shapefile
multi_domain_data_path = os.path.join(WRI_PROJECT_ROOT, "data", "multi_domain_data")
ecoregion_shapefile = os.path.join(
    multi_domain_data_path,
    "int/epa_ecoregions_north_america_level_iii/intersecting_ecoregion_shapes/ecoregions_intersecting_study_area.shp"
)

# target CRS: Mollweide projection
target_crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

# parallel processing cores
# Default number of workers if not overridden
default_workers = 4
# Override for this run; adjust as needed
num_workers = 12

# ensure output dirs exist
for d in [raw_dir, reprojected_dir]:
    os.makedirs(d, exist_ok=True)

#### logging setup ####
log_file = os.path.join(base_dir, "raw/esri_present_landcover", "ecoregion_processing.log")
logging.basicConfig(
    filename=log_file,
    level=logging.INFO,
    format="%(asctime)s %(levelname)s:%(message)s"
)


#### functions ####

def load_ecoregions(shapefile):
    """
    Read the ecoregion shapefile and ensure it's in EPSG:4326. This is needed to query the stac api
    """
    gdf = gpd.read_file(shapefile)
    if gdf.crs != "EPSG:4326":
        gdf = gdf.to_crs("EPSG:4326")
    return gdf


def combine_geometry(df, code):
    """
    Filter the GeoDataFrame by NA_L3CODE and combine all polygons into one.
    Returns a GeoJSON-like mapping of the combined geometry.
    """
    subset = df[df['NA_L3CODE'] == code]
    geom = subset.geometry.union_all()
    return mapping(geom)


def query_stac_items(aoi_geojson, year, collection="io-lulc-annual-v02", retries=4):
    """
    Query the Planetary Computer STAC API for LULC items intersecting the AOI.
    If the request fails due to size, split the bbox into quarters and retry.
    """
    catalog = pystac_client.Client.open(
        "https://planetarycomputer.microsoft.com/api/stac/v1",
        modifier=planetary_computer.sign_inplace
    )

    def _search(bbox_geojson, attempt=1):
        try:
            items = list(
                catalog.search(
                    collections=[collection],
                    intersects=bbox_geojson,
                    datetime=f"{year}-01-01/{year}-12-31"
                ).items()
            )
            return [it for it in items if it.properties.get('start_datetime','').startswith(str(year))]
        except Exception as e:
            msg = str(e)
            if "Entity is too large" in msg and attempt < retries:
                geom = shape(bbox_geojson)
                minx, miny, maxx, maxy = geom.bounds
                midx, midy = (minx+maxx)/2, (miny+maxy)/2
                quarters = [
                    box(minx,miny,midx,midy), box(midx,miny,maxx,midy),
                    box(minx,midy,midx,maxy), box(midx,midy,maxx,maxy)
                ]
                results = []
                for q in quarters:
                    results.extend(_search(mapping(q), attempt+1))
                return results
            elif "Timeout" in msg and attempt < retries:
                time.sleep(1)
                return _search(bbox_geojson, attempt+1)
            else:
                raise

    initial_bbox = mapping(box(*shape(aoi_geojson).bounds))
    return _search(initial_bbox)


def download_rasters(items, dest_dir):
    """
    Download TIFF assets from STAC items into dest_dir.
    Skips items without a 'data' asset or if file already exists.
    Logs errors for failed downloads.
    """
    downloaded = []
    for it in items:
        if 'data' not in it.assets:
            logging.warning(f"No data asset for {it.id}")
            continue
        fname = f"{it.id}.tif"
        outpath = os.path.join(dest_dir, fname)
        if os.path.exists(outpath):
            downloaded.append(outpath)
            continue
        url = planetary_computer.sign(it.assets['data'].href)
        try:
            resp = requests.get(url, timeout=30)
            resp.raise_for_status()
            with open(outpath, 'wb') as f:
                f.write(resp.content)
            downloaded.append(outpath)
        except Exception as e:
            logging.error(f"Error downloading {it.id}: {e}")
    return downloaded


def reproject_raster(input_path, output_dir, crs=target_crs, force=False):
    """
    Reproject a single raster to the target CRS and save to output_dir.
    Skips if output exists and force=False. Returns output path.
    """
    base = os.path.splitext(os.path.basename(input_path))[0]
    out_fp = os.path.join(output_dir, f"moll_{base}.tif")
    if os.path.exists(out_fp) and not force:
        return out_fp
    with rasterio.open(input_path) as src:
        transform, w, h = calculate_default_transform(src.crs, crs, src.width, src.height, *src.bounds)
        meta = src.meta.copy()
        meta.update({"crs": crs, "transform": transform, "width": w, "height": h})
        with rasterio.open(out_fp, 'w', **meta) as dst:
            for b in range(1, src.count+1):
                reproject(
                    source=rasterio.band(src, b),
                    destination=rasterio.band(dst, b),
                    src_transform=src.transform,
                    src_crs=src.crs,
                    dst_transform=transform,
                    dst_crs=crs,
                    resampling=Resampling.nearest
                )
    return out_fp

#### main workflow ####
if __name__ == "__main__":
    # Load and prepare ecoregions
    regions = load_ecoregions(ecoregion_shapefile)
    codes = regions['NA_L3CODE'].unique()
    missed = []  # Track codes that fail initial run
    summary = {}

    # Parallel processing using configurable cores
    args = [(code, regions[regions['NA_L3CODE']==code]) for code in codes]
    with ProcessPoolExecutor(max_workers=num_workers) as executor:
        for code, group in tqdm(args, desc="ecoregions"):
            try:
                aoi = combine_geometry(group, code)
                items = query_stac_items(aoi, year)
                raws = download_rasters(items, raw_dir)
                # Reproject rasters in parallel
                futures = [executor.submit(reproject_raster, p, reprojected_dir) for p in raws]
                molls = [f.result() for f in futures]
                summary[code] = {'downloaded': len(raws), 'reprojected': len(molls)}
                logging.info(f"{code}: {len(raws)} downloaded, {len(molls)} reprojected")
            except Exception as e:
                logging.error(f"{code}: {e}")
                missed.append(code)
            finally:
                gc.collect()

    # Reprocess any missed codes sequentially
    if missed:
        logging.info(f"Reprocessing missed codes: {missed}")
        for code in missed:
            try:
                group = regions[regions['NA_L3CODE']==code]
                aoi = combine_geometry(group, code)
                items = query_stac_items(aoi, year)
                raws = download_rasters(items, raw_dir)
                for p in raws:
                    reproject_raster(p, reprojected_dir, force=True)
                logging.info(f"{code} reprocessed successfully")
            except Exception as e:
                logging.error(f"{code} reprocess failed: {e}")

    # Print a summary of results
    for c, stats in summary.items():
        print(f"{c}: {stats['downloaded']} downloaded, {stats['reprojected']} reprojected")
