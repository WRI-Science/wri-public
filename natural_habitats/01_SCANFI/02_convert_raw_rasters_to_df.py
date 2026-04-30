#### Goal ####
# The goal of this script is to convert the biomass, closure, and species rasters 
# into individual dataframes for easier processing of species counts, density, and trait matrix prep.
# once they are saved as their own data frames they will need to be merged in the next step in R.

# Presently this takes about 1.5 hours to run.

#### Packages ####
import os
import rasterio
import pandas as pd
import numpy as np
import pyarrow.csv as pacsv
import pyarrow as pa
import gc  # Garbage collection
import geopandas as gpd
from rasterio.mask import mask
from shapely.geometry import mapping
from concurrent.futures import ProcessPoolExecutor
from affine import Affine

#### File Paths and Setup ####
# Base path for the raster files
scanfi_raw_path = "/home/shares/wwri-wildfire/data/natural_habitats/raw/scanfi/"
output_path = "/home/shares/wwri-wildfire/data/natural_habitats/int/scanfi/individual_csvs_to_join/"
study_area_shape_path = "/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp"

# List of raster file names and their corresponding column names
raster_files = {
    "biomass": "SCANFI_att_biomass_SW_2020_v1.2.tif",
    "closure": "SCANFI_att_closure_SW_2020_v1.2.tif",
    "balsam_fir": "SCANFI_sps_balsamFir_SW_2020_v1.2.tif",
    "black_spruce": "SCANFI_sps_blackSpruce_SW_2020_v1.2.tif",
    "douglas_fir": "SCANFI_sps_douglasFir_SW_2020_v1.2.tif",
    "jack_pine": "SCANFI_sps_jackPine_SW_2020_v1.2.tif",
    "lodge_pole": "SCANFI_sps_lodgepolePine_SW_2020_v1.2.tif",
    "ponderosa_pine": "SCANFI_sps_ponderosaPine_SW_2020_v1.2.tif",
    "tamarack": "SCANFI_sps_tamarack_SW_2020_v1.2.tif",
    "white_red_pine": "SCANFI_sps_whiteRedPine_SW_2020_v1.2.tif",
    "broadleaf_tree_prcB": "SCANFI_sps_prcB_SW_2020_v1.2.tif",
    "other_coniferous_prcC": "SCANFI_sps_prcC_other_SW_2020_v1.2.tif"

}

#### Functions ####
def round_transform(transform, decimals=8):
    """
    Round affine transform values to reduce floating point precision issues.
    """
    return Affine(*[round(v, decimals) for v in transform])


def check_alignment_consistency(raster_paths, decimals=8):
    """
    Check if all rasters in the list have the same rounded transform and CRS.
    If not, raise an error and stop execution.
    """
    reference_transform = None
    reference_crs = None
    reference_shape = None

    for i, path in enumerate(raster_paths):
        with rasterio.open(path) as src:
            transform = round_transform(src.transform, decimals)
            crs = src.crs
            shape = (src.height, src.width)

            if i == 0:
                reference_transform = transform
                reference_crs = crs
                reference_shape = shape
            else:
                if (transform != reference_transform or crs != reference_crs or shape != reference_shape):
                    raise ValueError(
                        f"\nAlignment inconsistency detected in file: {path}\n"
                        f"Expected transform: {reference_transform}, shape: {reference_shape}, CRS: {reference_crs}\n"
                        f"Found transform: {transform}, shape: {shape}, CRS: {crs}\n"
                        f"Ensure all raw rasters are aligned before proceeding."
                    )
    print("All rasters are properly aligned.")


def load_and_buffer_shapefile(shapefile_path, target_crs):
    """Loads a shapefile, reprojects it to the target CRS, and buffers by 1000 meters."""
    print("Reading and processing study area shapefile...")
    study_area = gpd.read_file(shapefile_path)

    if study_area.crs != target_crs:
        # this is commented out to prevent long print statement from crs conversion
        #print(f"Reprojecting shapefile from {study_area.crs} to {target_crs}...")
        study_area = study_area.to_crs(target_crs)

    study_area["geometry"] = study_area.geometry.buffer(1000)  # Buffer by 1000 meters
    return [mapping(geom) for geom in study_area.geometry]  # Convert to rasterio-friendly format

def mask_raster(raster_path, shapes):
    """Masks a raster using the provided shapefile geometry."""
    with rasterio.open(raster_path) as src:
        masked_raster, masked_transform = mask(src, shapes, crop=True)
        return masked_raster[0], masked_transform, src.crs, src.nodata  # Return masked array, transform, CRS, and nodata value

def process_raster_chunk(masked_raster, transform, var_name, row_start, row_end, nodata_value):
    """Processes a chunk of the masked raster and extracts valid data."""
    band_chunk = masked_raster[row_start:row_end, :]
    
    mask = band_chunk != nodata_value  # Ensure nodata handling
    row_idx, col_idx = np.where(mask)
    
    if len(row_idx) == 0:
        return pd.DataFrame()  # Return empty DataFrame if no valid data
    
    values = band_chunk[row_idx, col_idx]
    global_row_idx = row_idx + row_start  # Adjust row indices to global position
    x_coords, y_coords = rasterio.transform.xy(transform, global_row_idx, col_idx)

    return pd.DataFrame({"x": x_coords, "y": y_coords, var_name: values})

def raster_to_dataframe(raster_path, var_name, num_workers=4):
    """Processes a raster file in parallel (by row chunks) and returns a DataFrame."""
    print(f"Processing {var_name} from {raster_path}...")

    with rasterio.open(raster_path) as src:
        shapes = load_and_buffer_shapefile(study_area_shape_path, src.crs)  # Load shapefile in raster CRS
        masked_raster, masked_transform, raster_crs, nodata_value = mask_raster(raster_path, shapes)

    # Process in chunks
    rows_per_chunk = max(1, masked_raster.shape[0] // num_workers)
    tasks = []
    
    with ProcessPoolExecutor(max_workers=num_workers) as executor:
        for i in range(0, masked_raster.shape[0], rows_per_chunk):
            row_start = i
            row_end = min(i + rows_per_chunk, masked_raster.shape[0])
            tasks.append(executor.submit(process_raster_chunk, masked_raster, masked_transform, var_name, row_start, row_end, nodata_value))
        
        results = [task.result() for task in tasks]

    # Merge all chunks into a single DataFrame
    df = pd.concat([df for df in results if not df.empty], ignore_index=True)
    print(f"Finished processing {var_name}, extracted {len(df)} valid records.")
    
    # Explicitly clear memory
    del results, tasks, masked_raster
    gc.collect()
    
    return df

def save_to_csv_pyarrow(df, output_file):
    """Saves DataFrame to CSV using PyArrow efficiently."""
    print(f"Saving DataFrame to {output_file} using PyArrow...")
    table = pa.Table.from_pandas(df)
    pacsv.write_csv(table, output_file)
    print("Save completed.")

#### Main Processing ####
# 1. Build full paths list for the alignment check:
full_raster_paths = [
    os.path.join(scanfi_raw_path, fname)
    for fname in raster_files.values()
]
check_alignment_consistency(full_raster_paths)

# 2. Process each raster one at a time and save immediately
csv_files = []
for var_name, filename in raster_files.items():
    # use the correct base path variable here:
    raster_path = os.path.join(scanfi_raw_path, filename)
    df = raster_to_dataframe(raster_path, var_name)
    
    if not df.empty:
        csv_file = os.path.join(output_path, f"{var_name}.csv")
        save_to_csv_pyarrow(df, csv_file)
        csv_files.append(csv_file)

    # Clear memory after each raster
    del df
    gc.collect()

print("Processing complete.")

