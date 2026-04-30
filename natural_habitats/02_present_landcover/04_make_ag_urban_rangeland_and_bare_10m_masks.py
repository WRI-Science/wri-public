#### Goal ####
# The goal of this script is to generate three masks per ecoregion raster—in Mollweide projection—for:
#   1. Agriculture & Urban (classes 5 & 7)
#   2. Rangeland (class 11)
#   3. Bare Ground (class 8)
# Each mask flags pixels with value=1 for the target class(es), and 0 elsewhere.
# Once these layers are made, the next step will be to aggregate them to 90m resolution
# and merge into a single raster layer for further processing.

#### Packages ####
import os
import glob
import gc
import numpy as np
import rasterio
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

#### File Paths and Setup ####
# Year to process (for naming outputs)
year = 2023

# Base directory for all natural ecosystems data
data_root = "/home/shares/wwri-wildfire/data/natural_habitats/"

# Directory containing ecoregion-masked rasters in Mollweide projection
ecoregion_rast_dir = os.path.join(
    data_root,
    "int", "esri_present_landcover", str(year),
    "ecoregion_masked_rasters_moll"
)

# Classification definitions and their output subfolder names
CLASS_MASKS = {
    "ag_urban": [5, 7],
    "rangeland": [11],
    "bare_ground": [8]
}

# Build output directories for each mask type
environment = "esri_present_landcover"
output_dirs = {}
for mask_name in CLASS_MASKS:
    out_dir = os.path.join(
        data_root,
        "int", environment, str(year),
        f"{mask_name}_moll_10m"
    )
    os.makedirs(out_dir, exist_ok=True)
    output_dirs[mask_name] = out_dir
    print(f"Output directory ready: {out_dir}")

# Define target CRS: Mollweide projection (global-area-preserving)
target_crs = (
    "+proj=moll +lon_0=0 +x_0=0 +y_0=0 "
    "+ellps=WGS84 +datum=WGS84 +units=m"
)

# Parallelization setting: maximum workers for ProcessPoolExecutor
max_workers = 8  # adjust based on available CPU cores

# Overwrite existing outputs if True
enforce_overwrite = False

gc.collect()

#### Functions ####
def process_tif(tif_path, force=enforce_overwrite):
    """
    Processes a single ecoregion raster:
    - Reads the input raster
    - For each mask in CLASS_MASKS, creates a binary mask array
    - Writes one output file per mask type into its respective folder
    """
    region_code = os.path.splitext(os.path.basename(tif_path))[0]
    with rasterio.open(tif_path) as src:
        data = src.read(1)
        profile = src.profile.copy()

    # Update profile for float32 and NaN nodata
    profile.update(
        dtype='float32',
        nodata=np.nan,
        compress='lzw'
    )

    # Generate and save each mask
    for mask_name, class_vals in CLASS_MASKS.items():
        out_dir = output_dirs[mask_name]
        out_path = os.path.join(out_dir, f"{region_code}.tif")
        # Skip if exists and not forcing
        if not force and os.path.exists(out_path):
            continue
        # Build mask: 1 for target classes, else 0
        mask = np.where(np.isin(data, class_vals), 1, 0)
        with rasterio.open(out_path, 'w', **profile) as dst:
            dst.write(mask.astype('float32'), 1)
    print(f"Processed {region_code}")
    gc.collect()

#### Main Processing ####
if __name__ == "__main__":
    # Gather all .tif files
    tif_files = glob.glob(os.path.join(ecoregion_rast_dir, "*.tif"))
    print(f"Found {len(tif_files)} TIFF files to process for year {year}.")

    # Parallel execution
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = [executor.submit(process_tif, path) for path in tif_files]
        for _ in tqdm(as_completed(futures), total=len(futures), desc="Processing TIFF files"):
            pass

    gc.collect()
