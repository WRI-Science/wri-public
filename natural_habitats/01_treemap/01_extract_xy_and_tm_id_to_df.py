#### Goal ####
# The goal of the script is to make an xy csv dataframe of the raw raster data from treemap.
# This will let us move forward with the other calculations needed from this data including 
# the carbon domain and natural habitats domain
# When rerunning everything with its current settings and redo_all = True the script takes about 30 minutes

#### Packages ####
import rasterio
import geopandas as gpd
import pandas as pd
import numpy as np
from rasterio.mask import mask
from shapely.geometry import mapping
from dbfread import DBF
from rasterio.transform import xy
import time
import os

start_time = time.time()

#### Paths and Setup #### 
redo_all = True   # or True, if you want to force a full re‐run

multi_domain_data_path = "/home/shares/wwri-wildfire/data/multi_domain_data/"
raw_treemap_data_base = f"{multi_domain_data_path}raw/treemap/from_publication_zip/Data/"
tif_path    = f"{raw_treemap_data_base}TreeMap2016.tif"
dbf_path    = f"{raw_treemap_data_base}TreeMap2016.tif.vat.dbf"
shape_path  = "/home/shares/wwri-wildfire/data/multi_domain_data/int/boundary_layers/admin_boundary_layers/wwri_study_area_admin_0.shp"

# Save Paths
xy_value_int_path = f"{multi_domain_data_path}int/treemap/study_area_points.feather"
out_tif     = f"{multi_domain_data_path}int/treemap/study_area_treemap_2016.tif"
out_csv     = f"{multi_domain_data_path}int/treemap/study_area_treemap_2016_all_layers.csv"

#### Functions ####
def raster_to_points(arr, transform, nodata):
    # 1) find the row, col indices of all non‐nodata cells at once
    # keep pixels that are not the nodata code AND are finite
    valid = (arr != nodata) & np.isfinite(arr)
    rows, cols = np.where(valid)

    # 2) get the X/Y coords for each (row, col)
    xs, ys = xy(transform, rows, cols, offset="center")

    # 3) gather the pixel codes
    vals = arr[rows, cols].astype(int)

    # 4) build the DataFrame
    df = pd.DataFrame({
        "X": xs,
        "Y": ys,
        "Value": vals
    })
    return df

#### Main Processing ####
print("\n1) Reading raster metadata and CRS…")
with rasterio.open(tif_path) as src:
    raster_crs  = src.crs
    raster_meta = src.meta
print(f"    • CRS: {raster_crs}")
print("    • Metadata loaded.")

print("\n2) Reading study area shapefile…")
study_area = gpd.read_file(shape_path)
print(f"    • Original shapefile CRS: {study_area.crs}")
if study_area.crs != raster_crs:
    print("    • Reprojecting shapefile to match raster CRS…")
    study_area = study_area.to_crs(raster_crs)
    print(f"    • New shapefile CRS: {study_area.crs}")
else:
    print("    • CRS already matches; no reprojection needed.")
shapes = [mapping(geom) for geom in study_area.geometry]
print(f"    • Prepared {len(shapes)} geometry(ies) for masking.")

print("\n3) Masking and cropping raster (all_touched=True)…")
with rasterio.open(tif_path) as src:
    nodata = src.nodata
    masked_arr, masked_transform = mask(
        src,
        shapes,
        crop=True,
        all_touched=True
    )
print(f"    • Masking complete; array shape is {masked_arr.shape}")


print("\n4) Writing clipped GeoTIFF…")
meta = raster_meta.copy()
meta.update({
    "height":    masked_arr.shape[1],
    "width":     masked_arr.shape[2],
    "transform": masked_transform
})
with rasterio.open(out_tif, "w", **meta) as dst:
    dst.write(masked_arr)
print("    • Clipped GeoTIFF saved to:", out_tif)

print("\n5) Loading DBF attribute table…")
dbf_df = pd.DataFrame(iter(DBF(dbf_path)))
print("    • DBF columns:", dbf_df.columns.tolist())

# Step 6: skip or run conversion
if os.path.exists(xy_value_int_path) and not redo_all:
    print(f"     • Feather already exists at {xy_value_int_path} and redo_all=False → skipping conversion.")
    points_df = pd.read_feather(xy_value_int_path)
else:
    print("\n6) Converting masked raster to point DataFrame…")
    points_df = raster_to_points(
        masked_arr[0],
        masked_transform,
        nodata
)
print(f"    • Generated {len(points_df)} point records")    


print("\n7) Joining points to DBF attributes…")
unique_vals = points_df["Value"].unique()
dbf_small   = dbf_df[dbf_df["Value"].isin(unique_vals)]
print(f"    • Reduced DBF from {len(dbf_df)} to {len(dbf_small)} rows")

print("    • Mapping tm_id and CARBON_L via dict lookup…")
for col in ["tm_id", "CARBON_L"]:
    lookup = dbf_small.set_index("Value")[col].to_dict()
    points_df[col] = points_df["Value"].map(lookup)
print("    • Joined tm_id and CARBON_L to the xy df")

points_df.dropna(subset=["tm_id","CARBON_L"], how="any", inplace=True)

# clean up
del dbf_df, dbf_small, masked_arr

print("\n8) Writing final CSV in chunks…")
with open(out_csv, "w", newline="") as f:
    f.write(",".join(points_df.columns) + "\n")
    chunk_size = 2_000_000
    for start in range(0, len(points_df), chunk_size):
        end = start + chunk_size
        points_df.iloc[start:end].to_csv(f, index=False, header=False)
        print(f"    • Written rows {start}-{min(end, len(points_df))}")

print("    • CSV complete at", out_csv)

end_time = time.time()
elapsed = end_time - start_time
hours, rem = divmod(elapsed, 3600)
minutes, seconds = divmod(rem, 60)
print(f"\nScript complete! Total runtime: {int(hours)}h {int(minutes)}m {seconds:.2f}s")
