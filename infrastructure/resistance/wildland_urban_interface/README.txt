# Wilderness Urban Interface

This text is repeated in the wilderness-urban-interface.ipynb

==========

Step 1: Imports and Setup
-------------------------
- The code imports standard libraries (e.g., os, sys, subprocess, logging) and specialized libraries for geospatial data (geopandas, rasterio, shapely, tqdm, pandas, matplotlib).
- It also sets up XML parsing and parallel processing using ProcessPoolExecutor.

Step 2: Configuration Variables
-------------------------------
- Key parameters such as the number of parallel workers, output directory names (for masked rasters, VRT, and plot), and logging settings are defined.
- The new VRT file name and plot output details are specified.

Step 3: Logger Initialization
-----------------------------
- Logging is configured to capture errors to both a log file (masking_errors.log) and the console.
- A standard formatter is applied to log messages.

Step 4: VRT File Validation and Parsing
-----------------------------------------
- The code checks that the specified VRT file exists.
- It then parses the VRT file using XML to extract a list of raster file paths, converting any relative paths to absolute ones.

Step 5: Loading the Boundary Shapefile
----------------------------------------
- The study area boundary shapefile is validated (including its necessary auxiliary files).
- GeoPandas is used to load the shapefile, representing the study area.

Step 6: Reprojecting the Study Area
-----------------------------------
- The CRS (Coordinate Reference System) is determined from the first valid raster.
- The study area boundary is reprojected to match this CRS to ensure spatial compatibility.

Step 7: Raster Classification
-----------------------------
- Each raster file’s bounding box is created and evaluated.
- Rasters are classified as “fully inside” (bounding box completely within the study area) or “edge” (bounding box intersects but is not completely within the study area).

Step 8: Masking Edge Rasters in Parallel
----------------------------------------
- For rasters that only partially intersect the study area (“edge rasters”), a mask is applied to set cells outside the boundary to a specified nodata value.
- Parallel processing (using ProcessPoolExecutor) is used to perform masking on multiple rasters simultaneously.
- Masked rasters are saved in a designated output directory.

Step 9: Preparing Rasters for VRT Creation
------------------------------------------
- The lists of “fully inside” and newly masked “edge” rasters are combined.
- The code checks that there is at least one raster available to include in the new VRT.

Step 10: Creating a New VRT File
--------------------------------
- Paths of the selected rasters are written to a temporary file.
- The GDAL utility ‘gdalbuildvrt’ is executed via a subprocess to create a new VRT file referencing these rasters.
- The temporary file is removed after successful creation of the VRT.

Step 11: Plotting Raster Bounding Boxes
---------------------------------------
- GeoDataFrames are created for both “fully inside” and “edge” rasters based on their bounding boxes.
- The study area boundary and raster bounding boxes are plotted using Matplotlib.
- The plot is saved to a specified file location.

Step 12: Process Completion Notification
------------------------------------------
- Completion messages are printed to the console.
- The user is informed that any errors encountered have been logged in the log file (masking_errors.log).

==========================================