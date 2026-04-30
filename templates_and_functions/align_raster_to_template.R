align_raster_to_template <- function(template_raster, input_raster, input_type = c("categorical", "continuous")) {
  input_type <- match.arg(input_type)
  message("Starting raster alignment...")
  
  # Choose resampling method based on input type
  resample_method <- if (input_type == "categorical") "near" else "bilinear"
  
  # Check CRS
  message("Checking CRS...")
  if (!crs(template_raster) == crs(input_raster)) {
    warning("CRS mismatch: reprojecting raster to match template CRS.")
    input_raster <- project(input_raster, template_raster, method = resample_method)
  }
  
  # Check resolution
  message("Checking resolution...")
  if (!all(res(template_raster) == res(input_raster))) {
    warning("Resolution mismatch: resampling raster to match template resolution.")
    input_raster <- resample(input_raster, template_raster, method = resample_method)
  }
  
  # Check extent
  message("Checking extent...")
  if (!ext(template_raster) == ext(input_raster)) {
    warning("Extent mismatch: cropping and extending raster to match template extent.")
    input_raster <- crop(input_raster, ext(template_raster), snap = "out")
    input_raster <- extend(input_raster, template_raster)
  }
  
  # Check dimensions
  message("Checking dimensions...")
  if (!all(dim(template_raster) == dim(input_raster))) {
    warning("Dimension mismatch: resampling raster to match template.")
    input_raster <- resample(input_raster, template_raster, method = resample_method)
  }
  
  # Check origin
  message("Checking origin...")
  if (!all(origin(template_raster) == origin(input_raster))) {
    warning("Origin mismatch: resampling raster to match template grid.")
    input_raster <- resample(input_raster, template_raster, method = resample_method)
  }
  
  # Final checks
  message("Validating final alignment...")
  stopifnot(
    crs(input_raster) == crs(template_raster),
    all(res(input_raster) == res(template_raster)),
    ext(input_raster) == ext(template_raster),
    all(dim(input_raster) == dim(template_raster)),
    all(origin(input_raster) == origin(template_raster))
  )
  
  # Value range check (applies to both types)
  message("Checking value range (0 to 1)...")
  vals <- values(input_raster, mat = FALSE)
  if (any(!is.na(vals) & (vals < 0 | vals > 1))) {
    stop("Raster values are out of expected [0, 1] range.")
  }
  
  # Mask to template
  message("Masking input raster to template...")
  input_raster <- mask(input_raster, template_raster)
  
  message("Raster alignment complete.")
  return(input_raster)
}
