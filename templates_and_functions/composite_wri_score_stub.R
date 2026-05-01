# =============================================================================
# 🧭 Composite WRI score (stub) ===============================================
# =============================================================================
# The published full-index **composite** is the unweighted arithmetic mean of
# eight domain-level score rasters (each on the shared 90 m study-area grid).
# This repo publishes domain pipelines separately; run those first, then adapt
# the paths below if your filenames differ.
#
# Sense of Place combines **iconic places** and **iconic species** sub-domains:
# we average those two domain-score rasters, then average with the other seven.

# =============================================================================
# ⚙️ Settings =================================================================
# =============================================================================
LAYER_YEAR <- "2024"
OUTPUT_COMPOSITE_SUBDIR <- "composite"
WRITE_OUTPUT <- FALSE

# =============================================================================
# 📁 Paths ====================================================================
# =============================================================================
wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

final_dir <- function(...) {
  file.path(wri_project_root, "final_layers", LAYER_YEAR, ...)
}

# Relative paths under final_layers/<year>/ — edit if your outputs use other names.
DOMAIN_SINGLE_FILES <- list(
  air_quality      = final_dir("air_quality", "air_quality_domain_score.tif"),
  communities      = final_dir("communities", "communities_domain_score_masked.tif"),
  infrastructure   = final_dir("infrastructure", "infrastructure_domain_score.tif"),
  livelihoods      = final_dir("livelihoods", "livelihoods_domain_score.tif"),
  natural_habitats   = final_dir("natural_habitats", "natural_habitats_domain_score_masked.tif"),
  water              = final_dir("water", "water_domain_score_mean.tif"),
  species            = final_dir("biodiversity", "biodiversity_domain_score.tif")
)

SENSE_OF_PLACE_CANDIDATES <- list(
  final_dir(
    "sense_of_place", "iconic_places",
    "sense_of_place_iconic_places_domain_score_mean.tif"
  ),
  final_dir(
    "sense_of_place", "iconic_species",
    "sense_of_place_iconic_species_domain_score_mean.tif"
  )
)

log_pt <- function(...) {
  stamp <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
  message(stamp, " ", ...)
}

# =============================================================================
# 🔢 Composite ================================================================
# =============================================================================
library(terra)

sense_of_place_layer <- function(paths = SENSE_OF_PLACE_CANDIDATES) {
  existing <- paths[file.exists(paths)]
  if (length(existing) < length(paths)) {
    alt <- gsub("_mean\\.tif$", ".tif", paths)
    pick <- alt[file.exists(alt)]
    if (length(pick) >= 2) {
      existing <- pick[1:2]
    }
  }
  if (length(existing) != 2) {
    stop(
      "Expected two Sense of Place score rasters (iconic places + iconic species).\n",
      "Tried:\n", paste(" -", paths, collapse = "\n"),
      call. = FALSE
    )
  }
  r1 <- rast(existing[[1]])
  r2 <- rast(existing[[2]])
  if (!terra::compareGeom(r1, r2, stopOnError = FALSE)) {
    stop("Sense of Place iconic places vs species rasters do not align.", call. = FALSE)
  }
  mean(c(r1, r2))
}

mean_composite <- function() {
  missing <- DOMAIN_SINGLE_FILES[!vapply(DOMAIN_SINGLE_FILES, file.exists, logical(1))]
  if (length(missing)) {
    stop(
      "Missing domain score file(s). Generate domain outputs first:\n",
      paste(" -", unlist(missing), collapse = "\n"),
      call. = FALSE
    )
  }

  sop <- sense_of_place_layer()
  rasters <- c(lapply(DOMAIN_SINGLE_FILES, rast), list(sense_of_place = sop))
  ref <- rasters[[1]]
  for (nm in names(rasters)) {
    ri <- rasters[[nm]]
    if (!terra::compareGeom(ref, ri, stopOnError = FALSE)) {
      stop("Raster geometry mismatch for ", nm, call. = FALSE)
    }
  }
  s <- terra::sprc(rasters)
  terra::mean(s)
}

write_wri_composite <- function(write_output = WRITE_OUTPUT) {
  log_pt("Building composite (", length(DOMAIN_SINGLE_FILES), " domains + Sense of Place mean).")
  comp <- mean_composite()
  if (!isTRUE(write_output)) {
    log_pt("write_output is FALSE; returning SpatRaster in memory.")
    return(comp)
  }
  out_dir <- final_dir(OUTPUT_COMPOSITE_SUBDIR)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0("wri_composite_score_", LAYER_YEAR, ".tif"))
  terra::writeRaster(comp, out_path, overwrite = TRUE)
  log_pt("Wrote ", out_path)
  invisible(comp)
}

log_pt("Composite stub loaded. Run write_wri_composite() or write_wri_composite(write_output = TRUE).")
