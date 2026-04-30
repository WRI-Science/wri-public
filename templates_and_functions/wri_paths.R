# Wildfire Resilience Index — shared path configuration
# Override project root with env var WRI_PROJECT_ROOT (see README and .Renviron.example).

wri_project_root <- Sys.getenv(
  "WRI_PROJECT_ROOT",
  unset = "/home/shares/wwri-wildfire"
)

#' Build paths under `{root}/data/...`
wri_data <- function(...) {
  file.path(wri_project_root, "data", ...)
}

#' Build paths under `{root}/final_layers/...`
wri_final_layers <- function(...) {
  file.path(wri_project_root, "final_layers", ...)
}
