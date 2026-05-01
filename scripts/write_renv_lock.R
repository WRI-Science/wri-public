#!/usr/bin/env Rscript
# One-off helper: build renv.lock (Package, Version, Source, Repository, Hash) from CRAN.
# Run with TMPDIR pointing at an exec-capable temp dir, e.g.:
#   TMPDIR=/home/cbroderick/buildtmp Rscript scripts/write_renv_lock.R

Sys.setenv(TMPDIR = Sys.getenv("TMPDIR", "/home/cbroderick/buildtmp"))
options(repos = c(CRAN = "https://cloud.r-project.org"))

suppressPackageStartupMessages({
  library(renv)
  library(jsonlite)
})

cmd <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", cmd, value = TRUE)
root <- if (length(fa)) {
  dirname(dirname(normalizePath(sub("^--file=", "", fa[1]))))
} else {
  normalizePath(getwd())
}
if (!dir.exists(file.path(root, "species"))) {
  stop("Run from repository root or via Rscript scripts/write_renv_lock.R", call. = FALSE)
}

d <- renv::dependencies(root, progress = FALSE)
pkgs <- sort(unique(d[["Package"]]))
drop <- c(
  "tools", "compiler", "parallel", "grDevices", "stats", "utils",
  "datasets", "graphics", "methods", "grid", "splines", "stats4",
  "tcltk", "base"
)
pkgs <- setdiff(pkgs, drop)

ap <- available.packages(filters = c("R_version", "OS_type", "subarch"))
deps_tree <- unique(unlist(c(
  pkgs,
  tools::package_dependencies(
    pkgs,
    db = ap,
    recursive = TRUE,
    which = c("Depends", "Imports", "LinkingTo")
  )
)))
deps_tree <- sort(deps_tree[!deps_tree %in% drop])

tmpdir <- tempfile("wri_renv_lock")
dir.create(tmpdir)
on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

records <- list()
for (p in deps_tree) {
  if (!p %in% rownames(ap)) next
  ver <- unname(ap[p, "Version"])
  dest <- file.path(tmpdir, sprintf("%s_%s.tar.gz", p, ver))
  url <- sprintf("https://cran.r-project.org/src/contrib/%s_%s.tar.gz", p, ver)
  dl <- download.file(url, dest, quiet = TRUE)
  if (dl != 0 || !file.exists(dest) || file.info(dest)$size < 100L) {
    url <- sprintf(
      "https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz",
      p, p, ver
    )
    dl <- download.file(url, dest, quiet = TRUE)
  }
  if (dl != 0 || !file.exists(dest) || file.info(dest)$size < 100L) {
    warning("could not download ", p, " ", ver, call. = FALSE)
    next
  }
  ex <- file.path(tmpdir, "extract", p)
  dir.create(ex, recursive = TRUE, showWarnings = FALSE)
  untar(dest, exdir = ex)
  desc_paths <- list.files(ex, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE)
  if (!length(desc_paths)) {
    warning("no DESCRIPTION for ", p, call. = FALSE)
    next
  }
  h <- renv:::renv_hash_description(desc_paths[[1]])
  records[[p]] <- list(
    Package = unbox(p),
    Version = unbox(ver),
    Source = unbox("Repository"),
    Repository = unbox("CRAN"),
    Hash = unbox(h)
  )
}

out <- list(
  renv = list(Version = unbox(as.character(packageVersion("renv")))),
  R = list(
    Version = unbox(paste(R.version$major, R.version$minor, sep = ".")),
    Repositories = list(list(
      Name = unbox("CRAN"),
      URL = unbox("https://cloud.r-project.org")
    ))
  ),
  Packages = records
)

lock_path <- file.path(root, "renv.lock")
writeLines(toJSON(out, pretty = 4, auto_unbox = FALSE), lock_path)
message("Wrote ", lock_path, " (", length(records), " packages)")
