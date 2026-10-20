#!/usr/bin/env Rscript
# Serve the map front end on :8000 (exe.dev proxies https://sw-maps.exe.xyz -> :8000).
#   SW_MAPS_ROOT=~/sw-maps Rscript app/serve.R
suppressPackageStartupMessages(library(plumber))
ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))

pr(file.path(ROOT, "app/plumber.R")) |>
  pr_static("/files", file.path(ROOT, "outputs/location-maps")) |>
  pr_static("/maps-files", file.path(ROOT, "outputs/maps")) |>
  pr_static("/comp-files", file.path(ROOT, "outputs/comps")) |>
  pr_static("/static", file.path(ROOT, "app/static")) |>
  pr_run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", "8000")), docs = FALSE)
