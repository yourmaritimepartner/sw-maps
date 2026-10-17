#!/usr/bin/env Rscript
# Fetch the reusable Port Everglades inset cache with ONE UA-bearing Overpass POST.
# The combined response is parsed offline, split by tag, and saved in the same
# list-of-osmdata-layers shape consumed by mod_inset.R.
suppressPackageStartupMessages({
  library(sf); library(osmdata)
})

ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
OUT  <- file.path(ROOT, "outputs/location-maps")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# Covers the Amman Building, nearby port berths/terminals, and turning basin.
bb <- c(xmin = -80.145, ymin = 26.075, xmax = -80.100, ymax = 26.115)
query <- sprintf(paste0(
  "[out:xml][timeout:120];(",
  "nwr[\"building\"](%.6f,%.6f,%.6f,%.6f);",
  "nwr[\"natural\"=\"water\"](%.6f,%.6f,%.6f,%.6f);",
  "nwr[\"man_made\"=\"pier\"](%.6f,%.6f,%.6f,%.6f);",
  "nwr[\"harbour\"](%.6f,%.6f,%.6f,%.6f);",
  "way[\"natural\"=\"coastline\"](%.6f,%.6f,%.6f,%.6f);",
  "way[\"highway\"](%.6f,%.6f,%.6f,%.6f);",
  ");(._;>;);out body;"),
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"],
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"],
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"],
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"],
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"],
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"])

endpoints <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)
ua <- "sw-maps-florida/1.0 (Shipwright location map; contact j@djinna.com)"
valid_osm <- function(path) {
  file.exists(path) && file.info(path)$size > 500 &&
    any(grepl("<osm", readLines(path, n = 5, warn = FALSE), fixed = TRUE))
}

fetch_overpass <- function(query, tries = 4L) {
  for (i in seq_len(tries)) {
    tmp <- tempfile(fileext = ".osm")
    ep <- endpoints[((i - 1L) %% length(endpoints)) + 1L]
    cat(sprintf("Overpass attempt %d via %s\n", i, ep))
    suppressWarnings(system2("curl", args = c(
      "-sS", "--fail-with-body", "--max-time", "150",
      "-A", shQuote(ua), "-X", "POST", shQuote(ep),
      "--data-urlencode", shQuote(paste0("data=", query)),
      "-o", shQuote(tmp))))
    if (valid_osm(tmp)) return(tmp)
    if (file.exists(tmp)) unlink(tmp)
    Sys.sleep(3 * i)
  }
  stop("Port Everglades OSM fetch failed after endpoint failover.")
}

empty_like <- function(x) x[FALSE, , drop = FALSE]
subset_osm <- function(o, predicate) {
  parts <- c("osm_points", "osm_lines", "osm_polygons", "osm_multilines", "osm_multipolygons")
  ans <- setNames(vector("list", length(parts)), parts)
  for (nm in parts) {
    x <- o[[nm]]
    if (is.null(x) || !inherits(x, "sf")) {
      ans[[nm]] <- x
    } else if (!nrow(x)) {
      ans[[nm]] <- x
    } else {
      keep <- predicate(x)
      keep[is.na(keep)] <- FALSE
      ans[[nm]] <- if (any(keep)) x[keep, , drop = FALSE] else empty_like(x)
    }
  }
  ans
}
tagged <- function(col, value = NULL) function(x) {
  if (!col %in% names(x)) return(rep(FALSE, nrow(x)))
  if (is.null(value)) !is.na(x[[col]]) & nzchar(x[[col]]) else !is.na(x[[col]]) & x[[col]] == value
}

osm_file <- fetch_overpass(query)
on.exit(if (file.exists(osm_file)) unlink(osm_file), add = TRUE)
cat("Parsing combined response offline...\n")
osm <- osmdata_sf(doc = osm_file)

cache <- list(
  water     = subset_osm(osm, tagged("natural", "water")),
  buildings = subset_osm(osm, tagged("building")),
  piers     = subset_osm(osm, tagged("man_made", "pier")),
  harbour   = subset_osm(osm, tagged("harbour")),
  coast     = subset_osm(osm, tagged("natural", "coastline")),
  roads     = subset_osm(osm, tagged("highway")),
  bbox      = bb,
  fetched_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)
count <- function(layer, part) nrow0(cache[[layer]][[part]])
cat(sprintf(paste0(
  "buildings polygons=%d multipolygons=%d | water polygons=%d multipolygons=%d | ",
  "piers lines=%d polygons=%d | coastline lines=%d | roads lines=%d\n"),
  count("buildings", "osm_polygons"), count("buildings", "osm_multipolygons"),
  count("water", "osm_polygons"), count("water", "osm_multipolygons"),
  count("piers", "osm_lines"), count("piers", "osm_polygons"),
  count("coast", "osm_lines"), count("roads", "osm_lines")))
if (count("buildings", "osm_polygons") + count("buildings", "osm_multipolygons") == 0L)
  stop("Parsed response contains no building footprints.")

outfile <- file.path(OUT, "florida-osm.rds")
saveRDS(cache, outfile)
cat("Saved", outfile, "\n")
