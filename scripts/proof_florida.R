#!/usr/bin/env Rscript
# Hard-gate proof for the Florida map: one UA-bearing Overpass POST, parsed
# offline, then a bare Port Everglades coastline/water render.
suppressPackageStartupMessages({
  library(sf); library(osmdata); library(ggplot2); library(ragg)
})

ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
OUT  <- file.path(ROOT, "outputs/location-maps")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

bb <- c(xmin = -80.145, ymin = 26.065, xmax = -80.100, ymax = 26.120)
query <- sprintf(paste0(
  "[out:xml][timeout:90];(",
  "way[\"natural\"=\"coastline\"](%.6f,%.6f,%.6f,%.6f);",
  "nwr[\"natural\"=\"water\"](%.6f,%.6f,%.6f,%.6f);",
  ");(._;>;);out body;"),
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

fetch_overpass <- function(query, tries = length(endpoints)) {
  tmp <- tempfile(fileext = ".osm")
  for (i in seq_len(tries)) {
    ep <- endpoints[((i - 1L) %% length(endpoints)) + 1L]
    cat(sprintf("Overpass attempt %d via %s\n", i, ep))
    suppressWarnings(system2("curl", args = c(
      "-sS", "--fail-with-body", "--max-time", "90",
      "-A", shQuote(ua), "-X", "POST", shQuote(ep),
      "--data-urlencode", shQuote(paste0("data=", query)),
      "-o", shQuote(tmp))))
    if (valid_osm(tmp)) return(tmp)
    if (file.exists(tmp)) unlink(tmp)
    tmp <- tempfile(fileext = ".osm")
    Sys.sleep(2 * i)
  }
  stop("Florida proof failed: Overpass returned no valid OSM XML.")
}

osm_file <- fetch_overpass(query)
on.exit(if (file.exists(osm_file)) unlink(osm_file), add = TRUE)
osm <- osmdata_sf(doc = osm_file)

lines <- osm$osm_lines
polys <- osm$osm_polygons
multipolys <- osm$osm_multipolygons

coast_lines <- if (!is.null(lines) && nrow(lines)) {
  lines[!is.na(lines$natural) & lines$natural == "coastline", ]
} else NULL
water_lines <- if (!is.null(lines) && nrow(lines)) {
  lines[!is.na(lines$natural) & lines$natural == "water", ]
} else NULL
water_polys <- if (!is.null(polys) && nrow(polys)) {
  polys[!is.na(polys$natural) & polys$natural == "water", ]
} else NULL
water_multipolys <- if (!is.null(multipolys) && nrow(multipolys)) {
  multipolys[!is.na(multipolys$natural) & multipolys$natural == "water", ]
} else NULL
nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)
cat(sprintf("Parsed coastline lines=%d, water lines=%d, water polygons=%d, water multipolygons=%d\n",
            nrow0(coast_lines), nrow0(water_lines), nrow0(water_polys), nrow0(water_multipolys)))
if (nrow0(coast_lines) + nrow0(water_lines) + nrow0(water_polys) + nrow0(water_multipolys) == 0L)
  stop("Florida proof failed: parsed response contains no coastline or water geometry.")

p <- ggplot() +
  { if (nrow0(water_polys)) geom_sf(data = water_polys, fill = "#9FC3D6", color = "#7FB0C9", linewidth = 0.15) } +
  { if (nrow0(water_multipolys)) geom_sf(data = water_multipolys, fill = "#9FC3D6", color = "#7FB0C9", linewidth = 0.15) } +
  { if (nrow0(water_lines)) geom_sf(data = water_lines, color = "#7FB0C9", linewidth = 0.4) } +
  { if (nrow0(coast_lines)) geom_sf(data = coast_lines, color = "#101820", linewidth = 0.65) } +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE) +
  labs(title = "HARD-GATE PROOF · PORT EVERGLADES",
       subtitle = "OSM coastline + natural=water · UA-bearing curl POST, parsed offline") +
  theme_minimal(base_family = "Arial") +
  theme(panel.background = element_rect(fill = "#EDE7DB", color = NA),
        panel.grid = element_line(color = "#FFFFFF", linewidth = 0.25),
        plot.title = element_text(face = "bold", color = "#101820"))

outfile <- file.path(OUT, "PROOF-florida-coast-water.png")
agg_png(outfile, width = 1500, height = 1250, units = "px", res = 150)
print(p)
invisible(dev.off())
cat("Wrote", outfile, "\n")
