#!/usr/bin/env Rscript
# PROOF RENDER — verify the osmdata->sf->ggplot->ragg pipeline works end to end,
# and render the Honolulu Harbor pier complex with real coordinate axes so we can
# locate the office (orange box on JD's Google screenshot) against OSM buildings.
suppressPackageStartupMessages({
  library(sf); library(osmdata); library(ggplot2); library(ragg)
})
`%||%` <- function(a, b) if (is.null(a)) b else a
nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)

outdir <- file.path(Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps")), "outputs/location-maps")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Inset bbox ~ matches JD's screenshot extent (Honolulu Harbor Piers 19-40 + Chinatown)
bb <- c(xmin = -157.872, ymin = 21.307, xmax = -157.862, ymax = 21.316)

cat("Querying OSM (Overpass) for harbor layers...\n")
q_bbox <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])

endpoints <- c("https://overpass-api.de/api/interpreter",
               "https://overpass.kumi.systems/api/interpreter")
get_osm <- function(key, value = NULL, tries = 6) {
  for (i in seq_len(tries)) {
    ep <- endpoints[((i - 1) %% length(endpoints)) + 1]
    res <- tryCatch({
      set_overpass_url(ep)
      q <- opq(bbox = q_bbox, timeout = 90)
      q <- if (is.null(value)) add_osm_feature(q, key = key) else add_osm_feature(q, key = key, value = value)
      osmdata_sf(q)
    }, error = function(e) e)
    if (!inherits(res, "error")) return(res)
    cat(sprintf("  [%s=%s] attempt %d via %s failed: %s\n", key, value %||% "*", i,
                sub("https://","",ep), conditionMessage(res)))
    Sys.sleep(2 * i)
  }
  stop(sprintf("OSM fetch failed for %s=%s after %d tries", key, value %||% "*", tries))
}

water     <- get_osm("natural", "water")
buildings <- get_osm("building")
piers     <- get_osm("man_made", "pier")
coast     <- list()

cat("water polys:", nrow0(water$osm_polygons), " buildings:", nrow0(buildings$osm_polygons),
    " piers(poly):", nrow0(piers$osm_polygons), " piers(line):", nrow0(piers$osm_lines), "\n")

# provisional office estimate (refine against render)
office <- st_sf(name = "Shipwright", geometry = st_sfc(st_point(c(-157.8672, 21.3122)), crs = 4326))

p <- ggplot() +
  { if (!is.null(water$osm_polygons) && nrow(water$osm_polygons)) geom_sf(data = water$osm_polygons, fill = "#9FC3D6", color = NA) } +
  { if (!is.null(water$osm_multipolygons) && nrow(water$osm_multipolygons)) geom_sf(data = water$osm_multipolygons, fill = "#9FC3D6", color = NA) } +
  { if (!is.null(buildings$osm_polygons) && nrow(buildings$osm_polygons)) geom_sf(data = buildings$osm_polygons, fill = "#C9C2B6", color = "#8A8577", linewidth = 0.1) } +
  { if (!is.null(piers$osm_polygons) && nrow(piers$osm_polygons)) geom_sf(data = piers$osm_polygons, fill = "#D8D2C6", color = NA) } +
  { if (!is.null(piers$osm_lines) && nrow(piers$osm_lines)) geom_sf(data = piers$osm_lines, color = "#8A8577", linewidth = 0.3) } +
  geom_sf(data = office, color = "#101820", fill = "#F2C14E", shape = 21, size = 5, stroke = 1.2) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE) +
  labs(title = "PROOF: Honolulu Harbor from OSM (osmdata->sf->ggplot->ragg)",
       subtitle = "Gridlines = real lon/lat. Provisional pin at -157.8672, 21.3122") +
  theme_minimal(base_size = 11) +
  theme(panel.background = element_rect(fill = "#EDE7DB", color = NA),
        panel.grid = element_line(color = "#B9C7CF", linewidth = 0.25))

outfile <- file.path(outdir, "PROOF-harbor.png")
agg_png(outfile, width = 1600, height = 1500, units = "px", res = 150)
print(p)
invisible(dev.off())
cat("Wrote", outfile, "\n")
saveRDS(list(water=water, buildings=buildings, piers=piers, coast=coast), file.path(outdir, "harbor-osm.rds"))
cat("Saved OSM cache to harbor-osm.rds\n")
