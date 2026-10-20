#!/usr/bin/env Rscript
# Prove a map's raw material with tools, for any config (offices and work sites):
#   1. the pin falls inside the target building footprint -- the one the renderer will
#      highlight (mod_inset.R's inset_office_building), and, for sites, the OSM id chosen
#      at confirmation;
#   2. building area intersecting water is ~0 inside the inset view. Water = sea (OSM
#      coastline via mod_oceanfill) + natural=water, minus pier decks (buildings on piers
#      stand over water legitimately). Catches a flipped coastline or misplaced extent.
#   Rscript scripts/check_site.R <id> [--json data/sites/<id>-checks.json]
# Exit status 1 if a check fails.
suppressPackageStartupMessages({ library(sf); library(jsonlite) })
suppressMessages(sf_use_s2(FALSE))
ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
args <- commandArgs(trailingOnly = TRUE)
id <- args[1]
json_out <- if (length(args) >= 3 && args[2] == "--json") args[3] else NULL
for (m in c("mod_oceanfill.R", "mod_inset.R")) source(file.path(ROOT, "scripts", m))
source(file.path(ROOT, "configs", paste0(id, ".R")))

WET_MAX <- 0.005          # building area in water, as a share of building area: pass below 0.5 %
EDGE_TOL <- 2             # metres: footprints and coastlines are traced from different imagery and
                          # disagree by 1-3 m, so only water more than 2 m inside an outline counts
crs_m <- sprintf("+proj=aeqd +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs", office$lat, office$lon)
h <- readRDS(file.path(ROOT, "outputs/location-maps", office$osm_rds))
polys_with_ids <- function(o) {
  take <- function(x, type) if (!is.null(x) && inherits(x, "sf") && nrow(x))
    st_sf(osm = paste0(type, "/", x$osm_id), name = if ("name" %in% names(x)) x$name else NA_character_,
          geometry = st_geometry(st_transform(x, 4326)))
  parts <- Filter(Negate(is.null), list(take(o$osm_polygons, "way"), take(o$osm_multipolygons, "relation")))
  if (length(parts)) do.call(rbind, parts) else NULL
}
B <- polys_with_ids(h$buildings)
pin <- st_sf(geometry = st_sfc(st_point(c(office$lon, office$lat)), crs = 4326))
res <- list(id = id, kind = office$kind %||% "office", checked = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"))

# ---- 1. pin in footprint -----------------------------------------------------------
tb <- if (!is.null(B)) inset_office_building(B, pin) else NULL   # the renderer's pick
expected <- office$target_osm %||% ""
if (is.null(tb)) {
  res$pin <- list(pass = !nzchar(expected), target = NA, inside = FALSE,
                  note = if (nzchar(expected)) "no footprint within 40 m of the pin" else
                         "no target footprint (point site); nothing to highlight")
} else {
  tbm <- st_transform(tb, crs_m); pm <- st_transform(pin, crs_m)
  inside <- lengths(st_within(pm, tbm))[1] > 0
  depth <- as.numeric(st_distance(pm, st_cast(st_geometry(tbm), "MULTILINESTRING")))   # to the outline
  id_ok <- !nzchar(expected) || identical(tb$osm[1], expected)
  res$pin <- list(pass = inside && id_ok, target = tb$osm[1], name = tb$name[1] %||% NA,
                  expected = if (nzchar(expected)) expected else NA, inside = inside,
                  edge_m = round(depth, 1), footprint_m2 = round(as.numeric(st_area(tbm))),
                  note = if (!inside) sprintf("pin is %.1f m outside the footprint the renderer highlights", depth)
                         else if (!id_ok) sprintf("renderer highlights %s, not the confirmed %s", tb$osm[1], expected)
                         else sprintf("inside %s, %.1f m from its outline", tb$osm[1], depth))
}

# ---- 2. buildings in water -----------------------------------------------------------
iv <- office$inset_view
view <- st_transform(st_as_sfc(st_bbox(c(xmin = iv[["xmin"]], ymin = iv[["ymin"]], xmax = iv[["xmax"]], ymax = iv[["ymax"]]), crs = 4326)), crs_m)
ol <- ocean_land(office$inset_bbox)
to_m <- function(x) if (is.null(x) || !length(st_geometry(x))) NULL else st_make_valid(st_union(st_make_valid(st_transform(st_geometry(x), crs_m))))
sea   <- to_m(ol$ocean)
lakes <- to_m(polys_with_ids(h$water))
piers <- to_m(polys_with_ids(h$piers))
wet <- Reduce(function(a, b) if (is.null(a)) b else if (is.null(b)) a else st_union(a, b), list(sea, lakes))
if (!is.null(wet)) wet <- suppressWarnings(st_intersection(wet, view))
if (!is.null(wet) && !is.null(piers)) wet <- suppressWarnings(st_difference(wet, piers))
Bm <- if (!is.null(B)) st_make_valid(st_transform(B, crs_m)) else NULL
if (!is.null(Bm)) Bm <- Bm[lengths(st_intersects(Bm, view)) > 0, ]
if (is.null(Bm) || !nrow(Bm)) {
  res$water <- list(pass = TRUE, buildings = 0L, note = "no buildings in the inset view")
} else {
  area <- as.numeric(st_area(suppressWarnings(st_intersection(Bm, view))))
  wet_in <- function(g) if (is.null(wet) || all(st_is_empty(wet))) rep(0, length(g)) else
    vapply(seq_along(g), function(i) {
      if (st_is_empty(g[i])) return(0)
      x <- suppressWarnings(st_intersection(g[i], wet))
      if (length(x)) sum(as.numeric(st_area(x))) else 0
    }, 0)
  raw_a <- wet_in(st_geometry(Bm))
  wet_a <- wet_in(suppressWarnings(st_buffer(st_geometry(Bm), -EDGE_TOL)))
  share <- sum(wet_a) / sum(area)
  worst <- order(-raw_a)[seq_len(min(3, sum(raw_a > 1)))]
  res$water <- list(pass = share < WET_MAX, buildings = nrow(Bm), building_m2 = round(sum(area)),
                    wet_m2 = round(sum(wet_a), 1), raw_wet_m2 = round(sum(raw_a), 1), share = signif(share, 3),
                    edge_tolerance_m = EDGE_TOL, sea_source = ol$source,
                    worst = lapply(worst, function(i) list(osm = Bm$osm[i], name = Bm$name[i] %||% NA,
                                                          wet_m2 = round(wet_a[i], 1), raw_wet_m2 = round(raw_a[i], 1),
                                                          of_m2 = round(area[i]))),
                    note = sprintf("%.2f %% of %s m² of buildings in water (%.0f m²; %.0f m² counting the outer %d m)",
                                   100 * share, format(round(sum(area)), big.mark = ","), sum(wet_a), sum(raw_a), EDGE_TOL))
}
res$pass <- isTRUE(res$pin$pass) && isTRUE(res$water$pass)

cat(sprintf("%-9s %s  pin: %s\n%-9s %s  water: %s\n", id, if (res$pin$pass) "PASS" else "FAIL", res$pin$note,
            "", if (res$water$pass) "PASS" else "FAIL", res$water$note))
for (w in res$water$worst) cat(sprintf("           %s %s: %.1f of %d m² wet (%.1f with the outer %d m)\n",
                                      w$osm, w$name %||% "", w$wet_m2, w$of_m2, w$raw_wet_m2, EDGE_TOL))
if (!is.null(json_out)) {
  dir.create(dirname(json_out), showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(res, auto_unbox = TRUE, pretty = TRUE, na = "null"), json_out)
}
quit(status = if (res$pass) 0 else 1)
