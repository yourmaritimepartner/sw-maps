#!/usr/bin/env Rscript
# mod_inset.R — Agent C deliverable for the Shipwright location map.
# Styled INSET card: a Honolulu Harbor closeup (~1.4 w:h) showing water, building
# footprints (with the office shed subtly highlighted), pier lines, and the office
# PIN, rendered as a clean flat-vector bordered card.
#
# Data: cached OSM only — outputs/location-maps/harbor-osm.rds
#       list(water, buildings, piers, coast) as osmdata_sf objects. No network.
#
# Sources cleanly (defines functions only). Palette + ragg pattern per the shared
# build contract (docs/map-build-contract.md).

suppressPackageStartupMessages({
  library(sf); library(ggplot2)
})

# ---- palette (build contract) -------------------------------------------------
.inset_pal <- list(
  ink        = "#101820",
  yellow     = "#F2C14E",   # brand — marker/accent only
  white      = "#FFFFFF",
  text2      = "#5A6870",   # secondary text (caption)
  water      = "#9FC3D6",   # calm blue (sea / harbor basin)
  water_deep = "#7FB0C9",
  land       = "#EDE7DB",   # warm neutral land base (pier decks / aprons)
  gold       = "#F2C14E",   # marker — yellow-gold, FILL ONLY (no outline)
  bldg       = "#C9C2B6",   # warm gray building fill
  bldg_edge  = "#5A5348",   # thin warm-ink building edge
  office     = "#B9A98A",   # office shed highlight (warmer/darker fill)
  pier       = "#7E7A6C",   # pier lines
  border     = "#101820"    # card border
)

# ---- font: Aptos, silent fallback to Arial/Helvetica --------------------------
.inset_font <- function() {
  fam <- tryCatch({
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      have <- systemfonts::system_fonts()$family
      for (f in c("Aptos", "Arial", "Helvetica")) if (f %in% have) return(f)
    }
    "sans"
  }, error = function(e) "sans")
  fam
}

# small helpers ---------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
.nz    <- function(x) !is.null(x) && inherits(x, "sf") && nrow(x) > 0

# Pull a clean sf polygon layer out of an osmdata_sf object (polys + multipolys),
# reprojected to 4326, geometry-only to avoid ragged column binds.
.osm_polys <- function(o) {
  if (is.null(o)) return(NULL)
  parts <- list()
  if (.nz(o$osm_polygons))      parts[[length(parts)+1]] <- st_geometry(st_transform(o$osm_polygons, 4326))
  if (.nz(o$osm_multipolygons)) parts[[length(parts)+1]] <- st_geometry(st_transform(o$osm_multipolygons, 4326))
  if (!length(parts)) return(NULL)
  g <- do.call(c, parts)
  st_sf(geometry = g)
}
.osm_lines <- function(o) {
  if (is.null(o) || !.nz(o$osm_lines)) return(NULL)
  st_sf(geometry = st_geometry(st_transform(o$osm_lines, 4326)))
}

#' Identify the office shed footprint.
#' Returns the single building polygon that contains the office point, or (if the
#' point falls just off a footprint, as OSM generalization can cause) the nearest
#' building within ~40 m. NULL if nothing is close.
inset_office_building <- function(buildings_sf, office_sf) {
  if (!.nz(buildings_sf) || is.null(office_sf)) return(NULL)
  pt <- st_geometry(st_transform(office_sf, 4326))[1]
  inside <- which(lengths(st_intersects(buildings_sf, pt)) > 0)
  if (length(inside)) return(buildings_sf[inside[1], ])
  # metres, measured in a local azimuthal projection (lon/lat distances with s2 off need
  # lwgeom, which the VM does not have)
  xy <- st_coordinates(pt)[1, ]
  aeqd <- sprintf("+proj=aeqd +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs", xy[2], xy[1])
  d <- as.numeric(st_distance(st_transform(buildings_sf, aeqd), st_transform(pt, aeqd)))
  i <- which.min(d)
  if (length(i) && d[i] <= 40) return(buildings_sf[i, ])
  NULL
}

#' build_inset — styled harbor inset card.
#'
#' @param office_sf sf POINT (EPSG:4326) for the office PIN.
#' @param bbox4326  c(xmin, ymin, xmax, ymax). Default tuned to a genuine ~1.4 w:h
#'   at 21 N (lon degree ~= 0.9336 x lat degree), framing the Pier 19-21 complex.
#' @param harbor    list(water, buildings, piers, coast) from harbor-osm.rds.
#' @param ocean     optional sf POLYGON of the sea (e.g. Agent A oceanfill). If
#'   supplied it is painted as the water body; otherwise the whole card background
#'   is filled with the water colour to approximate the harbor/sea (the harbor
#'   basin is coastline, not in `harbor$water`).
#' @return a ggplot.
build_inset <- function(office_sf,
                        bbox4326 = c(-157.874, 21.3075, -157.862, 21.3155),
                        harbor   = NULL,
                        ocean    = NULL,
                        land     = NULL,
                        caption  = "Honolulu Harbor") {
  stopifnot(length(bbox4326) == 4)
  if (is.null(harbor)) stop("build_inset(): `harbor` (harbor-osm.rds contents) is required.")
  xmin <- bbox4326[1]; ymin <- bbox4326[2]; xmax <- bbox4326[3]; ymax <- bbox4326[4]
  ff <- .inset_font()

  buildings <- .osm_polys(harbor$buildings)
  water_p   <- .osm_polys(harbor$water)
  piers_l   <- .osm_lines(harbor$piers)
  piers_p   <- .osm_polys(harbor$piers)

  office_bldg <- inset_office_building(buildings, office_sf)

  # Remove the office footprint from the base building layer so the highlight
  # draws once, cleanly.
  base_bldg <- buildings
  if (.nz(buildings) && !is.null(office_bldg)) {
    keep <- lengths(st_equals(buildings, office_bldg)) == 0
    base_bldg <- buildings[keep, ]
  }

  p <- ggplot()

  # --- sea + land ------------------------------------------------------------
  # panel.background is water (the sea). Draw LAND (A's oceanfill land polygon =
  # pier decks/aprons) as the base so buildings sit on ground, not floating in water.
  if (.nz(ocean)) {
    p <- p + geom_sf(data = st_transform(ocean, 4326),
                     fill = .inset_pal$water, color = NA)
  }
  if (.nz(land)) {
    p <- p + geom_sf(data = st_transform(land, 4326),
                     fill = .inset_pal$land, color = NA)
  }
  # OSM water polygons (canals/basins that ARE tagged natural=water)
  if (.nz(water_p)) {
    p <- p + geom_sf(data = water_p, fill = .inset_pal$water_deep, color = NA)
  }

  # --- pier aprons (polygon piers), then buildings ---------------------------
  if (.nz(piers_p)) {
    p <- p + geom_sf(data = piers_p, fill = "#D8D2C6", color = NA)
  }
  if (.nz(base_bldg)) {
    p <- p + geom_sf(data = base_bldg, fill = .inset_pal$bldg,
                     color = .inset_pal$bldg_edge, linewidth = 0.12)
  }
  # office shed highlight
  if (!is.null(office_bldg)) {
    p <- p + geom_sf(data = office_bldg, fill = .inset_pal$office,
                     color = .inset_pal$ink, linewidth = 0.45)
  }
  # --- pier lines ------------------------------------------------------------
  if (.nz(piers_l)) {
    p <- p + geom_sf(data = piers_l, color = .inset_pal$pier, linewidth = 0.55)
  }

  # --- office PIN (yellow-gold, FILL ONLY, no outline) -----------------------
  p <- p + geom_sf(data = st_transform(office_sf, 4326),
                   shape = 16, size = 6, color = .inset_pal$gold)

  # --- caption ---------------------------------------------------------------
  p <- p + annotate("text",
                    x = xmin + 0.030 * (xmax - xmin),
                    y = ymin + 0.045 * (ymax - ymin),
                    label = caption,
                    hjust = 0, vjust = 0, family = ff,
                    fontface = "bold", size = 3.4, color = .inset_pal$text2)

  # --- crop + theme (flat-vector card) --------------------------------------
  p <- p +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
             expand = FALSE, datum = NA) +
    theme_void(base_family = ff) +
    theme(
      panel.background = element_rect(fill = .inset_pal$water, color = NA),
      plot.background  = element_rect(fill = .inset_pal$white, color = NA),
      # thin card border drawn as a panel border
      panel.border     = element_rect(fill = NA, color = .inset_pal$border,
                                       linewidth = 0.9),
      plot.margin      = margin(6, 6, 6, 6)
    )
  p
}
