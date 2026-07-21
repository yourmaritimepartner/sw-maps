#!/usr/bin/env Rscript
# mod_terrain.R  —  Agent D
# Subtle shaded-relief (hillshade) base for the MAIN establishing map so the
# Koʻolau Range reads as mountains behind Honolulu instead of blank beige.
#
# DATA SOURCE: elevatr (AWS/OpenTopo terrain tiles) — NOT Overpass. DEM cached
# to outputs/location-maps/ as GeoTIFF so re-runs are fully offline.
#
# Design: a flat cartographic map, NOT a terrain poster. The relief is baked
# toward the land color #EDE7DB so the flat coastal plain stays exactly land
# color and only ridgelines pick up a soft warm-gray shadow (and a whisper of
# highlight on sun-facing slopes). The strength knob is `alpha` — keep it low.
#
# Exposes:
#   terrain_layer(bbox4326, alpha = 0.35, z = 12) -> list of ggplot2 layers
#       Add FIRST to a ggplot (before geom_sf layers). Returns an
#       annotation_raster (opaque, colors pre-blended toward land) so it renders
#       correctly UNDER later geom_sf layers. Sea (elev < 0) is transparent.
#   hillshade_raster(bbox4326, z = 12) -> terra SpatRaster of shade values (0..1),
#       EPSG:4326. Useful internally / for custom blends.
#
# Geometry/raster stays EPSG:4326 to match the other modules.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(elevatr)
  library(ggplot2)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## default MAIN bbox (matches contract / mod_mainmap.R)
TERRAIN_MAIN_BBOX <- c(xmin = -157.98, ymin = 21.24, xmax = -157.79, ymax = 21.40)

## palette anchors (per contract) ------------------------------------------------
.TERRAIN_LAND   <- c(237, 231, 219) / 255  # #EDE7DB warm neutral land
.TERRAIN_SHADOW <- c( 74,  68,  56) / 255  # #4A4438 warm dark-gray shadow
.TERRAIN_LIGHT  <- c(255, 252, 245) / 255  # #FFFCF5 warm near-white highlight

.terrain_outdir <- function() {
  d <- path.expand("~/sw-projects/sw-locations/outputs/location-maps")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

.bbox_key <- function(bb) {
  sprintf("%.5f_%.5f_%.5f_%.5f",
          bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}

.norm_bbox <- function(bbox4326) {
  bb <- bbox4326
  if (is.null(names(bb))) names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bb
}

## ---- DEM fetch + disk cache --------------------------------------------------
# Returns a terra SpatRaster elevation grid in EPSG:4326, cached to a GeoTIFF
# keyed by bbox + z so subsequent runs are offline.
dem_raster <- function(bbox4326 = TERRAIN_MAIN_BBOX, z = 12) {
  bb   <- .norm_bbox(bbox4326)
  path <- file.path(.terrain_outdir(),
                    sprintf("dem-%s-z%d.tif", .bbox_key(bb), z))

  if (file.exists(path)) {
    r <- terra::rast(path)
    if (is.na(terra::crs(r)) || terra::crs(r) == "")
      terra::crs(r) <- "EPSG:4326"
    return(r)
  }

  poly <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = bb[["xmin"]], ymin = bb[["ymin"]],
      xmax = bb[["xmax"]], ymax = bb[["ymax"]]), crs = 4326)))

  dem <- elevatr::get_elev_raster(locations = poly, z = z, clip = "bbox",
                                  verbose = FALSE)
  r <- terra::rast(dem)
  # reproject to EPSG:4326 to match the other layers (no-op if already 4326)
  r <- terra::project(r, "EPSG:4326")
  terra::writeRaster(r, path, overwrite = TRUE)
  r
}

## ---- hillshade ----------------------------------------------------------------
# Soft shaded relief (0..1 illumination) in EPSG:4326. Sun from the NW
# (direction 315) at 45° altitude — the conventional cartographic light.
hillshade_raster <- function(bbox4326 = TERRAIN_MAIN_BBOX, z = 12) {
  r      <- dem_raster(bbox4326, z = z)
  slope  <- terra::terrain(r, "slope",  unit = "radians")
  aspect <- terra::terrain(r, "aspect", unit = "radians")
  hs     <- terra::shade(slope, aspect, angle = 45, direction = 315)
  terra::crs(hs) <- "EPSG:4326"
  hs
}

## ---- blended relief -> ggplot layer ------------------------------------------
# Builds an annotation_raster whose flat pixels are exactly the land color and
# whose slopes carry a soft shadow / whisper of highlight, scaled by `alpha`.
# Sea (elev < 0) is transparent so the ocean fill shows through. Added first,
# it sits UNDER the vector layers as a gentle relief base.
terrain_layer <- function(bbox4326 = TERRAIN_MAIN_BBOX, alpha = 0.35, z = 12) {
  bb <- .norm_bbox(bbox4326)

  dem <- dem_raster(bb, z = z)
  hs  <- hillshade_raster(bb, z = z)

  hv <- terra::values(hs, mat = FALSE)
  ev <- terra::values(dem, mat = FALSE)

  # flat reference = illumination of level ground; relief is measured from it
  flat_ref <- stats::median(hv, na.rm = TRUE)
  rel <- hv - flat_ref

  # asymmetric response: shadows read stronger than highlights (natural relief),
  # both gentle. Scales chosen so only genuine ridges tint appreciably.
  shadow_amt <- pmin(pmax(-rel / 0.55, 0), 1)
  light_amt  <- pmin(pmax( rel / 0.32, 0), 1)

  w_sh <- alpha * shadow_amt          # toward shadow
  w_li <- alpha * 0.45 * light_amt    # toward highlight (subtler)

  # per-channel blend: land + w_sh*(shadow-land) + w_li*(light-land)
  R <- .TERRAIN_LAND[1] + w_sh * (.TERRAIN_SHADOW[1] - .TERRAIN_LAND[1]) +
                          w_li * (.TERRAIN_LIGHT[1]  - .TERRAIN_LAND[1])
  G <- .TERRAIN_LAND[2] + w_sh * (.TERRAIN_SHADOW[2] - .TERRAIN_LAND[2]) +
                          w_li * (.TERRAIN_LIGHT[2]  - .TERRAIN_LAND[2])
  B <- .TERRAIN_LAND[3] + w_sh * (.TERRAIN_SHADOW[3] - .TERRAIN_LAND[3]) +
                          w_li * (.TERRAIN_LIGHT[3]  - .TERRAIN_LAND[3])

  # NA/NaN pixels (sea, no-data) -> land color placeholder, masked to NA below
  R[!is.finite(R)] <- .TERRAIN_LAND[1]
  G[!is.finite(G)] <- .TERRAIN_LAND[2]
  B[!is.finite(B)] <- .TERRAIN_LAND[3]
  R <- pmin(pmax(R, 0), 1); G <- pmin(pmax(G, 0), 1); B <- pmin(pmax(B, 0), 1)

  cols <- grDevices::rgb(R, G, B)
  # sea + missing -> transparent (let the ocean fill / background carry it)
  cols[is.na(hv) | is.na(ev) | ev < 0] <- NA

  nr <- terra::nrow(hs); nc <- terra::ncol(hs)
  # terra values are row-major, north row first -> matrix with byrow = TRUE gives
  # the same top(north)->bottom(south) order annotation_raster expects.
  m <- matrix(cols, nrow = nr, ncol = nc, byrow = TRUE)

  ex <- terra::ext(hs)
  list(
    ggplot2::annotation_raster(
      m,
      xmin = ex$xmin, xmax = ex$xmax,
      ymin = ex$ymin, ymax = ex$ymax,
      interpolate = TRUE)
  )
}

## ---- verify (only when run directly: `Rscript mod_terrain.R`) -----------------
if (sys.nframe() == 0L) {
  outdir <- .terrain_outdir()
  MAIN_BBOX <- TERRAIN_MAIN_BBOX

  suppressPackageStartupMessages(library(ragg))

  cat("Building terrain layer for MAIN bbox (z=12)...\n")
  tl <- terrain_layer(MAIN_BBOX, alpha = 0.35, z = 12)

  # semi-transparent land polygon on top, to prove it reads UNDER vectors and
  # that the flat plain stays land-color while the Koʻolau ridge shows relief.
  land <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = MAIN_BBOX[["xmin"]], ymin = MAIN_BBOX[["ymin"]],
      xmax = MAIN_BBOX[["xmax"]], ymax = MAIN_BBOX[["ymax"]]), crs = 4326)))

  g <- ggplot2::ggplot() +
    tl +
    ggplot2::geom_sf(data = land, fill = "#EDE7DB", color = NA, alpha = 0.25) +
    ggplot2::coord_sf(xlim = c(MAIN_BBOX[["xmin"]], MAIN_BBOX[["xmax"]]),
                      ylim = c(MAIN_BBOX[["ymin"]], MAIN_BBOX[["ymax"]]),
                      expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#9FC3D6", color = NA),
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", color = NA))

  outfile <- file.path(outdir, "D-terrain-check.png")
  ragg::agg_png(outfile, width = 1600, height = 1350, units = "px", res = 150)
  print(g)
  invisible(grDevices::dev.off())
  cat("Wrote", outfile, "\n")
}
