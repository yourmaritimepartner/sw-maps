#!/usr/bin/env Rscript
# mod_mainmap.R  —  reusable MAIN establishing-map module
# TIGER/US Census + rnaturalearth layers parameterized by state/county, with
# office-specific labels supplied by the orchestrator. No Overpass.
#
# Exposes:
#   main_layers(bbox4326) -> named list of sf objects
#       $land        land polygon (rnaturalearth island body carved by TIGER water)
#       $coastline   land boundary as lines
#       $water       TIGER area_water polygons cropped to bbox (harbor / lochs / canal)
#       $major_roads TIGER primary_secondary_roads cropped to bbox
#       $freeways    subset of major_roads that are interstates (H-1/H-201)
#       $parks       optional park polygons (NULL by default; hook for later)
#       $labels      point sf of place labels (Pearl Harbor / Airport / Diamond Head /
#                    Koolau / Downtown), column `name`
#   build_main(bbox4326, office_sf, oceanfill = NULL) -> ggplot
#       Paints oceanfill (if given) as the water base, else falls back to a water-colored
#       panel + TIGER water. Land on top, thin road casings, subtle labels, office PIN.
#
# Geometry stays EPSG:4326; crop via coord_sf(expand = FALSE).

suppressPackageStartupMessages({
  library(sf)
  library(tigris)
  library(rnaturalearth)
  library(ggplot2)
  library(ragg)
  library(systemfonts)
})

options(tigris_use_cache = TRUE, tigris_class = "sf")
suppressMessages(sf::sf_use_s2(FALSE))

`%||%` <- function(a, b) if (is.null(a)) b else a
.nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)

## ---- palette (per contract) --------------------------------------------------
PAL <- list(
  ink        = "#101820",
  yellow     = "#F2C14E",
  white      = "#FFFFFF",
  text2      = "#D9E2E7",
  water      = "#9FC3D6",  # calm blue (light)
  water_deep = "#7FB0C9",
  land       = "#EDE7DB",  # warm neutral panel
  building   = "#C9C2B6",
  park       = "#CBD8C0",
  road_case  = "#FFFFFF",
  road_fill  = "#C9C2B6",
  coast_line = "#8FB0C0"
)

## font: Aptos if present, else Arial/Helvetica (silent fallback)
.pick_font <- function() {
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character(0))
  for (f in c("Aptos", "Arial", "Helvetica")) if (f %in% fams) return(f)
  ""  # ggplot default
}
FONT <- .pick_font()

## default MAIN bbox (tuned for a clean establishing frame)
MAIN_BBOX <- c(xmin = -157.98, ymin = 21.24, xmax = -157.79, ymax = 21.40)

## ---- helpers -----------------------------------------------------------------
.as_bpoly <- function(bbox4326) {
  bb <- bbox4326
  if (is.null(names(bb))) names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  p <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = bb[["xmin"]], ymin = bb[["ymin"]], xmax = bb[["xmax"]], ymax = bb[["ymax"]]),
    crs = 4326))
  p
}

.safe_crop <- function(x, bbox_num) {
  if (is.null(x) || nrow(x) == 0) return(x)
  suppressWarnings(sf::st_crop(sf::st_make_valid(x), bbox_num))
}

## ---- layer builder -----------------------------------------------------------
# Returns named list of sf objects clipped to bbox4326. TIGER + rnaturalearth only.
main_layers <- function(bbox4326 = MAIN_BBOX,
                        state = "Hawaii", county = "Honolulu", year = 2022,
                        labels = NULL) {
  bb <- bbox4326
  if (is.null(names(bb))) names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bpoly   <- .as_bpoly(bb)
  bbox_nm <- sf::st_bbox(bpoly)

  ## --- water: TIGER county area_water (harbors, channels, canals, ocean) ---
  aw    <- sf::st_transform(tigris::area_water(state, county, year = year), 4326)
  water <- .safe_crop(aw, bbox_nm)

  ## --- land: rnaturalearth island body (clean ocean, no offshore county-limit
  ##     gaps) carved by the TIGER water polygons so lochs/harbor read crisply ---
  neL    <- sf::st_transform(
    rnaturalearth::ne_download(scale = 10, type = "land",
                               category = "physical", returnclass = "sf"),
    4326)
  neL    <- sf::st_make_valid(neL)
  island <- suppressWarnings(sf::st_intersection(neL, bpoly))
  island <- sf::st_make_valid(island)
  sf::st_crs(island) <- 4326
  island <- sf::st_union(island)

  wu <- if (.nrow0(water) > 0) {
    u <- sf::st_union(sf::st_make_valid(water)); sf::st_crs(u) <- 4326; u
  } else NULL

  land <- if (!is.null(wu)) sf::st_difference(island, wu) else island
  land <- sf::st_make_valid(land)
  land <- sf::st_sf(name = "land", geometry = sf::st_sfc(land, crs = 4326))

  coastline <- tryCatch({
    cl <- sf::st_boundary(land)
    # drop the bbox frame edges: keep only segments not on the bbox perimeter
    cl
  }, error = function(e) NULL)

  ## --- major roads: TIGER primary/secondary roads for the requested state ---
  psr <- sf::st_transform(tigris::primary_secondary_roads(state, year = year), 4326)
  major_roads <- .safe_crop(psr, bbox_nm)
  freeways <- if (.nrow0(major_roads) > 0)
    major_roads[!is.na(major_roads$RTTYP) & major_roads$RTTYP == "I", ] else major_roads

  ## --- office-specific labels; Hawaii defaults preserve direct module use ---
  if (is.null(labels)) {
    labels <- data.frame(
      name = c("PEARL HARBOR", "HNL AIRPORT", "DOWNTOWN\nHONOLULU",
               "DIAMOND HEAD", "KOʻOLAU RANGE"),
      lon = c(-157.948, -157.923, -157.845, -157.808, -157.860),
      lat = c(21.360, 21.328, 21.322, 21.262, 21.392),
      stringsAsFactors = FALSE)
  }
  labels <- if (inherits(labels, "sf")) {
    sf::st_transform(labels, 4326)
  } else {
    stopifnot(all(c("name", "lon", "lat") %in% names(labels)))
    sf::st_as_sf(labels, coords = c("lon", "lat"), crs = 4326)
  }

  list(
    land        = land,
    coastline   = coastline,
    water       = water,
    major_roads = major_roads,
    freeways    = freeways,
    parks       = NULL,          # optional hook; none fetched to keep the frame clean
    labels      = labels
  )
}

## ---- map builder -------------------------------------------------------------
# oceanfill: optional sf polygon (from Agent A ocean_land()$ocean). If supplied it is
# painted as the water base; otherwise the water-colored panel + TIGER water carry it.
build_main <- function(bbox4326 = MAIN_BBOX, office_sf, oceanfill = NULL,
                       layers = NULL, show_labels = TRUE,
                       title = NULL, subtitle = NULL, terrain = NULL) {
  bb <- bbox4326
  if (is.null(names(bb))) names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  L  <- layers %||% main_layers(bb)

  if (missing(office_sf) || is.null(office_sf)) {
    office_sf <- sf::st_sf(
      name = "Shipwright",
      geometry = sf::st_sfc(sf::st_point(c(-157.8685, 21.3122)), crs = 4326))
  }

  fam <- FONT

  g <- ggplot2::ggplot()

  ## water base -----------------------------------------------------------------
  if (!is.null(oceanfill) && .nrow0(oceanfill) > 0) {
    g <- g + ggplot2::geom_sf(data = sf::st_transform(oceanfill, 4326),
                              fill = PAL$water, color = NA)
  }
  ## land base (opaque) ---------------------------------------------------------
  g <- g + ggplot2::geom_sf(data = L$land, fill = PAL$land, color = NA)

  ## subtle terrain relief, composited OVER the land base (flat pixels match the
  ## land color, ridges tint; sea pixels transparent). See mod_terrain.R.
  if (!is.null(terrain)) g <- g + terrain

  ## water on top: crisp coastline; also covers any terrain sea-edge fringe ------
  if (.nrow0(L$water) > 0) {
    g <- g + ggplot2::geom_sf(data = L$water, fill = PAL$water, color = NA)
  }
  ## coastline stroke (drawn last so the shore edge stays crisp) -----------------
  g <- g + ggplot2::geom_sf(data = L$land, fill = NA,
                            color = PAL$coast_line, linewidth = 0.35)

  ## roads: white casing under thin gray fill -----------------------------------
  if (.nrow0(L$major_roads) > 0) {
    g <- g +
      ggplot2::geom_sf(data = L$major_roads, color = PAL$road_case, linewidth = 1.1) +
      ggplot2::geom_sf(data = L$major_roads, color = PAL$road_fill, linewidth = 0.55)
    if (.nrow0(L$freeways) > 0) {
      g <- g +
        ggplot2::geom_sf(data = L$freeways, color = PAL$road_case, linewidth = 1.5) +
        ggplot2::geom_sf(data = L$freeways, color = "#B8B0A2", linewidth = 0.8)
    }
  }

  ## place labels ---------------------------------------------------------------
  if (isTRUE(show_labels) && !is.null(L$labels) && nrow(L$labels) > 0) {
    lab_xy <- sf::st_coordinates(L$labels)
    labdf  <- data.frame(x = lab_xy[, 1], y = lab_xy[, 2], name = L$labels$name)
    g <- g + ggplot2::geom_text(
      data = labdf, ggplot2::aes(x = x, y = y, label = name),
      family = fam, fontface = "bold", size = 3.1, lineheight = 0.9,
      color = PAL$ink, alpha = 0.72)
  }

  ## office PIN (brand yellow dot, ink outline — no box/crosshair/arrow) ---------
  g <- g +
    ggplot2::geom_sf(data = office_sf, shape = 16, color = PAL$yellow, size = 4)

  ## crop + theme ---------------------------------------------------------------
  g <- g +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]),
                      ylim = c(bb[["ymin"]], bb[["ymax"]]),
                      expand = FALSE) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_family = fam) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = PAL$water, color = NA),
      plot.background  = ggplot2::element_rect(fill = PAL$white, color = NA),
      plot.title       = ggplot2::element_text(face = "bold", size = 20,
                                               color = PAL$ink,
                                               margin = ggplot2::margin(b = 2)),
      plot.subtitle    = ggplot2::element_text(size = 11, color = "#5A6670",
                                               margin = ggplot2::margin(b = 6)),
      plot.margin      = ggplot2::margin(0, 0, 0, 0)
    )
  g
}

## ---- bare render (only when run directly: `Rscript mod_mainmap.R`) -----------
if (sys.nframe() == 0L) {
  outdir <- file.path(Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps")), "outputs/location-maps")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  office <- sf::st_sf(
    name = "Shipwright",
    geometry = sf::st_sfc(sf::st_point(c(-157.8685, 21.3122)), crs = 4326))

  cat("Building main layers (TIGER + rnaturalearth)...\n")
  L <- main_layers(MAIN_BBOX)
  cat(sprintf("  water polys: %d | roads: %d (freeways %d) | labels: %d\n",
              .nrow0(L$water), .nrow0(L$major_roads), .nrow0(L$freeways),
              .nrow0(L$labels)))

  p <- build_main(MAIN_BBOX, office_sf = office, oceanfill = NULL, layers = L)

  outfile <- file.path(outdir, "B-mainmap-check.png")
  ragg::agg_png(outfile, width = 1600, height = 1350, units = "px", res = 150)
  print(p)
  invisible(grDevices::dev.off())
  cat("Wrote", outfile, "\n")
}
