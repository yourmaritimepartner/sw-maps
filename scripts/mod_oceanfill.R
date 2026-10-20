#!/usr/bin/env Rscript
# mod_oceanfill.R  --  Agent A deliverable for the Shipwright location-map build.
#
# PROBLEM: In OSM, open ocean and Honolulu Harbor are tagged `natural=coastline`
# (LINES, land-on-the-left), NOT `natural=water` polygons. A naive geom_sf therefore
# paints no sea. This module turns coastline lines into a filled OCEAN polygon for
# ANY bbox.
#
# APPROACH:  ocean = bbox_rectangle  MINUS  land
#   land is assembled from OSM `natural=coastline` ways:
#     1. clip coastline lines to the bbox,
#     2. node (coastline UNION bbox-boundary) and st_polygonize -> closed faces,
#     3. label each face land/ocean using OSM's "land is on the LEFT of the way
#        direction" convention (left/right probe points), with a south-edge sanity
#        check that flips the labelling if the coastline happens to be digitised the
#        other way,
#     4. ocean = bbox - union(land faces).
#   If no coastline is available (fetch fails, or a bbox with no OSM coastline),
#   fall back to rnaturalearth ne_10m land clipped to the bbox.
#
# PUBLIC API:
#   ocean_land(bbox4326, use_cache=TRUE, prefer=c("osm","ne"))
#     -> list(ocean = <sf MULTIPOLYGON>, land = <sf MULTIPOLYGON or NULL>,
#             source = "osm" | "ne")
#   where bbox4326 = c(xmin, ymin, xmax, ymax) in EPSG:4326.
#
# Sourcing this file has NO side effects beyond defining functions and a small
# in-memory cache environment.

suppressPackageStartupMessages({
  library(sf)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Overpass fetch (retry + endpoint failover), copied pattern from proof.R ----
.oceanfill_endpoints <- c(
  "https://overpass-api.de/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)

# module-level cache so the two verify renders (and repeat calls) don't re-hit Overpass
.oceanfill_cache <- new.env(parent = emptyenv())

.of_bbox_key <- function(bb) paste(sprintf("%.5f", bb), collapse = "_")

# Fetch natural=coastline LINES for a bbox. Returns an sfc of LINESTRINGs (crs 4326)
# clipped to the bbox, or NULL on total failure / no data.
of_get_coastline <- function(bbox4326, tries = 6, disk_cache = TRUE) {
  suppressPackageStartupMessages(library(osmdata))
  bb <- c(xmin = bbox4326[1], ymin = bbox4326[2], xmax = bbox4326[3], ymax = bbox4326[4])
  key <- .of_bbox_key(bb)

  # in-memory cache
  if (!is.null(.oceanfill_cache[[key]])) return(.oceanfill_cache[[key]])

  # optional on-disk cache (spares Overpass across process restarts)
  cache_dir <- file.path(Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps")), "outputs/location-maps")
  cache_file <- file.path(cache_dir, paste0("coastline-", key, ".rds"))
  if (disk_cache && file.exists(cache_file)) {
    ln <- readRDS(cache_file)
    if (!length(ln)) return(NULL)            # cached "no coastline in this bbox"
    .oceanfill_cache[[key]] <- ln
    return(ln)
  }

  # NOTE: we deliberately do NOT use osmdata's built-in network call. In this
  # environment osmdata_sf(opq(...)) HANGS indefinitely, whereas a plain curl POST
  # with an explicit User-Agent returns in a few seconds (Overpass 406s requests
  # that lack a UA). So: curl the query to an OSM-XML file, then parse it OFFLINE
  # with osmdata_sf(doc = <file>).
  # Overpass bbox order in the query is (south, west, north, east).
  query <- sprintf(
    '[out:xml][timeout:90];way["natural"="coastline"](%.6f,%.6f,%.6f,%.6f);(._;>;);out body;',
    bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"])
  ua <- "sw-maps-oceanfill/1.0 (Shipwright location map; contact j@djinna.com)"
  # a valid answer can be tiny: open sea has no coastline at all (offshore sites). Reject only
  # non-OSM bodies and partial answers (Overpass reports a timeout as a <remark>).
  osm_status_ok <- function(f) file.exists(f) && file.info(f)$size > 50 &&
    any(grepl("<osm", readLines(f, n = 5, warn = FALSE))) &&
    !any(grepl("<remark>", readLines(f, warn = FALSE)))

  tmpf <- tempfile(fileext = ".osm")
  res <- NULL
  for (i in seq_len(tries)) {
    ep <- sub("/interpreter$", "/interpreter",
              .oceanfill_endpoints[((i - 1) %% length(.oceanfill_endpoints)) + 1])
    rc <- suppressWarnings(system2("curl",
      args = c("-s", "--max-time", "90", "-A", shQuote(ua), "-X", "POST", shQuote(ep),
               "--data-urlencode", shQuote(paste0("data=", query)), "-o", shQuote(tmpf)),
      stdout = TRUE, stderr = TRUE))
    if (osm_status_ok(tmpf)) {
      got <- tryCatch(osmdata_sf(doc = tmpf), error = function(e) e)
      if (!inherits(got, "error")) { res <- got; break }
      cat(sprintf("  [coastline] parse attempt %d failed: %s\n", i, conditionMessage(got)))
    } else {
      cat(sprintf("  [coastline] fetch attempt %d via %s failed (no valid OSM XML)\n",
                  i, sub("https://", "", ep)))
    }
    Sys.sleep(2 * i)
  }
  if (!is.null(tmpf) && file.exists(tmpf)) unlink(tmpf)
  if (is.null(res)) return(NULL)

  ln <- .of_coast_from_osm(res)
  if (is.null(ln)) {
    # no coastline here: remember that on disk too, so renders stay offline (an empty sfc;
    # ocean_land() then falls back to Natural Earth, which says land or sea)
    if (disk_cache) tryCatch({ dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
                                saveRDS(sf::st_sfc(crs = 4326), cache_file) }, error = function(e) NULL)
    return(NULL)
  }
  .oceanfill_cache[[key]] <- ln
  if (disk_cache) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    tryCatch(saveRDS(ln, cache_file), error = function(e) NULL)
  }
  ln
}

# natural=coastline ways from an osmdata_sf object as LINESTRINGs in OSM node order.
# Closed ways -- islands, and long barrier-island rings such as Fort Lauderdale beach --
# come back from osmdata as POLYGONS, not lines; reading only osm_lines dropped them all
# (Harbor Beach was painted as sea). osmdata keeps the node order, so land stays on the left.
.of_coast_from_osm <- function(o) {
  pick <- function(x) {
    if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(NULL)
    if ("natural" %in% names(x)) x <- x[!is.na(x$natural) & x$natural == "coastline", ]
    if (nrow(x)) sf::st_cast(sf::st_geometry(x), "LINESTRING", warn = FALSE) else NULL
  }
  parts <- Filter(Negate(is.null), list(pick(o$osm_lines), pick(o$osm_polygons)))
  if (!length(parts)) NULL else do.call(c, parts)
}

# ---- geometry helpers ----

.of_bbox_poly <- function(bbox4326) {
  bb <- sf::st_bbox(c(xmin = bbox4326[1], ymin = bbox4326[2],
                      xmax = bbox4326[3], ymax = bbox4326[4]), crs = 4326)
  sf::st_as_sfc(bb)
}

# Build left/right probe points from directed coastline segments.
# OSM convention: LAND is on the LEFT of the way direction. Probes are offset a few
# metres perpendicular to each segment; left probes seed land, right probes seed sea.
# Longitudinal offset is scaled by cos(lat) so the offset is isotropic on the ground.
.of_probes <- function(coast_lines, eps_m = 3) {
  eps_deg <- eps_m / 111320  # metres -> degrees latitude
  left <- list(); right <- list()
  geoms <- sf::st_geometry(coast_lines)
  for (g in geoms) {
    m <- sf::st_coordinates(g)[, 1:2, drop = FALSE]
    if (nrow(m) < 2) next
    for (k in seq_len(nrow(m) - 1)) {
      x1 <- m[k, 1]; y1 <- m[k, 2]; x2 <- m[k + 1, 1]; y2 <- m[k + 1, 2]
      latc <- (y1 + y2) / 2
      coslat <- max(cos(latc * pi / 180), 1e-6)
      # direction in metres-ish (scale lon by cos lat)
      dx <- (x2 - x1) * coslat
      dy <- (y2 - y1)
      L <- sqrt(dx * dx + dy * dy)
      if (L == 0) next
      dx <- dx / L; dy <- dy / L
      # left normal = (-dy, dx); convert lon component back out of the cos-lat frame
      nlx <- -dy / coslat; nly <- dx
      mxc <- (x1 + x2) / 2; myc <- (y1 + y2) / 2
      left[[length(left) + 1]]  <- c(mxc + eps_deg * nlx, myc + eps_deg * nly)
      right[[length(right) + 1]] <- c(mxc - eps_deg * nlx, myc - eps_deg * nly)
    }
  }
  mk <- function(lst) {
    if (!length(lst)) return(sf::st_sfc(crs = 4326))
    pts <- do.call(rbind, lst)
    sf::st_sfc(lapply(seq_len(nrow(pts)), function(i) sf::st_point(pts[i, ])), crs = 4326)
  }
  list(left = mk(left), right = mk(right))
}

# Core OSM path: coastline lines -> filled land + ocean polygons for the bbox.
.of_from_coastline <- function(bbox4326, coast_lines) {
  bbp <- .of_bbox_poly(bbox4326)
  s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(s2), add = TRUE)  # planar ops

  # clip coastline to bbox
  coast <- suppressWarnings(sf::st_intersection(coast_lines, bbp))
  coast <- coast[!sf::st_is_empty(coast)]
  if (length(coast) == 0) return(NULL)
  # via MULTILINESTRING: on a mixed set, a direct cast keeps only the FIRST part of each
  # multi-part line, and clipping a ring that leaves and re-enters the bbox makes several
  coast <- suppressWarnings(sf::st_cast(sf::st_cast(coast, "MULTILINESTRING"), "LINESTRING"))

  # node coastline together with the bbox boundary, then polygonize into faces
  boundary <- sf::st_cast(bbp, "LINESTRING")
  polyfaces <- function(blade) {
    f <- sf::st_collection_extract(sf::st_polygonize(blade), "POLYGON")
    suppressWarnings(sf::st_cast(f, "POLYGON", warn = FALSE))
  }
  faces <- polyfaces(sf::st_union(c(coast, boundary)))
  if (length(faces) == 0) return(NULL)

  # classify each face via left(land)/right(sea) probe containment
  pr <- .of_probes(coast)
  hits <- function(faces) list(
    land = if (length(pr$left))  lengths(sf::st_intersects(faces, pr$left))  else rep(0, length(faces)),
    sea  = if (length(pr$right)) lengths(sf::st_intersects(faces, pr$right)) else rep(0, length(faces)))
  h <- hits(faces)
  # A face holding plenty of BOTH land and sea probes means the coastline never split the
  # bbox: a clipped line's endpoint sits only approximately on the boundary and missed it
  # (Seattle's waterfront). Then node the UNCLIPPED ways against the boundary so GEOS
  # computes the crossings itself, and keep the faces inside the bbox. Only on failure, so
  # maps whose split already worked (the offices) are untouched.
  mixed <- pmin(h$land, h$sea) >= 3 & pmin(h$land, h$sea) >= 0.2 * pmax(h$land, h$sea)
  if (any(mixed)) {
    full <- coast_lines[lengths(sf::st_intersects(coast_lines, bbp)) > 0]
    f2 <- polyfaces(sf::st_union(c(suppressWarnings(sf::st_cast(full, "LINESTRING", warn = FALSE)), boundary)))
    if (length(f2)) f2 <- f2[lengths(sf::st_within(suppressWarnings(sf::st_point_on_surface(f2)), bbp)) > 0]
    if (length(f2)) { faces <- f2; h <- hits(faces) }
  }
  land_hits <- h$land; sea_hits <- h$sea
  is_land <- land_hits > sea_hits

  # OSM coastlines are validated to keep land on the left, so the probes decide. (This
  # used to force the face at the bbox's south-edge midpoint to be sea and invert the
  # whole labelling otherwise, which is wrong wherever the sea lies north or west of a
  # site: Seattle, Maine, any north shore.) Faces where the probes tie -- typically
  # no probe landed in them -- are settled by Natural Earth land at a point inside.
  tie <- which(land_hits == sea_hits)
  if (length(tie)) {
    ne <- tryCatch(.of_from_ne(bbox4326)$land, error = function(e) NULL)
    for (i in tie) {
      p <- suppressWarnings(sf::st_point_on_surface(faces[i]))
      is_land[i] <- !is.null(ne) && lengths(sf::st_intersects(p, ne))[1] > 0
    }
  }

  land <- if (any(is_land)) sf::st_union(faces[is_land]) else NULL
  if (!is.null(land)) land <- sf::st_make_valid(land)
  ocean <- if (!is.null(land)) sf::st_difference(bbp, land) else bbp
  ocean <- sf::st_make_valid(ocean)

  list(
    ocean = sf::st_sf(kind = "ocean", geometry = sf::st_make_valid(sf::st_geometry(ocean))),
    land  = if (!is.null(land)) sf::st_sf(kind = "land", geometry = sf::st_geometry(land)) else NULL
  )
}

# Fallback path: rnaturalearth ne_10m land clipped to bbox (coarse; no harbor detail).
.of_from_ne <- function(bbox4326) {
  suppressPackageStartupMessages(library(rnaturalearth))
  bbp <- .of_bbox_poly(bbox4326)
  s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(s2), add = TRUE)
  land_full <- tryCatch(
    rnaturalearth::ne_download(scale = 10, type = "land", category = "physical",
                               returnclass = "sf"),
    error = function(e) tryCatch(
      rnaturalearth::ne_countries(scale = 10, returnclass = "sf"),
      error = function(e2) NULL)
  )
  if (is.null(land_full)) {
    # no land data at all -> whole bbox is ocean
    return(list(ocean = sf::st_sf(kind = "ocean", geometry = bbp), land = NULL))
  }
  land_full <- sf::st_make_valid(sf::st_geometry(land_full))
  land <- suppressWarnings(sf::st_intersection(land_full, bbp))
  land <- land[!sf::st_is_empty(land)]
  if (length(land) == 0) {
    return(list(ocean = sf::st_sf(kind = "ocean", geometry = bbp), land = NULL))
  }
  land <- sf::st_make_valid(sf::st_union(land))
  ocean <- sf::st_make_valid(sf::st_difference(bbp, land))
  list(
    ocean = sf::st_sf(kind = "ocean", geometry = sf::st_geometry(ocean)),
    land  = sf::st_sf(kind = "land", geometry = sf::st_geometry(land))
  )
}

# ---- PUBLIC ENTRY POINT ----
# bbox4326 = c(xmin, ymin, xmax, ymax) in EPSG:4326.
ocean_land <- function(bbox4326, use_cache = TRUE, prefer = c("osm", "ne")) {
  prefer <- match.arg(prefer)
  stopifnot(length(bbox4326) == 4)
  bbox4326 <- as.numeric(bbox4326)

  if (prefer == "ne") {
    out <- .of_from_ne(bbox4326)
    out$source <- "ne"
    return(out)
  }

  coast <- tryCatch(of_get_coastline(bbox4326, disk_cache = use_cache),
                    error = function(e) { cat("coastline fetch error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(coast) && length(coast) > 0) {
    out <- tryCatch(.of_from_coastline(bbox4326, coast),
                    error = function(e) { cat("coastline assembly error:", conditionMessage(e), "\n"); NULL })
    if (!is.null(out) && !is.null(out$ocean)) {
      out$source <- "osm"
      return(out)
    }
  }
  cat("ocean_land: falling back to rnaturalearth land.\n")
  out <- .of_from_ne(bbox4326)
  out$source <- "ne"
  out
}
