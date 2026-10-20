#!/usr/bin/env Rscript
# New work site, in steps the web app runs in the background (each writes status.json):
#   Rscript scripts/new_site.R locate <draft>            geocode, fetch OSM around it, rank pin candidates, preview.png
#   Rscript scripts/new_site.R repin  <draft> <lon> <lat> re-rank around a clicked/typed point (no network)
#   Rscript scripts/new_site.R build  <draft> <pick>      pick = candidate rank or "point"; writes configs/<id>.R,
#                                                         caches OSM, runs scripts/check_site.R, renders B · Drawing
#   Rscript scripts/new_site.R refresh <site id>          re-fetch a site's inset OSM and re-run its checks
# A draft lives in data/drafts/<draft>/ and starts as input.json: {"title", "where", "preset"}.
# From the shell:  Rscript scripts/new_site.R new "<title>" "<address or lat, lon>" <site|harbor|region>
#                  (creates the draft, locates, and prints its id)
suppressPackageStartupMessages({ library(sf); library(jsonlite); library(ggplot2) })
suppressMessages(sf_use_s2(FALSE))
ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
for (m in c("mod_oceanfill.R", "mod_inset.R", "mod_site.R")) source(file.path(ROOT, "scripts", m))
args <- commandArgs(trailingOnly = TRUE)
cmd <- args[1]

if (cmd == "new") {
  tok <- paste(sample(c(0:9, letters[1:6]), 10, TRUE), collapse = "")
  dir.create(file.path(ROOT, "data/drafts", tok), recursive = TRUE)
  writeLines(toJSON(list(title = args[2], where = args[3], preset = args[4]), auto_unbox = TRUE),
             file.path(ROOT, "data/drafts", tok, "input.json"))
  cat(tok, "\n"); cmd <- "locate"; args <- c("locate", tok)
}
tok <- args[2]
if (cmd == "refresh") {             # Rscript scripts/new_site.R refresh <site id>
  stopifnot(file.exists(file.path(ROOT, "configs", paste0(tok, ".R"))))
  D <- tempfile("refresh"); dir.create(D)
  writeLines('{"title":"","where":"","preset":"harbor"}', file.path(D, "input.json"))
} else {
  stopifnot(grepl("^[a-f0-9]{10}$", tok))
  D <- file.path(ROOT, "data/drafts", tok)
}
dpath <- function(f) file.path(D, f)
status <- function(state, step = "", message = "", ...) {
  s <- list(state = state, step = step, message = message, pid = Sys.getpid(),
            updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), ...)
  writeLines(toJSON(s, auto_unbox = TRUE), paste0(dpath("status.json"), ".tmp"))
  file.rename(paste0(dpath("status.json"), ".tmp"), dpath("status.json"))
  if (nzchar(step)) message(step)
}
input <- fromJSON(dpath("input.json"))

# Hanken Grotesk for the preview too
FD <- file.path(ROOT, "fonts")
if (!"Hanken Grotesk" %in% systemfonts::system_fonts()$family)
  systemfonts::register_font("Hanken Grotesk", file.path(FD, "HankenGrotesk-Regular.ttf"),
                             file.path(FD, "HankenGrotesk-Bold.ttf"))
source(file.path(ROOT, "styles", "drawing.R"))   # the preview borrows B · Drawing's palette
C <- style$col

# ---- preview: footprints around the pin, candidates numbered, for confirmation --------
# Drawn in lon/lat on a bbox fit to the image's aspect, so the page can turn a click on
# the image back into coordinates (draft.json keeps the bbox).
PREV_W <- 1200; PREV_H <- 800
# Number labels beside each candidate's pin, greedily moved off each other (no ggrepel on the VM).
label_spots <- function(cands, bb) {
  px <- (cands$lon - bb[[1]]) / (bb[[3]] - bb[[1]]) * PREV_W
  py <- (cands$lat - bb[[2]]) / (bb[[4]] - bb[[2]]) * PREV_H
  dirs <- list(c(0, 1), c(1, 0), c(-1, 0), c(0, -1), c(1, 1), c(-1, 1), c(1, -1), c(-1, -1), c(0, 2), c(2, 0))
  took <- matrix(numeric(), 0, 2); out <- matrix(NA_real_, nrow(cands), 2)
  for (i in seq_len(nrow(cands))) {
    for (d in dirs) {
      q <- c(px[i], py[i]) + d * 26
      clear <- !nrow(took) || all(abs(took[, 1] - q[1]) > 26 | abs(took[, 2] - q[2]) > 26)
      if (clear && all(abs(px - q[1]) > 14 | abs(py - q[2]) > 14)) break
    }
    out[i, ] <- q; took <- rbind(took, q)
  }
  data.frame(x = bb[[1]] + out[, 1] / PREV_W * (bb[[3]] - bb[[1]]),
             y = bb[[2]] + out[, 2] / PREV_H * (bb[[4]] - bb[[2]]), rank = cands$rank)
}
preview <- function(loc, cands, sel, geo_pt, pin, bb = NULL) {
  pts <- rbind(c(pin[["lon"]], pin[["lat"]]), if (nrow(cands)) cbind(cands$lon, cands$lat),
               if (!is.null(geo_pt)) c(geo_pt[["lon"]], geo_pt[["lat"]]))
  if (is.null(bb)) {
    cx <- mean(range(pts[, 1])); cy <- mean(range(pts[, 2])); k <- cos(cy * pi / 180)
    w <- max(diff(range(pts[, 1])) * k * m_per_deg_lat * 1.5, 260)
    h <- max(diff(range(pts[, 2])) * m_per_deg_lat * 1.5, 260 / 1.5)
    bb <- fit_aspect(bbox_around(cx, cy, w, h), PREV_W / PREV_H)
  }
  bbn <- as.numeric(bb)
  f <- loc$features
  land <- if (length(loc$coast))
    tryCatch(.of_from_coastline(bbn, loc$coast)$land, error = function(e) NULL)
  # no coastline near: at sea or inland? Natural Earth decides (offshore sites)
  sea_bg <- !is.null(land) || {
    ne <- tryCatch(.of_from_ne(bbn)$land, error = function(e) NULL)
    is.null(ne) || !lengths(st_intersects(st_sfc(st_point(c(pin[["lon"]], pin[["lat"]])), crs = 4326), ne))[1]
  }
  poly <- function(sel) { x <- f[f$geomtype == "polygon" & sel, ]; if (nrow(x)) x else NULL }
  water <- poly(!is.na(f$natural) & f$natural == "water")
  piers <- poly(!is.na(f$man_made) & f$man_made %in% c("pier", "quay", "breakwater", "offshore_platform"))
  bld   <- poly(!is.na(f$building) & f$building != "no")
  # an sf background layer always, so coord_sf keeps the lon/lat aspect even with no OSM features
  frame <- st_sf(geometry = st_as_sfc(st_bbox(bb, crs = 4326)))
  p <- ggplot() + geom_sf(data = frame, fill = if (sea_bg) C$water else C$land, colour = NA)
  if (!is.null(land)) p <- p + geom_sf(data = land, fill = C$land, colour = NA)
  if (!is.null(water)) p <- p + geom_sf(data = water, fill = C$water, colour = NA)
  if (!is.null(piers)) p <- p + geom_sf(data = piers, fill = C$building, colour = C$building_edge, linewidth = 0.2)
  if (!is.null(bld))   p <- p + geom_sf(data = bld, fill = C$building, colour = C$building_edge, linewidth = 0.25)
  cf <- cands[cands$footprint, ]
  if (nrow(cf)) {
    g <- f[match(cf$osm, f$osm), ]
    p <- p + geom_sf(data = g, fill = ifelse(cf$rank == sel, C$target_fill, NA), colour = C$accent,
                     linewidth = ifelse(cf$rank == sel, 1.1, 0.6))
  }
  if (!is.null(geo_pt)) p <- p + annotate("point", x = geo_pt[["lon"]], y = geo_pt[["lat"]], shape = 21,
                                          size = 3.4, stroke = 1, colour = C$ink, fill = "white")
  cp <- cands[!cands$footprint, ]       # point candidates (no footprint): a ring
  if (nrow(cp)) p <- p + annotate("point", x = cp$lon, y = cp$lat, shape = 21, size = 3, stroke = 0.9,
                                  colour = C$accent, fill = NA)
  if (nrow(cands)) p <- p + shadowtext::geom_shadowtext(data = label_spots(cands, bb), aes(x = x, y = y, label = rank),
      family = "Hanken Grotesk", fontface = "bold", size = 4.6, colour = C$accent, bg.colour = "white", bg.r = 0.18)
  p <- p + annotate("point", x = pin[["lon"]], y = pin[["lat"]], shape = 21, size = 3.6, stroke = 0.9,
                    fill = C$accent, colour = "white")
  # 50 / 100 / 200 m bar, bottom right
  gw <- (bb[[3]] - bb[[1]]) * cos(mean(bb[c(2, 4)]) * pi / 180) * m_per_deg_lat
  sb <- c(50, 100, 200, 500)[findInterval(gw * 0.18, c(0, 100, 200, 500))]
  sbd <- sb / gw * (bb[[3]] - bb[[1]]); x1 <- bb[[3]] - 0.04 * (bb[[3]] - bb[[1]]); y1 <- bb[[2]] + 0.05 * (bb[[4]] - bb[[2]])
  p <- p + annotate("segment", x = x1 - sbd, xend = x1, y = y1, yend = y1, colour = C$ink, linewidth = 0.6) +
    shadowtext::geom_shadowtext(data = data.frame(x = x1 - sbd / 2, y = y1, label = paste(sb, "m")),
      aes(x = x, y = y, label = label), family = "Hanken Grotesk", size = 3.4, colour = C$ink,
      bg.colour = "white", bg.r = 0.15, vjust = -0.6) +
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE, datum = NA) +
    theme_void() + theme(panel.background = element_rect(fill = if (sea_bg) C$water else C$land, colour = NA),
                         plot.margin = margin(0, 0, 0, 0))
  ragg::agg_png(dpath("preview.png"), width = PREV_W, height = PREV_H, res = 150, background = "white")
  print(p); invisible(dev.off())
  bb
}

save_draft <- function(d) writeLines(toJSON(d, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null"), dpath("draft.json"))
cands_df <- function(x) if (is.null(x) || !length(x)) data.frame() else as.data.frame(x, stringsAsFactors = FALSE)

run <- function() {
  if (cmd == "locate") {
    status("running", "Finding the place")
    pc <- parse_coords(input$where)
    geo <- NULL; county <- NULL
    if (is.null(pc)) {
      geo <- census_geocode(input$where)
      if (is.null(geo)) {
        status("error", message = sprintf(paste(
          "The Census geocoder found no match for \u201c%s\u201d. Check the street number and name, or enter",
          "coordinates instead (for example 47.6116, -122.3491, from Google Maps: right-click the building)."), input$where))
        return(invisible())
      }
      lon <- geo$lon; lat <- geo$lat; county <- geo$county
    } else {
      lon <- pc[["lon"]]; lat <- pc[["lat"]]
      county <- census_county(lon, lat)
    }
    status("running", "Fetching OpenStreetMap buildings around it (can take a minute when OSM is busy)")
    loc <- site_locate(lon, lat, input$title, geo, cache = dpath("locate.osm"))
    cands <- loc$candidates
    pin <- if (nrow(cands)) c(lon = cands$lon[1], lat = cands$lat[1]) else c(lon = lon, lat = lat)
    status("running", "Drawing the preview")
    sel <- if (!nrow(cands) || (!is.null(pc) && !startsWith(cands$reason[1], "contains"))) 0 else 1
    if (sel == 0) pin <- c(lon = lon, lat = lat)
    bb <- preview(loc, cands, sel, c(lon = lon, lat = lat), pin)
    save_draft(list(input = input, entered = list(lon = lon, lat = lat, coords = !is.null(pc)),
                    geocode = geo, county = county, candidates = cands,
                    # typed coordinates win unless a footprint contains them; 0 = keep the point
                    selected = if (!nrow(cands) || (!is.null(pc) && !startsWith(cands$reason[1], "contains"))) 0 else 1,
                    preview_bbox = as.list(bb), preview_size = c(PREV_W, PREV_H)))
    status("ready", message = if (nrow(cands)) "" else "No building footprint nearby; the pin stays at the point.")
  }

  if (cmd == "repin") {
    status("running", "Moving the pin")
    d <- fromJSON(dpath("draft.json"))
    lon <- as.numeric(args[3]); lat <- as.numeric(args[4])
    o <- suppressWarnings(osmdata::osmdata_sf(doc = dpath("locate.osm")))
    feats <- osm_features(o)
    coast <- .of_coast_from_osm(o)
    cands <- rank_candidates(feats, lon, lat, d$input$title, NULL, coords_input = TRUE, radius = 60, max_n = 4)
    if (!nrow(cands)) cands <- NULL
    if (is.null(cands) || !startsWith(cands$reason[1], "contains the coordinates")) {
      # clicked off any footprint: the pin goes exactly where they clicked
      cands <- rbind(data.frame(rank = 0, osm = "", name = "", kind = "point (no footprint)", footprint = FALSE,
                                lon = lon, lat = lat, dist_m = 0, reason = "where you clicked"), cands)
      cands$rank <- seq_len(nrow(cands))
    }
    bb <- unlist(d$preview_bbox)
    preview(list(features = feats, coast = coast), cands, 1,
            if (!is.null(d$geocode)) c(lon = d$entered$lon, lat = d$entered$lat) else NULL,
            c(lon = cands$lon[1], lat = cands$lat[1]), bb = bb)
    d$candidates <- cands; d$selected <- 1; d$clicked <- list(lon = lon, lat = lat)
    save_draft(d)
    status("ready")
  }

  if (cmd == "refresh") {
    # re-fetch an existing site's inset OSM (same bbox, same cache file) and re-check it
    id <- tok; source(file.path(ROOT, "configs", paste0(id, ".R")))
    site_fetch_inset(id, office$inset_bbox)
    if (!is.null(office$coast_bbox)) {      # and the main map's coastline
      f <- file.path(SITE_CACHE, paste0("coastline-", .of_bbox_key(office$coast_bbox), ".rds"))
      if (file.exists(f)) file.rename(f, paste0(f, ".old"))
      ok <- !is.null(tryCatch(of_get_coastline(as.numeric(office$coast_bbox)), error = function(e) NULL)) || file.exists(f)
      if (ok) unlink(paste0(f, ".old")) else file.rename(paste0(f, ".old"), f)   # keep the old one if the fetch failed
      message("main-map coastline ", if (ok) "refreshed" else "kept (fetch failed)")
    }
    ck <- system2("Rscript", c(file.path(ROOT, "scripts/check_site.R"), id, "--json",
                               file.path(ROOT, "data/sites", paste0(id, "-checks.json"))), stdout = TRUE, stderr = TRUE)
    message(paste(ck, collapse = "\n"))
    return(invisible())
  }

  if (cmd == "build") {
    d <- fromJSON(dpath("draft.json"))
    cands <- cands_df(d$candidates)
    pick <- args[3]
    if (pick == "point" || !nrow(cands)) {
      pin <- c(lon = d$entered$lon, lat = d$entered$lat); target <- ""; tname <- ""; fp <- NULL
      snap <- if (isTRUE(d$entered$coords)) "at the coordinates entered" else "at the geocoder's street point (not snapped)"
    } else {
      cd <- cands[as.integer(pick), ]
      pin <- c(lon = cd$lon, lat = cd$lat); target <- if (cd$footprint) cd$osm else ""; tname <- cd$name
      snap <- sprintf("%s %s%s (%s)", if (cd$footprint) "snapped to" else "at", cd$osm,
                      if (nzchar(cd$name)) paste0(" \u201c", cd$name, "\u201d") else "", cd$reason)
      fp <- NULL
      if (cd$footprint) {
        o <- suppressWarnings(osmdata::osmdata_sf(doc = dpath("locate.osm")))
        f <- osm_features(o); fp <- f[f$osm == cd$osm, ]
      }
    }
    preset <- d$input$preset
    id <- unique_id(d$input$title)
    status("building", "Working out the map extents", id = id)
    ext <- site_extents(pin[["lon"]], pin[["lat"]], preset, fp)
    status("building", "Finding the TIGER counties under the map", id = id)
    counties <- site_counties(ext$coast_bbox, pin[["lon"]], pin[["lat"]])
    status("building", "Fetching OpenStreetMap detail for the inset", id = id)
    rds <- site_fetch_inset(id, ext$inset_bbox)
    status("building", "Fetching the OSM coastline for the main map", id = id)
    ol <- ocean_land(ext$coast_bbox)
    if (ol$source != "osm") message("main map: no OSM coastline in the extent, Natural Earth land")
    status("building", "Picking place labels", id = id)
    labs <- site_labels(ext$main_bbox, NULL, pin[["lon"]], pin[["lat"]], preset, ext$inset_view)
    status("building", "Checking the terrain", id = id)
    relief <- site_relief(ext$main_bbox)
    geo <- d$geocode
    # offshore = the pin is at sea (county lines run 3 nmi out, so "in a county" says little)
    offshore <- is.null(d$county) || !length(d$county) || {
      ol_i <- ocean_land(ext$inset_bbox)
      lengths(st_intersects(pt_sf(pin[["lon"]], pin[["lat"]]), ol_i$ocean))[1] > 0 && is.null(geo)
    }
    subtitle <- if (!is.null(geo) && length(geo)) pretty_address(geo$matched)
                else if (offshore) paste0("Offshore, ", if (length(d$county)) "" else "near ", counties$home_name)
                else counties$home_name
    caption <- inset_caption(tname, d$input$title, offshore)
    s <- list(id = id, title = d$input$title, added_by = d$input$who %||% "", subtitle = subtitle, lon = pin[["lon"]], lat = pin[["lat"]],
              target_osm = target, geocoded = if (!is.null(geo) && length(geo)) c(lon = geo$lon, lat = geo$lat),
              preset = preset, ext = ext, osm_rds = rds, caption = caption,
              use_terrain = !is.na(relief) && relief > 150, relief = relief, counties = counties, labels = labs,
              input_note = sprintf("\u201c%s\u201d%s", d$input$where,
                                   if (!is.null(geo) && length(geo)) sprintf(" -> Census: %s", geo$matched) else ""),
              pin_note = snap)
    path <- write_site_config(s)
    message("wrote ", path)
    status("building", "Checking the pin and the water with tools", id = id)
    ck <- system2("Rscript", c(file.path(ROOT, "scripts/check_site.R"), id, "--json",
                               file.path(ROOT, "data/sites", paste0(id, "-checks.json"))), stdout = TRUE, stderr = TRUE)
    message(paste(ck, collapse = "\n"))
    status("building", "Rendering B \u00b7 Drawing at report size (about 30 s)", id = id)
    r <- system2("Rscript", c(file.path(ROOT, "scripts/comp_map.R"), id, "drawing", "report", "outputs/maps"),
                 stdout = TRUE, stderr = TRUE)
    message(paste(tail(r, 5), collapse = "\n"))
    if (!is.null(attr(r, "status")) && attr(r, "status") != 0) stop("The render failed: ", paste(tail(r, 6), collapse = "\n"))
    status("done", id = id)
  }
}
tryCatch(run(), error = function(e) {
  message("ERROR: ", conditionMessage(e))
  status("error", message = conditionMessage(e))
  quit(status = 1)
})
