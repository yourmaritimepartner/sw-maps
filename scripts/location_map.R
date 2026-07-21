#!/usr/bin/env Rscript
# =============================================================================
# Shipwright location map — PARAMETERIZED TEMPLATE
# Main establishing map + inset closeup, office marked with a PIN.
# Flat vector cartography (osmdata + TIGER), composed with cowplot, ragg export.
# Reuse for Florida & other offices by editing the PARAMS block only.
# =============================================================================
suppressPackageStartupMessages({
  library(sf); library(ggplot2); library(cowplot); library(ragg); library(svglite)
})

# ---- PALETTE / TYPE (shared) ------------------------------------------------
# NB: named SWPAL (not PAL) because sourced modules define their own global `PAL`
# and would clobber ours, silently dropping keys like `brand`.
SWPAL <- list(
  ink       = "#101820",
  brand     = "#F2C14E",   # marker / accent only
  white     = "#FFFFFF",
  text2     = "#D9E2E7",
  water     = "#9FC3D6",
  water_dp  = "#7FB0C9",
  land      = "#EDE7DB",
  building  = "#C9C2B6",
  bld_edge  = "#8A8577",
  park      = "#CBD8C0"
)
FONT <- local({
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  hit <- c("Calluna Sans", "Aptos", "Arial", "Helvetica")
  hit <- hit[hit %in% fams]
  if (length(hit)) hit[1] else "sans"
})

# ---- PATHS ------------------------------------------------------------------
ROOT    <- path.expand("~/sw-projects/sw-locations")
SCR     <- file.path(ROOT, "scripts")
OUT     <- file.path(ROOT, "outputs/location-maps")
CONFIGS <- file.path(ROOT, "configs")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- OFFICE CONFIG ----------------------------------------------------------
# Per-office PARAMS live in configs/<id>.R (each file defines `office`).
#   Rscript scripts/location_map.R hawaii     (default)
#   Rscript scripts/location_map.R florida
office_id <- { a <- commandArgs(trailingOnly = TRUE); if (length(a) >= 1) a[[1]] else "hawaii" }
cfg <- file.path(CONFIGS, paste0(office_id, ".R"))
if (!file.exists(cfg))
  stop("No config for '", office_id, "'. Available: ",
       paste(sub("\\.R$", "", list.files(CONFIGS, "\\.R$")), collapse = ", "))
source(cfg)                       # defines `office`
stopifnot(is.list(office), !is.null(office$slug))
cat("Building:", office$name, "->", office$slug, "\n")

office_sf <- st_sf(name = office$name,
                   geometry = st_sfc(st_point(c(office$lon, office$lat)), crs = 4326))

# ---- MODULES (produced by subagents A/B/C) ---------------------------------
src_if <- function(f) { p <- file.path(SCR, f); if (file.exists(p)) { source(p); TRUE } else { message("missing module: ", f); FALSE } }
have_ocean   <- src_if("mod_oceanfill.R")   # ocean_land(bbox)
have_main    <- src_if("mod_mainmap.R")     # main_layers(bbox); build_main(bbox, office_sf, ...)
have_inset   <- src_if("mod_inset.R")       # build_inset(office_sf, bbox, harbor, land)
have_terrain <- src_if("mod_terrain.R")     # terrain_layer(bbox, alpha, z)

# ---- ATTRIBUTION + THEME helpers -------------------------------------------
attribution <- "© OpenStreetMap contributors · TIGER/US Census"

# ---- BUILD ------------------------------------------------------------------
# Terrain only where it reads (skip for flat regions via office$use_terrain).
terrain <- if (isTRUE(office$use_terrain) && have_terrain) terrain_layer(office$main_bbox, alpha = 0.35) else NULL
harbor <- readRDS(file.path(OUT, office$osm_rds))
# Detailed coastline land serves the inset and corrects artificial port fill that
# rnaturalearth omits from the establishing map. The coastline is disk-cached.
inset_land <- if (have_ocean) ocean_land(office$inset_bbox)$land else NULL
L <- if (have_main) main_layers(
  office$main_bbox,
  state = office$tiger_state,
  county = office$tiger_county,
  year = office$tiger_year,
  labels = office$labels
) else NULL   # reused by layered export
if (!is.null(L) && !is.null(inset_land) && nrow(inset_land) > 0) {
  detailed <- suppressWarnings(st_crop(inset_land, st_bbox(L$land)))
  if (nrow(detailed) > 0) {
    merged <- suppressWarnings(st_union(c(st_geometry(L$land), st_geometry(detailed))))
    L$land <- st_sf(name = "land", geometry = merged)
  }
}
main <- if (have_main) build_main(office$main_bbox, office_sf, oceanfill = NULL,
                                  terrain = terrain, layers = L) else NULL
inset <- if (have_inset) build_inset(office_sf, office$inset_view, harbor = harbor,
                                     land = inset_land, caption = office$inset_caption) else NULL

# ---- COMPOSE (main fills the frame; inset card + leader inboard) ------------
# Canvas aspect is set to the MAIN map's own aspect so the map fills the frame with
# no pillarbox bars — then title/inset/attribution all sit INSIDE the map edges.
mb   <- office$main_bbox
CANVAS_ASP <- as.numeric(((mb["xmax"] - mb["xmin"]) *
                          cos(mean(c(mb["ymin"], mb["ymax"])) * pi/180)) /
                         (mb["ymax"] - mb["ymin"]))
# office dot position as a fraction of the (full-bleed) map panel:
dot_x <- as.numeric((office$lon - mb["xmin"]) / (mb["xmax"] - mb["xmin"]))
dot_y <- as.numeric((office$lat - mb["ymin"]) / (mb["ymax"] - mb["ymin"]))
# Shared composition geometry (also reused by the layered export).
cardH <- office$inset_height
cardW <- (1.4 / CANVAS_ASP) * cardH
cardX <- office$inset_x
cardY <- office$inset_top - cardH
# Leader terminates at the nearest point on the inset card boundary.
card_target_x <- min(max(dot_x, cardX), cardX + cardW)
card_target_y <- min(max(dot_y, cardY), cardY + cardH)

compose <- function(main, inset) {
  card <- ggdraw(inset) +
    theme(plot.background = element_rect(fill = SWPAL$white, color = SWPAL$ink, linewidth = 1))
  ggdraw() +
    draw_plot(main) +
    draw_line(x = c(dot_x, card_target_x), y = c(dot_y, card_target_y),
              color = SWPAL$ink, linewidth = 0.6) +
    draw_plot(card, x = cardX, y = cardY, width = cardW, height = cardH) +
    # title block, top-left, inboard of the map edge
    draw_label(office$name, x = office$title_x, y = office$title_y,
               hjust = 0, fontface = "bold",
               size = 27, fontfamily = FONT, color = SWPAL$ink) +
    draw_label(office$subtitle, x = office$title_x, y = office$subtitle_y, hjust = 0,
               size = 13, fontfamily = FONT, color = SWPAL$ink) +
    draw_label(attribution, x = 0.985, y = 0.018, hjust = 1, size = 8,
               fontfamily = FONT, color = SWPAL$ink, alpha = 0.65)
}

if (!is.null(main) && !is.null(inset)) {
  final <- compose(main, inset)
  W <- 2400; H <- round(W / CANVAS_ASP); DPI <- 200
  base_out <- file.path(OUT, paste0(office$slug, "-location-map"))

  # PNG (ragg) — final flat render
  png_out <- paste0(base_out, ".png")
  agg_png(png_out, width = W, height = H, units = "px", res = DPI)
  print(final); invisible(dev.off())
  cat("Wrote", png_out, " (", W, "x", H, ")\n")

  # SVG (svglite) — editable text + vector layers for Illustrator.
  # Terrain hillshade embeds as a raster; all else is vector/live text.
  svg_out <- paste0(base_out, ".svg")
  svglite(svg_out, width = W / DPI, height = H / DPI, bg = "white")
  print(final); invisible(dev.off())
  cat("Wrote", svg_out, "\n")

  # PDF (cairo) — opens cleanly in Illustrator (no SVG-Tiny clip warning),
  # text stays live, terrain embeds as a raster. Best for the AI editing pass.
  pdf_out <- paste0(base_out, ".pdf")
  grDevices::cairo_pdf(pdf_out, width = W / DPI, height = H / DPI, bg = "white")
  print(final); invisible(dev.off())
  cat("Wrote", pdf_out, "\n")

  # ---- LAYERED SVG (named <g> groups -> Illustrator layers) -----------------
  if (have_main && !is.null(L)) {
    bb <- office$main_bbox
    # partial main-map plot: only the requested components; panel transparent
    # unless "ocean" (the sea fill) is requested. Styling mirrors build_main.
    mp <- function(components) {
      g <- ggplot()
      if ("water"   %in% components && !is.null(L$water)       && nrow(L$water) > 0)
        g <- g + geom_sf(data = L$water, fill = SWPAL$water, color = NA)
      if ("land"    %in% components) g <- g + geom_sf(data = L$land, fill = SWPAL$land, color = NA)
      if ("terrain" %in% components && !is.null(terrain)) g <- g + terrain
      if ("coast"   %in% components) g <- g + geom_sf(data = L$land, fill = NA, color = "#8FB0C0", linewidth = 0.35)
      if ("roads"   %in% components && !is.null(L$major_roads) && nrow(L$major_roads) > 0) {
        g <- g + geom_sf(data = L$major_roads, color = "#FFFFFF", linewidth = 1.1) +
                 geom_sf(data = L$major_roads, color = "#C9C2B6", linewidth = 0.55)
        if (!is.null(L$freeways) && nrow(L$freeways) > 0)
          g <- g + geom_sf(data = L$freeways, color = "#FFFFFF", linewidth = 1.5) +
                   geom_sf(data = L$freeways, color = "#B8B0A2", linewidth = 0.8)
      }
      if ("marker"  %in% components) g <- g + geom_sf(data = office_sf, shape = 16, color = SWPAL$brand, size = 4)
      if ("labels"  %in% components && !is.null(L$labels) && nrow(L$labels) > 0) {
        xy <- sf::st_coordinates(L$labels)
        df <- data.frame(x = xy[,1], y = xy[,2], name = L$labels$name)
        g <- g + geom_text(data = df, aes(x, y, label = name), family = FONT,
                           fontface = "bold", size = 3.1, color = SWPAL$ink, alpha = 0.72)
      }
      pbg <- if ("ocean" %in% components) SWPAL$water else NA
      g + coord_sf(xlim = bb[c(1,3)], ylim = bb[c(2,4)], expand = FALSE) +
        theme_void(base_family = FONT) +
        theme(panel.background = element_rect(fill = pbg, color = NA),
              plot.background  = element_rect(fill = NA, color = NA),
              plot.margin = margin(0,0,0,0))
    }
    card <- ggdraw(inset) +
      theme(plot.background = element_rect(fill = SWPAL$white, color = SWPAL$ink, linewidth = 1))
    tp <- function(g) g + theme(plot.background = element_rect(fill = NA, color = NA))

    # bottom->top: ocean (sea base) -> land -> [terrain relief] -> water
    # (harbor/lochs/canals + crisp coast, ON TOP of terrain so named waterbodies stay
    # blue — the terrain raster doesn't resolve basins as below sea level and would
    # otherwise paint over them). Matches the flat map's draw order.
    layers <- list(
      ocean = tp(ggdraw() + draw_plot(mp("ocean"), 0, 0, 1, 1)),
      land  = tp(ggdraw() + draw_plot(mp("land"),  0, 0, 1, 1))
    )
    if (!is.null(terrain))
      layers$terrain <- tp(ggdraw() + draw_plot(mp("terrain"), 0, 0, 1, 1))
    layers$water <- tp(ggdraw() + draw_plot(mp(c("water", "coast")), 0, 0, 1, 1))
    layers <- c(layers, list(
      roads   = tp(ggdraw() + draw_plot(mp("roads"), 0, 0, 1, 1)),
      marker  = tp(ggdraw() + draw_plot(mp("marker"), 0, 0, 1, 1) +
                   draw_line(x = c(dot_x, card_target_x), y = c(dot_y, card_target_y),
                             color = SWPAL$ink, linewidth = 0.6)),
      inset   = tp(ggdraw() + draw_plot(card, cardX, cardY, cardW, cardH)),
      text    = tp(ggdraw() + draw_plot(mp("labels"), 0, 0, 1, 1) +
                   draw_label(office$name, x = office$title_x, y = office$title_y,
                              hjust = 0, fontface = "bold",
                              size = 27, fontfamily = FONT, color = SWPAL$ink) +
                   draw_label(office$subtitle, x = office$title_x, y = office$subtitle_y,
                              hjust = 0, size = 13, fontfamily = FONT, color = SWPAL$ink) +
                   draw_label(attribution, x = 0.985, y = 0.018, hjust = 1, size = 8,
                              fontfamily = FONT, color = SWPAL$ink, alpha = 0.65))
    ))

    tmp <- tempfile(fileext = ".svg"); hdr <- NULL; inner <- character(); i <- 0L
    for (nm in names(layers)) {
      i <- i + 1L
      svglite(tmp, width = W/DPI, height = H/DPI, bg = "transparent")
      print(layers[[nm]]); invisible(dev.off())
      x <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
      if (is.null(hdr)) hdr <- sub("(?s)^(.*?<svg[^>]*>).*$", "\\1", x, perl = TRUE)
      body <- sub("(?s)^.*?<svg[^>]*>(.*)</svg>\\s*$", "\\1", x, perl = TRUE)
      body <- sub("<rect width='100\\.00%' height='100\\.00%'[^/]*/>", "", body, perl = TRUE)
      body <- gsub("id='([^']+)'", sprintf("id='L%d_\\1'", i), body, perl = TRUE)
      body <- gsub("url\\(#([^)]+)\\)", sprintf("url(#L%d_\\1)", i), body, perl = TRUE)
      inner <- c(inner, sprintf("<g id='%s'>\n%s\n</g>", nm, body))
    }
    layered <- paste0(hdr, "\n", paste(inner, collapse = "\n"), "\n</svg>\n")
    svgL_out <- paste0(base_out, "-layered.svg")
    writeLines(layered, svgL_out)
    cat("Wrote", svgL_out, " (", length(layers), "layers )\n")
  }
} else {
  cat("Modules not all present yet; scaffold ready. have: ocean=", have_ocean,
      " main=", have_main, " inset=", have_inset, "\n")
}
