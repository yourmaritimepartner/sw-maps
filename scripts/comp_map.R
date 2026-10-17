#!/usr/bin/env Rscript
# =============================================================================
# Design comps: one office x one style x one size, laid out at TRUE PRINT SIZE so
# type and strokes are specified in real points. Data layers come from the same
# modules as location_map.R; the composition is new: fit-to-size extent, automatic
# inset placement (corner covering the least land), extent box + connectors,
# km / nmi scale bar, north arrow, label classes with halos.
#   Rscript scripts/comp_map.R hawaii platform report
# Styles live in styles/<id>.R (each defines `style`). Output: outputs/comps/.
# =============================================================================
suppressPackageStartupMessages({
  library(sf); library(ggplot2); library(cowplot); library(ragg); library(shadowtext)
})
suppressMessages(sf::sf_use_s2(FALSE))

ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
args <- commandArgs(trailingOnly = TRUE)
office_id <- if (length(args) >= 1) args[[1]] else "hawaii"
style_id  <- if (length(args) >= 2) args[[2]] else "platform"
size_id   <- if (length(args) >= 3) args[[3]] else "report"

SIZES <- list(                       # final printed size, inches
  report = c(w = 6.5,    h = 4.6),   # full-width report figure
  half   = c(w = 3.15,   h = 3.6),   # half-width / two-up
  slide  = c(w = 13.333, h = 7.5)    # 16:9 slide
)
DPI <- 300

# ---- fonts (OFL, committed in fonts/) ---------------------------------------
FD <- file.path(ROOT, "fonts")
systemfonts::register_font("SW Sans",
  file.path(FD, "HankenGrotesk-Regular.ttf"), file.path(FD, "HankenGrotesk-SemiBold.ttf"),
  file.path(FD, "HankenGrotesk-Italic.ttf"),  file.path(FD, "HankenGrotesk-SemiBoldItalic.ttf"))
systemfonts::register_font("SW Mono",
  file.path(FD, "IBMPlexMono-Regular.ttf"), file.path(FD, "IBMPlexMono-Medium.ttf"),
  file.path(FD, "IBMPlexMono-Regular.ttf"), file.path(FD, "IBMPlexMono-Medium.ttf"))
systemfonts::register_font("SW Serif",
  file.path(FD, "LibreCaslonText-Regular.ttf"), file.path(FD, "LibreCaslonText-Bold.ttf"),
  file.path(FD, "LibreCaslonText-Italic.ttf"),  file.path(FD, "LibreCaslonText-Bold.ttf"))

# ---- config, style, data modules --------------------------------------------
source(file.path(ROOT, "configs", paste0(office_id, ".R")))     # -> office
source(file.path(ROOT, "styles",  paste0(style_id,  ".R")))     # -> style
for (m in c("mod_oceanfill.R", "mod_mainmap.R", "mod_inset.R", "mod_terrain.R"))
  source(file.path(ROOT, "scripts", m))
C  <- style$col
FT <- style$fonts

S  <- SIZES[[size_id]]; W <- S[["w"]]; H <- S[["h"]]
ts <- min(max(W / 6.5, 1), 1.6)                     # type scale for larger formats
pt <- function(p) p * ts / ggplot2::.pt             # points -> ggplot text size
lw <- function(p) p * ts / 0.75 / ggplot2::.pt      # points -> ggplot linewidth

# ---- page geometry (inches) -------------------------------------------------
hh <- if (style$title_mode == "header") 0.66 * ts else 0     # header strip above map
bh <- if (style$title_mode == "block")  0.58 * ts else 0     # title block below map
map_w <- W; map_h <- H - hh - bh
map_y0 <- bh / H; map_hf <- map_h / H                        # map box in canvas fractions

# Grow the configured extent around its centre to the map box's aspect, so the map
# always fills the frame at any size (replaces the per-office canvas tuning).
fit_bbox <- function(bb, asp) {
  cx <- mean(bb[c(1, 3)]); cy <- mean(bb[c(2, 4)]); k <- cos(cy * pi / 180)
  gw <- (bb[[3]] - bb[[1]]) * k; gh <- bb[[4]] - bb[[2]]
  if (gw / gh < asp) gw <- gh * asp else gh <- gw / asp
  c(xmin = cx - gw / k / 2, ymin = cy - gh / 2, xmax = cx + gw / k / 2, ymax = cy + gh / 2)
}
bb <- fit_bbox(office$main_bbox, map_w / map_h)
dx <- bb[["xmax"]] - bb[["xmin"]]; dy <- bb[["ymax"]] - bb[["ymin"]]
kx <- cos(mean(bb[c(2, 4)]) * pi / 180)
m_per_deg_x <- 111320 * kx
ground_w_m  <- dx * m_per_deg_x
fx <- function(lon) (lon - bb[["xmin"]]) / dx
fy <- function(lat) (lat - bb[["ymin"]]) / dy
ilon <- function(f) bb[["xmin"]] + f * dx
ilat <- function(f) bb[["ymin"]] + f * dy

# ---- layers ------------------------------------------------------------------
office_sf <- st_sf(geometry = st_sfc(st_point(c(office$lon, office$lat)), crs = 4326))
L <- main_layers(bb, state = office$tiger_state, county = office$tiger_county,
                 year = office$tiger_year, labels = office$labels)
inset_land <- ocean_land(office$inset_bbox)$land
if (!is.null(inset_land) && nrow(inset_land) > 0) {
  detailed <- suppressWarnings(st_crop(inset_land, st_bbox(L$land)))
  if (nrow(detailed) > 0)
    L$land <- st_sf(name = "land", geometry = suppressWarnings(
      st_union(c(st_geometry(L$land), st_geometry(detailed)))))
}
harbor  <- readRDS(file.path(ROOT, "outputs/location-maps", office$osm_rds))
terrain <- if (isTRUE(style$terrain$on) && isTRUE(office$use_terrain))
  terrain_layer(bb, alpha = style$terrain$alpha, land = C$land,
                shadow = style$terrain$shadow, light = style$terrain$light) else NULL

# ---- automatic placement -----------------------------------------------------
land_planar <- st_set_crs(st_geometry(L$land), NA)
rect_planar <- function(r) st_as_sfc(st_bbox(c(xmin = ilon(r[1]), ymin = ilat(r[2]),
                                               xmax = ilon(r[3]), ymax = ilat(r[4]))))
land_frac <- function(r) {
  p <- rect_planar(r)
  i <- suppressWarnings(st_intersection(land_planar, p))
  if (!length(i)) 0 else sum(st_area(i)) / st_area(p)
}
overlaps <- function(r, q, pad = 0)
  !(r[3] + pad < q[1] || q[3] + pad < r[1] || r[4] + pad < q[2] || q[4] + pad < r[2])
mxf <- 0.14 / map_w; myf <- 0.14 / map_h                      # corner margins
corner_rect <- function(cn, wf, hf) switch(cn,
  bl = c(mxf, myf, mxf + wf, myf + hf),         br = c(1 - mxf - wf, myf, 1 - mxf, myf + hf),
  tl = c(mxf, 1 - myf - hf, mxf + wf, 1 - myf), tr = c(1 - mxf - wf, 1 - myf - hf, 1 - mxf, 1 - myf))
place <- function(wf, hf, avoid = list(), corners = c("bl", "br", "tl", "tr"), near = NULL) {
  best <- NULL; bs <- Inf
  for (cn in corners) {
    r <- corner_rect(cn, wf, hf)
    if (any(vapply(avoid, function(q) overlaps(r, q, 0.015), TRUE))) next
    s <- land_frac(r)
    if (!is.null(near)) s <- s + 0.6 * sqrt((mean(r[c(1, 3)]) - near[1])^2 + ((mean(r[c(2, 4)]) - near[2]) * map_h / map_w)^2)
    if (nzchar(Sys.getenv("COMP_DEBUG"))) message(sprintf("place %s: score %.3f (land %.3f)", cn, s, land_frac(r)))
    if (s < bs) { bs <- s; best <- list(corner = cn, r = r) }
  }
  best
}

iv  <- office$inset_view
ivr <- c(fx(iv[["xmin"]]), fy(iv[["ymin"]]), fx(iv[["xmax"]]), fy(iv[["ymax"]]))
pin <- c(fx(office$lon), fy(office$lat))
pin_r <- c(pin[1] - 0.05, pin[2] - 0.06, pin[1] + 0.05, pin[2] + 0.06)
iasp  <- ((iv[["xmax"]] - iv[["xmin"]]) * cos(office$lat * pi / 180)) / (iv[["ymax"]] - iv[["ymin"]])
card_w <- (if (size_id == "half") 0.46 else 0.34) * map_w
card_h <- min(card_w / iasp, 0.5 * map_h); card_w <- card_h * iasp
# try the card at full size, then smaller, so a near corner can win over a far one
inset_pl <- NULL
for (k in c(1, 0.85, 0.72)) {
  cand <- place(k * card_w / map_w, k * card_h / map_h, avoid = list(ivr, pin_r),
                near = c(mean(ivr[c(1, 3)]), mean(ivr[c(2, 4)])))
  if (is.null(cand)) next
  cand$score <- land_frac(cand$r) + 0.6 * sqrt((mean(cand$r[c(1, 3)]) - mean(ivr[c(1, 3)]))^2 +
    ((mean(cand$r[c(2, 4)]) - mean(ivr[c(2, 4)])) * map_h / map_w)^2) + 0.5 * (1 - k)
  if (is.null(inset_pl) || cand$score < inset_pl$score) inset_pl <- cand
}
taken <- list(ivr, pin_r, inset_pl$r)

sources <- "© OpenStreetMap contributors · US Census TIGER/Line 2022 · Natural Earth"
plate_pl <- NULL
if (style$title_mode == "plate") {
  # measure with ragg itself (the device that draws it); textshaping/systemfonts
  # string metrics fail on the instanced Caslon file
  tw <- function(txt, fam, size) {
    agg_png(tempfile(fileext = ".png"), width = 1, height = 1, units = "in", res = 72)
    on.exit(invisible(dev.off()))
    grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = grid::gpar(fontfamily = fam, fontsize = size * ts))),
                       "in", valueOnly = TRUE)
  }
  # The logo is one unit (mark + wordmark + tagline): placed whole, never split or
  # recoloured. It already says "Shipwright", so the plate titles the office only.
  LOGO_IN <- 1.0 * ts
  plate_title <- sub("^Shipwright · ", "", office$name)
  credit_lines <- c("\u00A9 OpenStreetMap contributors", "US Census TIGER/Line 2022 \u00B7 Natural Earth")
  plate_w <- (0.12 + 0.24 + 0.14) * ts + (if (!is.null(style$logo)) LOGO_IN else 0) +
    max(tw(plate_title, FT$title, 16), tw(office$subtitle, FT$label, 7), tw(credit_lines, FT$mono, 5))
  plate_h <- LOGO_IN + 0.2 * ts
  plate_pl <- place(plate_w / map_w, plate_h / map_h, avoid = taken, corners = c("tl", "tr", "bl", "br"))
  taken <- c(taken, list(plate_pl$r))
}

# scale bar lengths: a round km and a round nmi, each ~18% of the map width
nice <- function(x) { e <- 10^floor(log10(x)); m <- x / e; (if (m >= 5) 5 else if (m >= 2) 2 else 1) * e }
km_m <- nice(0.18 * ground_w_m)
nm_m <- nice(0.18 * ground_w_m / 1852) * 1852
bar_wf <- max(km_m, nm_m) / ground_w_m
sb_pl <- place(bar_wf + 0.06, 0.36 * ts / map_h, avoid = taken, corners = c("bl", "br", "tl", "tr"))

# ---- text helpers ------------------------------------------------------------
track <- function(s, family = "SW Sans") if (family == FT$mono) s else
  vapply(strsplit(s, ""), function(ch) paste(ch, collapse = " "), "")
title_case <- function(s) vapply(strsplit(tolower(s), "\n", fixed = TRUE),
  function(lines) paste(tools::toTitleCase(lines), collapse = "\n"), "")
fmt_dm <- function(v, pos, neg) {
  a <- abs(v); d <- floor(a + 1e-9); m <- round((a - d) * 60)
  if (m == 60) { d <- d + 1; m <- 0 }
  sprintf("%d°%02d′%s", d, m, if (v >= 0) pos else neg)
}
fmt_dd <- function(lat, lon) sprintf("%.5f° %s · %.5f° %s",
  abs(lat), if (lat >= 0) "N" else "S", abs(lon), if (lon >= 0) "E" else "W")
rf <- signif(ground_w_m / (map_w * 0.0254), 2)
scale_txt <- paste0("1:", formatC(rf, format = "d", big.mark = " "))
today <- format(Sys.Date(), "%Y-%m-%d")

halo_text <- function(df, family, face, size, colour, hjust = 0.5, vjust = 0.5)
  shadowtext::geom_shadowtext(data = df, aes(x = x, y = y, label = label),
    family = family, fontface = face, size = pt(size), colour = colour, hjust = hjust,
    vjust = vjust, lineheight = 0.92, bg.colour = C$halo, bg.r = 0.16)

# ---- main map ----------------------------------------------------------------
roads <- function(g, d, case_col, fill_col, wc, wf) {
  if (is.null(d) || nrow(d) == 0) return(g)
  g + geom_sf(data = d, colour = case_col, linewidth = lw(wc)) +
      geom_sf(data = d, colour = fill_col, linewidth = lw(wf))
}
g <- ggplot() + geom_sf(data = L$land, fill = C$land, colour = NA)
if (!is.null(terrain)) g <- g + terrain
if (!is.null(L$water) && nrow(L$water) > 0) g <- g + geom_sf(data = L$water, fill = C$water, colour = NA)
g <- g + geom_sf(data = L$land, fill = NA, colour = C$coast, linewidth = lw(0.45))
g <- roads(g, L$major_roads, C$road_case, C$road, 1.7, 0.95)
g <- roads(g, L$freeways, C$freeway_case, C$freeway, 2.6, 1.5)

# labels: water names in mixed-case italic, everything else tracked semibold caps
lab <- L$labels; xy <- st_coordinates(lab)
keep <- !(fx(xy[, 1]) > ivr[1] - 0.02 & fx(xy[, 1]) < ivr[3] + 0.02 & fy(xy[, 2]) > ivr[2] - 0.02 & fy(xy[, 2]) < ivr[4] + 0.02)
# ... and labels whose anchor would sit under the inset card or the title plate
for (q in list(inset_pl$r, plate_pl$r)) if (!is.null(q))
  keep <- keep & !(fx(xy[, 1]) > q[1] - 0.1 & fx(xy[, 1]) < q[3] + 0.1 & fy(xy[, 2]) > q[2] - 0.06 & fy(xy[, 2]) < q[4] + 0.06)
lab <- lab[keep, ]; xy <- xy[keep, , drop = FALSE]
is_water <- grepl("HARBOR|HARBOUR|OCEAN|WATERWAY|BAY|CHANNEL|LAGOON|RIVER|CANAL|SOUND|BASIN",
                  lab$name, ignore.case = TRUE)
if (any(is_water))
  g <- g + halo_text(data.frame(x = xy[is_water, 1], y = xy[is_water, 2],
                                label = title_case(lab$name[is_water])),
                     FT$water, if (isTRUE(style$water_italic)) "italic" else "plain", 7.6, C$water_label)
if (any(!is_water))
  g <- g + halo_text(data.frame(x = xy[!is_water, 1], y = xy[!is_water, 2],
                                label = track(toupper(lab$name[!is_water]))),
                     FT$label, "bold", 5.9, C$label)

# extent box + pin
g <- g + annotate("rect", xmin = iv[["xmin"]], xmax = iv[["xmax"]], ymin = iv[["ymin"]], ymax = iv[["ymax"]],
                  fill = NA, colour = C$accent, linewidth = lw(0.9)) +
  geom_sf(data = office_sf, shape = 21, size = 2.1 * ts, stroke = 0.55 * ts, fill = C$accent, colour = "white")

# neatline ticks (drawing style)
if (isTRUE(style$graticule)) {
  steps <- c(1, 2, 5, 10, 15, 20, 30, 60) / 60
  st_lon <- steps[which.min(abs(dx / steps - 5))]; st_lat <- steps[which.min(abs(dy / steps - 4))]
  lons <- seq(ceiling(bb[["xmin"]] / st_lon) * st_lon, bb[["xmax"]], by = st_lon)
  lats <- seq(ceiling(bb[["ymin"]] / st_lat) * st_lat, bb[["ymax"]], by = st_lat)
  tlx <- 0.07 / map_h * dy; tly <- 0.07 / map_w * dx
  lons <- lons[fx(lons) > 0.04 & fx(lons) < 0.96]; lats <- lats[fy(lats) > 0.05 & fy(lats) < 0.95]
  cr0 <- inset_pl$r                       # no ticks hidden under the inset card
  lons <- lons[!(fx(lons) > cr0[1] - 0.03 & fx(lons) < cr0[3] + 0.03 & cr0[4] > 0.9)]
  lats <- lats[!(fy(lats) > cr0[2] - 0.03 & fy(lats) < cr0[4] + 0.03 & cr0[1] < 0.1)]
  g <- g +
    annotate("segment", x = lons, xend = lons, y = bb[["ymax"]], yend = bb[["ymax"]] - tlx,
             colour = C$ink, linewidth = lw(0.5)) +
    annotate("segment", x = bb[["xmin"]], xend = bb[["xmin"]] + tly, y = lats, yend = lats,
             colour = C$ink, linewidth = lw(0.5)) +
    halo_text(data.frame(x = lons, y = bb[["ymax"]] - 1.9 * tlx,
                         label = vapply(lons, fmt_dm, "", "E", "W")), FT$mono, "plain", 5, C$dim, vjust = 1) +
    halo_text(data.frame(x = bb[["xmin"]] + 1.6 * tly, y = lats,
                         label = vapply(lats, fmt_dm, "", "N", "S")), FT$mono, "plain", 5, C$dim, hjust = 0)
}

# scale bar (km, alternating) over a nmi rule, north arrow to the right
if (!is.null(sb_pl)) {
  r  <- sb_pl$r
  x0 <- ilon(r[1] + 0.012); yb <- ilat(r[2] + 0.13 * ts / map_h)
  bhd <- 0.045 * ts / map_h * dy
  kmd <- km_m / m_per_deg_x; nmd <- nm_m / m_per_deg_x
  g <- g +
    annotate("rect", xmin = x0, xmax = x0 + kmd / 2, ymin = yb, ymax = yb + bhd, fill = C$ink, colour = C$ink, linewidth = lw(0.4)) +
    annotate("rect", xmin = x0 + kmd / 2, xmax = x0 + kmd, ymin = yb, ymax = yb + bhd, fill = C$paper, colour = C$ink, linewidth = lw(0.4)) +
    annotate("segment", x = x0, xend = x0 + nmd, y = yb - 1.4 * bhd, yend = yb - 1.4 * bhd, colour = C$ink, linewidth = lw(0.6)) +
    annotate("segment", x = c(x0, x0 + nmd), xend = c(x0, x0 + nmd), y = yb - 1.4 * bhd, yend = yb - 2.3 * bhd,
             colour = C$ink, linewidth = lw(0.6)) +
    halo_text(data.frame(x = c(x0, x0 + kmd / 2, x0 + kmd), y = yb + bhd * 1.7,
                         label = c("0", format(km_m / 2000), paste(format(km_m / 1000), "km"))),
              FT$mono, "plain", 5.2, C$ink, vjust = 0) +
    halo_text(data.frame(x = x0 + nmd, y = yb - 3.2 * bhd, label = paste(format(nm_m / 1852), "nmi")),
              FT$mono, "plain", 5.2, C$ink, hjust = 0.5, vjust = 1)
  nx <- x0 + max(kmd, nmd) + 0.045 * dx; ny <- yb - 1.2 * bhd; nl <- 0.2 * ts / map_h * dy
  g <- g +
    annotate("segment", x = nx, xend = nx, y = ny, yend = ny + nl, colour = C$ink, linewidth = lw(0.7),
             arrow = grid::arrow(length = grid::unit(0.05 * ts, "in"), type = "closed")) +
    halo_text(data.frame(x = nx, y = ny + nl * 1.12, label = "N"), FT$mono, "bold", 6, C$ink, vjust = 0)
}

g <- g + coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE, datum = NA) +
  theme_void() + theme(panel.background = element_rect(fill = C$water, colour = NA),
                       plot.margin = margin(0, 0, 0, 0))

# ---- inset -------------------------------------------------------------------
inset_plot <- function() {
  b <- .osm_polys(harbor$buildings); wp <- .osm_polys(harbor$water)
  pl <- .osm_lines(harbor$piers);    pp <- .osm_polys(harbor$piers)
  tb <- inset_office_building(b, office_sf)
  if (!is.null(tb)) b <- b[lengths(st_equals(b, tb)) == 0, ]
  p <- ggplot()
  if (!is.null(inset_land) && nrow(inset_land) > 0) p <- p + geom_sf(data = inset_land, fill = C$land, colour = NA)
  if (.nz(wp)) p <- p + geom_sf(data = wp, fill = C$water, colour = NA)
  if (.nz(pp)) p <- p + geom_sf(data = pp, fill = C$building, colour = NA)
  if (.nz(b))  p <- p + geom_sf(data = b, fill = C$building, colour = C$building_edge, linewidth = lw(0.25))
  if (!is.null(tb)) p <- p + geom_sf(data = tb, fill = C$target_fill, colour = C$accent, linewidth = lw(0.9))
  if (.nz(pl)) p <- p + geom_sf(data = pl, colour = C$pier, linewidth = lw(0.6))
  p <- p + geom_sf(data = office_sf, shape = 21, size = 3 * ts, stroke = 0.7 * ts, fill = C$accent, colour = "white")
  ix <- iv[["xmax"]] - iv[["xmin"]]; iy <- iv[["ymax"]] - iv[["ymin"]]
  ig <- ix * 111320 * cos(office$lat * pi / 180); im <- nice(0.22 * ig); imd <- im / ig * ix
  xe <- iv[["xmax"]] - 0.05 * ix; ys <- iv[["ymin"]] + 0.07 * iy
  p + annotate("segment", x = xe - imd, xend = xe, y = ys, yend = ys, colour = C$ink, linewidth = lw(0.6)) +
    annotate("segment", x = c(xe - imd, xe), xend = c(xe - imd, xe), y = ys, yend = ys + 0.035 * iy,
             colour = C$ink, linewidth = lw(0.6)) +
    halo_text(data.frame(x = xe - imd / 2, y = ys + 0.05 * iy, label = paste(im, "m")), FT$mono, "plain", 5, C$ink, vjust = 0) +
    halo_text(data.frame(x = iv[["xmin"]] + 0.04 * ix, y = iv[["ymax"]] - 0.05 * iy,
                         label = toupper(office$inset_caption)), FT$mono, "bold", 5.4, C$ink, hjust = 0, vjust = 1) +
    coord_sf(xlim = iv[c("xmin", "xmax")], ylim = iv[c("ymin", "ymax")], expand = FALSE, datum = NA) +
    theme_void() + theme(panel.background = element_rect(fill = C$water, colour = NA),
                         plot.margin = margin(0, 0, 0, 0))
}
inset <- inset_plot()

# ---- compose on the page -----------------------------------------------------
cx <- function(f) f                      # map fraction -> canvas x (map spans full width)
cy <- function(f) map_y0 + f * map_hf    # map fraction -> canvas y
canvas <- ggdraw() + theme(plot.background = element_rect(fill = C$paper, colour = NA)) +
  draw_plot(g, 0, map_y0, 1, map_hf)

# connectors: the two hull lines from the extent box to the matching card corners
cr <- inset_pl$r
bx <- c(cx(ivr[1]), cy(ivr[2]), cx(ivr[3]), cy(ivr[4])); cd <- c(cx(cr[1]), cy(cr[2]), cx(cr[3]), cy(cr[4]))
ddx <- mean(cd[c(1, 3)]) - mean(bx[c(1, 3)]); ddy <- mean(cd[c(2, 4)]) - mean(bx[c(2, 4)])
pairs <- if (ddx * ddy < 0) list(c(3, 4), c(1, 2)) else list(c(1, 4), c(3, 2))   # TR+BL else TL+BR
for (pr in pairs)
  canvas <- canvas + draw_line(x = c(bx[pr[1]], cd[pr[1]]), y = c(bx[pr[2]], cd[pr[2]]),
                               colour = C$accent, linewidth = lw(0.6), alpha = 0.6)
mat_x <- 0.035 * ts / W; mat_y <- 0.035 * ts / H
canvas <- canvas +
  annotate("rect", xmin = cd[1] - mat_x, xmax = cd[3] + mat_x, ymin = cd[2] - mat_y, ymax = cd[4] + mat_y,
           fill = C$paper, colour = NA) +
  draw_plot(inset, cd[1], cd[2], cd[3] - cd[1], cd[4] - cd[2]) +
  annotate("rect", xmin = cd[1], xmax = cd[3], ymin = cd[2], ymax = cd[4], fill = NA,
           colour = C$accent, linewidth = lw(0.9))

lab_at <- function(cv, txt, x, y, fam, face, size, col, hjust = 0, vjust = 0.5, lh = 1)
  cv + draw_label(txt, x = x, y = y, fontfamily = fam, fontface = face, size = size * ts,
                  colour = col, hjust = hjust, vjust = vjust, lineheight = lh)
iy <- function(inches) inches / H; ix <- function(inches) inches / W
top <- 1

if (style$title_mode == "header") {
  canvas <- canvas |>
    lab_at("SHIPWRIGHT · OFFICE LOCATION", 0, top - iy(0.09 * ts), FT$mono, "bold", 5.6, C$faint, vjust = 1) |>
    lab_at(office$name, 0, top - iy(0.25 * ts), FT$title, "bold", 13, C$ink, vjust = 1) |>
    lab_at(office$subtitle, 0, top - iy(0.48 * ts), FT$label, "plain", 7.6, C$dim, vjust = 1) |>
    lab_at(paste(fmt_dd(office$lat, office$lon), paste(scale_txt, "at this size ·", today), sources, sep = "\n"),
           1, top - iy(0.25 * ts), FT$mono, "plain", 5.3, C$dim, hjust = 1, vjust = 1, lh = 1.35)
  canvas <- canvas + annotate("rect", xmin = 0, xmax = 1, ymin = map_y0, ymax = map_y0 + map_hf,
                              fill = NA, colour = C$rule, linewidth = lw(0.8))
}

if (style$title_mode == "block") {
  b0 <- 0; b1 <- bh / H
  cells <- list(
    list(w = 0.38, k = "FIGURE",      v = office$name, v2 = office$subtitle),
    list(w = 0.18, k = "COORDINATES", v = sprintf("%.5f° N", office$lat), v2 = sprintf("%.5f° W", abs(office$lon))),
    list(w = 0.16, k = "SCALE",       v = scale_txt, v2 = "at 6.5 in width"),
    list(w = 0.12, k = "DATE",        v = today, v2 = "rev A"),
    list(w = 0.16, k = "SOURCES",     v = "OSM · TIGER 2022", v2 = "Natural Earth"))
  x <- 0
  for (i in seq_along(cells)) {
    cl <- cells[[i]]; pad <- ix(0.06 * ts)
    if (i > 1) canvas <- canvas + draw_line(x = c(x, x), y = c(b0, b1), colour = C$rule, linewidth = lw(0.5))
    canvas <- canvas |>
      lab_at(cl$k, x + pad, b1 - iy(0.08 * ts), FT$mono, "bold", 4.8, C$faint, vjust = 1) |>
      lab_at(cl$v,  x + pad, b1 - iy(0.25 * ts), if (i == 1) FT$title else FT$mono,
             if (i == 1) "bold" else "plain", if (i == 1) 8.4 else 6.4, C$ink, vjust = 1) |>
      lab_at(cl$v2, x + pad, b1 - iy(0.42 * ts), if (i == 1) FT$label else FT$mono, "plain",
             if (i == 1) 6.4 else 5.6, C$dim, vjust = 1)
    x <- x + cl$w
  }
  canvas <- canvas +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = NA, colour = C$rule, linewidth = lw(1)) +
    draw_line(x = c(0, 1), y = c(b1, b1), colour = C$rule, linewidth = lw(1))
}

if (style$title_mode == "plate" && !is.null(plate_pl)) {
  pr <- plate_pl$r; p <- c(cx(pr[1]), cy(pr[2]), cx(pr[3]), cy(pr[4]))
  canvas <- canvas + annotate("rect", xmin = p[1], xmax = p[3], ymin = p[2], ymax = p[4], fill = C$paper, colour = NA)
  lx <- p[1] + ix(0.12 * ts)
  if (!is.null(style$logo) && file.exists(file.path(ROOT, style$logo))) {
    lg <- png::readPNG(file.path(ROOT, style$logo))
    canvas <- canvas + draw_grob(grid::rasterGrob(lg), lx, p[2] + iy(0.1 * ts), ix(LOGO_IN), iy(LOGO_IN))
    lx <- lx + ix(LOGO_IN + 0.24 * ts)
    canvas <- canvas + draw_line(x = c(lx - ix(0.12 * ts), lx - ix(0.12 * ts)), y = c(p[2] + iy(0.16 * ts), p[4] - iy(0.16 * ts)),
                                 colour = C$rule, linewidth = lw(0.5))
  }
  canvas <- canvas |>
    lab_at(plate_title, lx, p[4] - iy(0.2 * ts), FT$title, "plain", 16, C$ink, vjust = 1) |>
    lab_at(office$subtitle, lx, p[4] - iy(0.52 * ts), FT$label, "plain", 7, C$dim, vjust = 1) |>
    lab_at(paste(credit_lines, collapse = "\n"), lx, p[2] + iy(0.14 * ts), FT$mono, "plain", 5, C$faint, vjust = 0, lh = 1.3)
}

# ---- write -------------------------------------------------------------------
out_dir <- file.path(ROOT, "outputs/comps"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, sprintf("%s-%s-%s.png", office_id, style_id, size_id))
agg_png(out, width = W, height = H, units = "in", res = DPI, background = C$paper)
print(canvas); invisible(dev.off())
cat(sprintf("Wrote %s  (%.2f x %.2f in, inset %s, scale %s, %s)\n", out, W, H,
            inset_pl$corner, sb_pl$corner %||% "none", scale_txt))
