#!/usr/bin/env Rscript
# Ground-truth comparison render: candidate Amman Building plus nearby OSM IDs.
suppressPackageStartupMessages({
  library(sf); library(ggplot2); library(cowplot); library(ragg)
})

ROOT <- path.expand("~/sw-projects/sw-locations")
OUT  <- file.path(ROOT, "outputs/location-maps")
cache <- readRDS(file.path(OUT, "florida-osm.rds"))
TARGET_ID <- "1156538249"

nz <- function(x) !is.null(x) && inherits(x, "sf") && nrow(x) > 0
bind_parts <- function(layer, parts) {
  xs <- lapply(parts, function(nm) layer[[nm]])
  xs <- xs[vapply(xs, nz, logical(1))]
  if (!length(xs)) return(NULL)
  cols <- Reduce(intersect, lapply(xs, names))
  do.call(rbind, lapply(xs, function(x) x[, cols, drop = FALSE]))
}

buildings <- bind_parts(cache$buildings, c("osm_polygons", "osm_multipolygons"))
water     <- bind_parts(cache$water, c("osm_polygons", "osm_multipolygons"))
piers_p   <- bind_parts(cache$piers, c("osm_polygons", "osm_multipolygons"))
piers_l   <- cache$piers$osm_lines
roads     <- cache$roads$osm_lines
coast     <- cache$coast$osm_lines
target <- buildings[buildings$osm_id == TARGET_ID, ]
stopifnot(nrow(target) == 1L)
office_pt <- st_sf(name = "Shipwright Florida", geometry = st_centroid(st_geometry(target)))
xy <- st_coordinates(office_pt)[1, ]

crop_sf <- function(x, bb) {
  if (!nz(x)) return(NULL)
  suppressWarnings(st_crop(x, st_bbox(c(xmin=bb[1], ymin=bb[2], xmax=bb[3], ymax=bb[4]), crs=4326)))
}
base_map <- function(bb, labels = FALSE) {
  b <- crop_sf(buildings, bb)
  w <- crop_sf(water, bb)
  pp <- crop_sf(piers_p, bb)
  pl <- crop_sf(piers_l, bb)
  r <- crop_sf(roads, bb)
  cl <- crop_sf(coast, bb)
  tgt <- crop_sf(target, bb)

  p <- ggplot() +
    { if (nz(w)) geom_sf(data=w, fill="#9FC3D6", color=NA) } +
    { if (nz(pp)) geom_sf(data=pp, fill="#D8D2C6", color="#8A8577", linewidth=.15) } +
    { if (nz(r)) geom_sf(data=r, color="#FFFFFF", linewidth=.9) } +
    { if (nz(r)) geom_sf(data=r, color="#AFA89B", linewidth=.35) } +
    { if (nz(b)) geom_sf(data=b, fill="#C9C2B6", color="#5A5348", linewidth=.18) } +
    { if (nz(pl)) geom_sf(data=pl, color="#7E7A6C", linewidth=.5) } +
    { if (nz(cl)) geom_sf(data=cl, color="#7FB0C9", linewidth=.5) } +
    { if (nz(tgt)) geom_sf(data=tgt, fill="#F2C14E", color="#101820", linewidth=.8) } +
    geom_sf(data=office_pt, shape=16, color="#F2C14E", size=4) +
    coord_sf(xlim=bb[c(1,3)], ylim=bb[c(2,4)], expand=FALSE, datum=NA) +
    theme_void(base_family="Arial") +
    theme(panel.background=element_rect(fill="#EDE7DB", color=NA),
          plot.background=element_rect(fill="white", color=NA),
          panel.border=element_rect(fill=NA, color="#101820", linewidth=.5),
          plot.margin=margin(8,8,8,8))

  if (labels && nz(b)) {
    lab <- st_point_on_surface(b)
    p <- p + geom_sf_text(data=lab, aes(label=osm_id), size=2.7, family="Arial",
                          color="#101820", check_overlap=TRUE) +
      annotate("label", x=xy[1], y=xy[2] + .00065,
               label="OSM 1156538249\n2550 Eisenhower Blvd",
               family="Arial", fontface="bold", size=3.2,
               fill="white", color="#101820", label.size=.25)
  }
  p
}

# Left approximates the supplied Google screenshot; right identifies nearby footprints.
context_bb <- c(-80.130, 26.083, -80.105, 26.100)
zoom_bb    <- c(-80.127, 26.089, -80.1225, 26.094)
p1 <- base_map(context_bb) +
  ggtitle("SCREENSHOT-MATCHING CONTEXT") +
  theme(plot.title=element_text(face="bold", size=13, color="#101820", margin=margin(b=5)))
p2 <- base_map(zoom_bb, labels=TRUE) +
  ggtitle("BUILDING FOOTPRINTS · OSM IDs") +
  theme(plot.title=element_text(face="bold", size=13, color="#101820", margin=margin(b=5)))

final <- plot_grid(p1, p2, nrow=1, rel_widths=c(1.45,1), align="h")
outfile <- file.path(OUT, "inspect-florida-buildings.png")
agg_png(outfile, width=2200, height=1150, units="px", res=150)
print(final)
invisible(dev.off())
cat(sprintf("Candidate OSM %s center: %.8f, %.8f\n", TARGET_ID, xy[1], xy[2]))
cat("Wrote", outfile, "\n")
