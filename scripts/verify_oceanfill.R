#!/usr/bin/env Rscript
# Verification renders for mod_oceanfill.R -- proves ocean fills at INSET and MAIN bbox.
suppressPackageStartupMessages({ library(sf); library(ggplot2); library(ragg) })
source(path.expand("~/sw-projects/sw-locations/scripts/mod_oceanfill.R"))

WATER <- "#7FB0C9"   # deeper calm blue (contract palette)
LANDC <- "#EDE7DB"   # warm neutral (panel land)
outdir <- path.expand("~/sw-projects/sw-locations/outputs/location-maps")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

render_one <- function(bbox, title, file) {
  ol <- ocean_land(bbox)
  cat(sprintf("[%s] source=%s ocean_parts=%d land=%s\n", title, ol$source,
              nrow(ol$ocean), if (is.null(ol$land)) "NULL" else nrow(ol$land)))
  p <- ggplot() +
    geom_sf(data = ol$ocean, fill = WATER, color = NA) +
    { if (!is.null(ol$land)) geom_sf(data = ol$land, fill = LANDC, color = "#B0A99A", linewidth = 0.15) } +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
    labs(title = title, subtitle = sprintf("ocean=%s source; blue must cover ALL sea", ol$source)) +
    theme_minimal(base_size = 11) +
    theme(panel.background = element_rect(fill = "#FF00FF", color = NA),
          panel.grid = element_line(color = "#C7C2B6", linewidth = 0.2))
  fp <- file.path(outdir, file)
  agg_png(fp, width = 1200, height = 1100, units = "px", res = 150)
  print(p); invisible(dev.off())
  cat("wrote", fp, "\n")
  ol
}

inset <- c(-157.874, 21.306, -157.862, 21.315)
main  <- c(-157.98, 21.24, -157.79, 21.40)

render_one(inset, "Oceanfill INSET (Honolulu Harbor)", "A-oceanfill-inset.png")
render_one(main,  "Oceanfill MAIN (south Oahu)",       "A-oceanfill-main.png")
cat("VERIFY DONE\n")
