# Shipwright FLORIDA — office config. Defines `office` for scripts/location_map.R.
# Render:  Rscript scripts/location_map.R florida
office <- list(
  slug     = "shipwright-florida",           # output filename stem
  name     = "Shipwright · Florida",
  subtitle = "2550 Eisenhower Blvd, Fort Lauderdale · Port Everglades",
  # Ground-truthed to the center of The Amman Building footprint, OSM 1156538249.
  lon      = -80.12478748,
  lat      =  26.09162755,
  main_bbox = c(xmin = -80.185, ymin = 26.058, xmax = -80.080, ymax = 26.125),
  # inset_bbox = the bbox the coastline + OSM detail are cached for;
  # inset_view = tighter ~1.4 w:h display crop around the office and turning basin.
  # Broad eastward extent keeps the bbox's south midpoint in the Atlantic,
  # which is required by mod_oceanfill's coastline-orientation sanity check.
  inset_bbox = c(xmin = -80.130,  ymin = 26.075,  xmax = -80.070,  ymax = 26.105),
  inset_view = c(xmin = -80.1275, ymin = 26.0858, xmax = -80.1095, ymax = 26.09735),
  osm_rds       = "florida-osm.rds",
  inset_caption = "Port Everglades",
  use_terrain   = FALSE,
  tiger_state   = "Florida",
  tiger_county  = "Broward",
  tiger_year    = 2022,
  # Composition positions in normalized canvas coordinates.
  title_x       = 0.035,
  title_y       = 0.945,
  subtitle_y    = 0.895,
  inset_x       = 0.040,
  inset_top     = 0.815,
  inset_height  = 0.300,
  # establishing-map place labels (name + lon/lat). name may contain \n for two lines.
  labels = data.frame(
    name = c("PORT EVERGLADES", "FLL AIRPORT", "DOWNTOWN\nFORT LAUDERDALE",
             "INTRACOASTAL\nWATERWAY", "ATLANTIC OCEAN"),
    lon  = c(-80.1160, -80.1570, -80.1280, -80.1065, -80.0915),
    lat  = c( 26.0910,  26.0705,  26.1185,  26.1115,  26.0830),
    stringsAsFactors = FALSE
  )
)
