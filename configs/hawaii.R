# Shipwright HAWAII — office config. Defines `office` for scripts/location_map.R.
# Render:  Rscript scripts/location_map.R hawaii
office <- list(
  slug     = "shipwright-hawaii",            # output filename stem
  name     = "Shipwright · Hawaii",
  subtitle = "501A N Nimitz Hwy, Honolulu · Honolulu Harbor",
  # NE ("upper-right") corner of the large shed, OSM building 113186868.
  lon      = -157.86855,
  lat      =  21.31176,
  main_bbox  = c(xmin = -157.98, ymin = 21.24, xmax = -157.79, ymax = 21.40),
  # inset_bbox = the bbox the coastline + OSM detail are cached for;
  # inset_view = tighter ~1.4 w:h display crop, zoomed onto the Pier 19-21 shed.
  inset_bbox = c(xmin = -157.874,  ymin = 21.306,  xmax = -157.862,  ymax = 21.315),
  inset_view = c(xmin = -157.8715, ymin = 21.3098, xmax = -157.8645, ymax = 21.31447),
  osm_rds       = "harbor-osm.rds",
  inset_caption = "Honolulu Harbor",
  use_terrain   = TRUE,                       # Oʻahu has the Koʻolau Range behind the harbor
  tiger_state   = "Hawaii",
  tiger_county  = "Honolulu",
  tiger_year    = 2022,
  # Composition positions (normalized canvas coords). Title lower-left over the ocean;
  # inset card bottom-right (inset_x is the card's LEFT edge).
  title_x       = 0.035,
  title_y       = 0.150,
  subtitle_y    = 0.104,
  inset_x       = 0.6056,
  inset_top     = 0.345,
  inset_height  = 0.280,
  # establishing-map place labels (name + lon/lat). name may contain \n for two lines.
  labels = data.frame(
    name = c("PEARL HARBOR", "HNL AIRPORT", "DOWNTOWN\nHONOLULU", "DIAMOND HEAD", "KOʻOLAU RANGE"),
    lon  = c(-157.948, -157.923, -157.845, -157.808, -157.860),
    lat  = c(  21.360,   21.328,   21.322,   21.262,   21.392),
    stringsAsFactors = FALSE
  )
)
