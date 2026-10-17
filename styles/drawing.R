# Comp B — DRAWING. An engineering report figure: quiet grey base, the site is the only
# colour, lat/lon ticks on the neatline, and a drawing-style title block below the map.
style <- list(
  id = "drawing", label = "B · Drawing",
  title_mode = "block",             # title block table under the map
  fonts = list(title = "SW Sans", label = "SW Sans", water = "SW Sans", mono = "SW Mono"),
  col = list(
    paper = "#FFFFFF", ink = "#1C1E21", dim = "#6B6E72", faint = "#9A9DA1", rule = "#1C1E21",
    land = "#F7F7F5", water = "#E1E8EC", coast = "#9AA7AE",
    road = "#FFFFFF", road_case = "#CFCFCA", freeway = "#FFFFFF", freeway_case = "#A9A9A3",
    label = "#55595E", water_label = "#55595E", halo = "#F7F7F5",
    building = "#E6E6E2", building_edge = "#A6A6A0", pier = "#A6A6A0",
    accent = "#015686", target_fill = "#C9DCEA"
  ),
  terrain = list(on = FALSE),
  graticule = TRUE,
  water_italic = TRUE
)
