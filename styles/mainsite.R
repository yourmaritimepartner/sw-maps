# Comp C — MAIN SITE. Bridges to shipwright.biz for proposals and collateral: Caslon
# masthead (Libre Caslon stands in for the site's Adobe Caslon), the whole logo lockup,
# fuller sea and relief, italic serif water names. Full-bleed map with a title plate.
style <- list(
  id = "mainsite", label = "C · Main site",
  title_mode = "plate",             # white plate on the map: logo + serif title
  fonts = list(title = "SW Serif", label = "SW Sans", water = "SW Serif", mono = "SW Mono"),
  col = list(
    paper = "#FFFFFF", ink = "#1A2733", dim = "#526071", faint = "#8A96A3", rule = "#D9D9D4",
    land = "#EFE9DC", water = "#A9C9DD", coast = "#7FA6BD",
    road = "#FFFFFF", road_case = "#CDC5B6", freeway = "#FFFFFF", freeway_case = "#B3AA99",
    label = "#3A4552", water_label = "#0A3A57", halo = "#EFE9DC",
    building = "#D8D1C3", building_edge = "#9A9283", pier = "#9A9283",
    accent = "#015686", target_fill = "#BFD6E6"
  ),
  terrain = list(on = TRUE, alpha = 0.34, shadow = "#4A4438", light = "#FFFCF5"),
  graticule = FALSE,
  water_italic = TRUE,
  logo = "app/static/shipwright-logo-vertical.png"   # official vertical lockup, always used whole
)
