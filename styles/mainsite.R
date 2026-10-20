# Style C — MAIN SITE. Bridges to shipwright.biz for proposals and collateral: a white
# title plate carrying the whole horizontal logo lockup, fuller sea and relief, italic
# water names. Full-bleed map.
style <- list(
  id = "mainsite", label = "C · Main site",
  title_mode = "plate",             # white plate on the map: logo lockup + office title
  fonts = list(title = "Hanken Grotesk", label = "Hanken Grotesk", water = "Hanken Grotesk", mono = "Hanken Grotesk"),
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
  logo = "app/static/logo/shipwright-logo-horizontal.png"   # official horizontal lockup, always used whole
)
