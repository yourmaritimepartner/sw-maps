# Style A — PLATFORM. The BD-apps design language (ADR 0004) applied to a map figure:
# Hanken Grotesk, warm neutrals with light relief, one navy accent, hairlines, header above.
style <- list(
  id = "platform", label = "A · Platform",
  title_mode = "header",            # eyebrow + title + meta strip above the map
  fonts = list(title = "Hanken Grotesk", label = "Hanken Grotesk", water = "Hanken Grotesk", mono = "Hanken Grotesk"),
  col = list(
    paper = "#FFFFFF", ink = "#1C1E21", dim = "#6B6E72", faint = "#9A9DA1", rule = "#D9D9D4",
    land = "#F1F0EB", water = "#CFDFE8", coast = "#A9C2D0",
    road = "#FFFFFF", road_case = "#D9D9D4", freeway = "#FFFFFF", freeway_case = "#BDBDB6",
    label = "#4A4F55", water_label = "#2C5F80", halo = "#F1F0EB",
    building = "#DEDCD5", building_edge = "#B9B7AE", pier = "#B9B7AE",
    accent = "#015686", target_fill = "#C9DCEA"
  ),
  terrain = list(on = TRUE, alpha = 0.24, shadow = "#5A5A55", light = "#FFFFFF"),
  graticule = FALSE,
  water_italic = TRUE
)
