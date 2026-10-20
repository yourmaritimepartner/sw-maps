# Roadmap

Goal: the team pulls fresh, report-ready maps for any work site from
https://sw-maps.exe.xyz instead of screenshotting Google Maps.

## Done (2026-10-17 → 10-20)

- VM `sw-maps` (exe-shipwright), systemd service, team-only sharing; repo renamed to `sw-maps`.
- Renderer `scripts/comp_map.R`: true print size, fit-to-size extent, automatic inset
  placement, extent box + connectors, km / nmi scale bar, north arrow, label classes with
  halos, PNG + PDF + SVG.
- Three styles with a switcher (B · Drawing default), three size presets.
- Hanken Grotesk throughout (maps and app); official horizontal logo used whole.
- Review notes (press N) with a `/notes` queue; design comps page.

## Next, in order

1. **New work sites (self-serve).** A form: address or coordinates (later: click a map),
   scale preset (site / harbor / region), title. Geocode (US Census geocoder), snap the pin
   to the building footprint and show it for confirmation, derive `main_bbox` and the inset
   from the preset, fetch and cache the inset OSM data, save a config, render. US-only to
   start (TIGER). This is the biggest gap.
2. **Local, versioned base data** on the VM (OSM extract or Overture via DuckDB) so renders
   never depend on Overpass; stamp the data release on the figure.
3. **True projection + provenance.** Draw in the local UTM zone instead of lat/lon; write a
   sidecar JSON per render (config, style, size, data releases, git SHA, time) and a figure ID.
4. **Custom sizes.** Width × height input alongside the presets.
5. **Labels.** Move labels that collide instead of dropping them; road names (H-1, Nimitz Hwy).
6. **Layered SVG** (named Illustrator layers) for the new styles, then retire `location_map.R`.
7. **Async renders.** Renders block the single-threaded server for ~30 s; queue them.

## Verification still owed (by tools, not by eye)

- Golden renders for both offices, pixel-diffed on every change.
- Pin falls inside the target building footprint.
- Building area intersecting water ≈ 0.
- Scale bar matches a known measured length.
- Teammate test: someone who has never seen the code makes a report-ready figure in under
  5 minutes with no Illustrator pass.

## Open questions for the owner

- Favicons on sw-maps and dashboard.shipwright.biz use the ship mark alone (as shipwright.biz
  does). OK, or switch?
- Design-language page on the dashboard: add a "Maps & figures" section and archive v1?
- Dashboard card for SW Maps: category still `99 Admin` (clear the field, pick `02 Office standards`).
