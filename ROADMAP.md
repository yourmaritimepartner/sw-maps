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

## Done (2026-10-20, session 2): work sites, self-serve

- **/sites/new**: title + address or coordinates + scale preset (site / harbor / region) ->
  Census geocoder -> OSM candidates ranked by address match, title match, distance ->
  confirm page (numbered footprints, geocoder point, click to move the pin) -> extents from
  the preset, inset OSM + coastline + labels + relief cached, `configs/<id>.R`, tool checks,
  B · Drawing render. Steps run in the background (`scripts/new_site.R`).
- **/locations** inventory (+ `/api/locations`): every office and site with address,
  coordinates, scale, pinned footprint, check results, who added it; "New map from here".
  The confirm page warns when a location already exists within 1 km.
- Tested on a downtown pier (Pier 66, Seattle), a rural boatyard (Brooklin Boat Yard, ME)
  and an offshore point (Block Island Wind Farm, RI; turbines drawn in the inset).
- Coastline fixes found by the checks: closed coastline rings (islands, barrier islands)
  were dropped; clipped multi-part lines lost all but their first part; a bbox whose
  coastline missed its edge never split; the south-edge "must be sea" rule; empty answers
  (open sea) were refetched on every render. Florida's Harbor Beach was painted as sea
  (27.6 % of building area in water, now 0 %) and Florida · Main site · Half page never
  rendered; both fixed, Florida renders changed on purpose, Hawaii pixel-identical.

## Next, in order

1. **Local, versioned base data** on the VM (OSM extract or Overture via DuckDB) so renders
   never depend on Overpass; stamp the data release on the figure. Overpass was at capacity
   for most of session 2: one site build waited 7 minutes on retries.
2. **True projection + provenance.** Draw in the local UTM zone instead of lat/lon; write a
   sidecar JSON per render (config, style, size, data releases, git SHA, time) and a figure ID.
3. **Labels.** Move labels that collide instead of dropping them (Florida half page clips
   "Atlantic Ocean" at the edge; inset captions can run into buildings); road names.
4. **Custom sizes.** Width × height input alongside the presets.
5. **Sites, next steps.** Edit or remove a site from the app; app-added sites live only on
   the VM until committed (`configs/`, `outputs/location-maps/<id>-osm.rds`).
6. **Layered SVG** (named Illustrator layers) for the new styles, then retire `location_map.R`.
7. **Async renders.** Card renders still block the single-threaded server for ~30 s; site
   builds already run in the background.

## Verification (by tools, not by eye)

- Done: golden renders for both offices, pixel-diffed (`scripts/golden.sh`).
- Done: pin falls inside the target building footprint (`scripts/check_site.R`).
- Done: building area intersecting water ≈ 0, ignoring the outer 2 m of each outline
  (digitizing tolerance; raw figure reported too).
- Owed: scale bar matches a known measured length.
- Teammate test: someone who has never seen the code makes a report-ready figure in under
  5 minutes with no Illustrator pass.

## Open questions for the owner

- Favicons on sw-maps and dashboard.shipwright.biz use the ship mark alone (as shipwright.biz
  does). OK, or switch?
- Design-language page on the dashboard: add a "Maps & figures" section and archive v1?
- Dashboard card for SW Maps: category still `99 Admin` (clear the field, pick `02 Office standards`).
