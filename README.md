# Shipwright Maps (sw-maps)

Location maps for Shipwright project reports, rendered from open data by an R
pipeline and served to the team at **https://sw-maps.exe.xyz** (exe.dev login,
shared with the `sw` team): our two offices, and any US work site a teammate adds
at **/sites/new**.

Each map is a figure laid out at its **printed size** (type in real points): a regional
map with an inset close-up of the site, an extent box and connector lines, a km / nmi
scale bar, a north arrow, and labels styled by class. The inset places itself in the
nearest mostly-water corner. Outputs: 300 dpi **PNG**, vector **PDF** and **SVG** with
live Hanken Grotesk text.

## Styles and sizes

| Style (`styles/`) | Look | Use |
|---|---|---|
| `drawing` — **B · Drawing** (default) | quiet grey base, lat/lon ticks, title block below | project reports |
| `platform` — A · Platform | BD-apps palette, light relief, header above | internal docs |
| `mainsite` — C · Main site | horizontal logo plate, fuller sea and relief | proposals, collateral |

Sizes: `report` 6.5 × 4.6 in, `half` 3.15 × 3.6 in, `slide` 13.33 × 7.5 in (16:9).

```sh
Rscript scripts/comp_map.R hawaii drawing report outputs/maps   # -> outputs/maps/hawaii-drawing-report.{png,pdf,svg}
```

## Web app

```
app/plumber.R           /  maps with style + size switchers, POST /render/<id>?style=&size=
                        /sites/new  new work site: form -> confirm the pin -> config + render
                        /comps (design comps), /view?f= (full size), /notes (review queue)
                        /api/notes (JSON), /healthz
app/static/notes.js     review notes: press N, click a spot, type a line
app/serve.R             plumber on :8000 (exe.dev proxies HTTPS -> :8000)
deploy/sw-maps.service  systemd unit (repo at /home/exedev/sw-maps)
```

Deploy: push to `main`, then
`ssh sw-maps.exe.xyz 'cd ~/sw-maps && git fetch -q && git reset -q --hard origin/main && sudo systemctl restart sw-maps'`.
Renders (`outputs/maps`, `outputs/comps`), notes and site drafts (`data/`) are untracked and
survive this. Work sites added through the app write `configs/<id>.R` and
`outputs/location-maps/<id>-osm.rds` (+ coastline caches) on the VM: untracked until someone
commits them, so `git clean` would delete them.

## Layout

```
scripts/
  comp_map.R         renderer: config + style + size -> composed figure (PNG/PDF/SVG)
  mod_mainmap.R      regional layers (TIGER + Natural Earth): main_layers()
  mod_inset.R        inset helpers from cached OSM (buildings, piers, water)
  mod_oceanfill.R    ocean/land from the OSM coastline: ocean_land()
  mod_terrain.R      hillshade blended toward the style's land colour: terrain_layer()
  mod_site.R         work sites: Census geocoder, OSM snapping, presets -> extents, caches, config writer
  new_site.R         the steps the app runs in the background: locate, repin, build
  check_site.R       tool checks for any config: pin in the target footprint, buildings in water
  golden.sh          office golden renders + ImageMagick pixel diff
  location_map.R     legacy renderer (original style + layered SVG for Illustrator)
styles/              drawing.R, platform.R, mainsite.R (each defines `style`)
configs/             hawaii.R, florida.R, plus generated work sites (each defines `office`)
fonts/               Hanken Grotesk static instances (OFL)
app/static/logo/     official horizontal logo: SVG master + transparent PNG for the renderer
outputs/location-maps/  cached OSM / coastline .rds (committed; renders run offline) + legacy renders
```

## Adding a work site (self-serve)

At **/sites/new**: a title, an address or coordinates, and a scale preset.

| Preset | Main map | Inset close-up | Labels |
|---|---|---|---|
| `site` | ~3 km across | ~400 m | suburbs, neighbourhoods |
| `harbor` | ~12 km | ~1 km | towns, suburbs, bays |
| `region` | ~45 km | ~2 km | cities, towns, bays |

1. **Locate.** An address goes to the US Census geocoder, coordinates (decimal or
   degrees-minutes) go straight through. The geocoder's point lies on the street centreline,
   not on the building, so OSM is fetched around it and candidates are ranked: OSM address
   equals the geocoded address, then an OSM name that matches the title (every distinctive
   word), then the nearest footprints. A building inside a named marina or boatyard area, or
   holding a named point, stands in for it.
2. **Confirm.** The page shows the footprints, numbered candidates and the geocoder's point.
   Pick one, keep the point (offshore, or no footprint), or click the picture to move the
   pin there. The click is re-snapped to the footprint under it without another fetch.
3. **Build.** The extents come from the preset; the inset grows to hold the whole target
   footprint. Inset OSM (buildings, water, piers, coastline), coastline for the main map,
   place labels and relief are fetched and cached. `configs/<id>.R` is written, then
   `scripts/check_site.R` runs and B · Drawing is rendered at report size. Other styles
   and sizes render from the card like the offices.

From the shell: `Rscript scripts/new_site.R new "<title>" "<address or lat, lon>" harbor`
prints a draft id after locating. Then `Rscript scripts/new_site.R build <draft> 1`
builds on candidate 1, or `point` keeps the point.

## Verifying (tools, not eyes)

```sh
Rscript scripts/check_site.R <id>          # pin in footprint + buildings in water; exit 1 on failure
scripts/golden.sh render before            # before a renderer change: all office x style x size PNGs
scripts/golden.sh render after && scripts/golden.sh diff before after   # AE pixel count per file
```

## Adding an office

1. Geocode the address; **ground-truth the exact building** against the OSM footprint
   (the geocoder's street centroid is not the building).
2. Fetch the OSM `buildings`, `natural=water`, `man_made=pier` for the inset area and save
   `outputs/location-maps/<name>-osm.rds` (see `scripts/fetch_florida_osm.R`).
3. Copy a `configs/*.R`; set the pin, `main_bbox`, `inset_bbox` / `inset_view`, labels,
   caption, `use_terrain`, TIGER state/county. Composition is automatic in `comp_map.R`
   (the `title_*` / `inset_*` position fields are only used by the legacy renderer).
   (Or add it as a work site at /sites/new and edit the generated config.)

## Gotchas

- Fonts: Hanken Grotesk only. On the VM `fonts/` is on fontconfig's path
  (`~/.config/fontconfig/fonts.conf`) so ragg, cairo_pdf and svglite all find it. Hanken has
  no hair/thin space, prime or ʻokina: no letter-spacing, `'` for minutes, `‘` for the ʻokina.
- The logo is one unit (mark + wordmark + tagline). Never crop or split it.
- OSM/Overpass returns HTTP 406 without a User-Agent, rate-limits hard and is often at
  capacity (502/504). `mod_site.R`'s `overpass()` sends a UA, backs off exponentially across
  four endpoints and rejects partial answers (a timeout arrives as a `<remark>` inside an
  otherwise valid document). Everything is cached so renders run offline.
- Without `lwgeom` (not on the VM), sf cannot measure lon/lat distances with s2 off:
  transform to a local projection first (see `inset_office_building`).
- The VM is the canonical renderer; other machines differ slightly in font metrics.

Basemap data © OpenStreetMap contributors; US Census TIGER/Line; Natural Earth.
