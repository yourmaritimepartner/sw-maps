# Shipwright Maps (sw-maps)

Location maps for Shipwright project reports, rendered from open data by an R
pipeline and served to the team at **https://sw-maps.exe.xyz** (exe.dev login,
shared with the `sw` team).

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
                        /comps (design comps), /view?f= (full size), /notes (review queue)
                        /api/notes (JSON), /healthz
app/static/notes.js     review notes: press N, click a spot, type a line
app/serve.R             plumber on :8000 (exe.dev proxies HTTPS -> :8000)
deploy/sw-maps.service  systemd unit (repo at /home/exedev/sw-maps)
```

Deploy: push to `main`, then
`ssh sw-maps.exe.xyz 'cd ~/sw-maps && git fetch -q && git reset -q --hard origin/main && sudo systemctl restart sw-maps'`.
Renders (`outputs/maps`, `outputs/comps`) and notes (`data/`) are untracked and survive this.

## Layout

```
scripts/
  comp_map.R         renderer: config + style + size -> composed figure (PNG/PDF/SVG)
  mod_mainmap.R      regional layers (TIGER + Natural Earth): main_layers()
  mod_inset.R        inset helpers from cached OSM (buildings, piers, water)
  mod_oceanfill.R    ocean/land from the OSM coastline: ocean_land()
  mod_terrain.R      hillshade blended toward the style's land colour: terrain_layer()
  location_map.R     legacy renderer (original style + layered SVG for Illustrator)
styles/              drawing.R, platform.R, mainsite.R (each defines `style`)
configs/             hawaii.R, florida.R (each defines `office`)
fonts/               Hanken Grotesk static instances (OFL)
app/static/logo/     official horizontal logo: SVG master + transparent PNG for the renderer
outputs/location-maps/  cached OSM / coastline .rds (committed; renders run offline) + legacy renders
```

## Adding an office

1. Geocode the address; **ground-truth the exact building** against the OSM footprint
   (the geocoder's street centroid is not the building).
2. Fetch the OSM `buildings`, `natural=water`, `man_made=pier` for the inset area and save
   `outputs/location-maps/<name>-osm.rds` (see `scripts/fetch_florida_osm.R`).
3. Copy a `configs/*.R`; set the pin, `main_bbox`, `inset_bbox` / `inset_view`, labels,
   caption, `use_terrain`, TIGER state/county. Composition is automatic in `comp_map.R`
   (the `title_*` / `inset_*` position fields are only used by the legacy renderer).

## Gotchas

- Fonts: Hanken Grotesk only. On the VM `fonts/` is on fontconfig's path
  (`~/.config/fontconfig/fonts.conf`) so ragg, cairo_pdf and svglite all find it. Hanken has
  no hair/thin space, prime or ʻokina: no letter-spacing, `'` for minutes, `‘` for the ʻokina.
- The logo is one unit (mark + wordmark + tagline). Never crop or split it.
- OSM/Overpass returns HTTP 406 without a User-Agent and rate-limits hard;
  `mod_oceanfill.R` fetches with a UA and caches to disk.
- The VM is the canonical renderer; other machines differ slightly in font metrics.

Basemap data © OpenStreetMap contributors; US Census TIGER/Line; Natural Earth.
