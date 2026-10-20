# mod_site.R — new work sites, self-serve: geocode (US Census), snap the pin to the actual
# building (OSM footprint), derive the extents from a scale preset, fetch + cache the OSM
# the renderer needs, and write configs/<id>.R so the site renders like the offices.
# Used by scripts/new_site.R (CLI the web app runs in the background). Defines functions only.
suppressPackageStartupMessages({ library(sf); library(jsonlite) })
`%||%` <- function(a, b) if (is.null(a)) b else a

SITE_ROOT <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
SITE_UA   <- "sw-maps-site/1.0 (Shipwright location maps; contact j@djinna.com)"
SITE_CACHE <- file.path(SITE_ROOT, "outputs/location-maps")

# Ground widths in metres. The main map is refit to each frame's aspect by comp_map.R;
# the inset view is ~1.4 w:h like the offices'. place classes = what gets a label.
SITE_PRESETS <- list(
  site   = list(label = "Site",   hint = "the yard or pier and its streets, about 3 km across",
                main_m = 3000,  inset_m = 400,
                places = c("suburb", "neighbourhood", "quarter", "village", "hamlet", "island", "islet")),
  harbor = list(label = "Harbor", hint = "the harbor and approaches, about 12 km across",
                main_m = 12000, inset_m = 1000,
                places = c("city", "town", "suburb", "village", "island", "hamlet")),
  region = list(label = "Region", hint = "the coast and nearby towns, about 45 km across",
                main_m = 45000, inset_m = 2000,
                places = c("city", "town", "village", "island"))
)
INSET_ASPECT <- 1.4

# ---- small geometry helpers (lon/lat <-> metres around a point) ---------------------
m_per_deg_lat <- 111320
bbox_around <- function(lon, lat, w_m, h_m) {
  k <- cos(lat * pi / 180)
  c(xmin = lon - w_m / 2 / (m_per_deg_lat * k), ymin = lat - h_m / 2 / m_per_deg_lat,
    xmax = lon + w_m / 2 / (m_per_deg_lat * k), ymax = lat + h_m / 2 / m_per_deg_lat)
}
grow_bbox <- function(bb, f) {
  cx <- mean(bb[c(1, 3)]); cy <- mean(bb[c(2, 4)])
  hw <- (bb[[3]] - bb[[1]]) * f / 2; hh <- (bb[[4]] - bb[[2]]) * f / 2
  c(xmin = cx - hw, ymin = cy - hh, xmax = cx + hw, ymax = cy + hh)
}
# same as comp_map.R's fit_bbox: grow around the centre to a ground aspect
fit_aspect <- function(bb, asp) {
  cx <- mean(bb[c(1, 3)]); cy <- mean(bb[c(2, 4)]); k <- cos(cy * pi / 180)
  gw <- (bb[[3]] - bb[[1]]) * k; gh <- bb[[4]] - bb[[2]]
  if (gw / gh < asp) gw <- gh * asp else gh <- gw / asp
  c(xmin = cx - gw / k / 2, ymin = cy - gh / 2, xmax = cx + gw / k / 2, ymax = cy + gh / 2)
}
union_bbox <- function(a, b) c(xmin = min(a[[1]], b[[1]]), ymin = min(a[[2]], b[[2]]),
                               xmax = max(a[[3]], b[[3]]), ymax = max(a[[4]], b[[4]]))
local_crs <- function(lon, lat) sprintf("+proj=aeqd +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs", lat, lon)
pt_sf <- function(lon, lat) st_sfc(st_point(c(lon, lat)), crs = 4326)
round_bb <- function(bb, d = 5) setNames(round(as.numeric(bb), d), c("xmin", "ymin", "xmax", "ymax"))

# ---- HTTP: Census geocoder ----------------------------------------------------------
.get_json <- function(url, tries = 3) {
  for (i in seq_len(tries)) {
    h <- curl::new_handle(useragent = SITE_UA, timeout = 40, connecttimeout = 15)
    r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200)
      return(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE))
    Sys.sleep(2 * i)
  }
  stop("The Census geocoder did not answer. Try again in a minute.")
}
CENSUS <- "https://geocoding.geo.census.gov/geocoder/geographies/"
CENSUS_Q <- "benchmark=Public_AR_Current&vintage=Current_Current&layers=Counties&format=json"

county_of <- function(geos) {
  cc <- geos$Counties
  if (!length(cc)) return(NULL)
  list(geoid = cc[[1]]$GEOID, name = cc[[1]]$NAME, state = cc[[1]]$STATE)
}

#' Address -> street point. Returns NULL when the geocoder has no match.
census_geocode <- function(address) {
  j <- .get_json(paste0(CENSUS, "onelineaddress?address=", utils::URLencode(address, reserved = TRUE), "&", CENSUS_Q))
  m <- j$result$addressMatches
  if (!length(m)) return(NULL)
  m1 <- m[[1]]
  list(lon = m1$coordinates$x, lat = m1$coordinates$y, matched = m1$matchedAddress,
       side = m1$tigerLine$side %||% NA, tlid = m1$tigerLine$tigerLineId %||% NA,
       county = county_of(m1$geographies), n_matches = length(m),
       others = vapply(m[-1], function(x) x$matchedAddress, ""))
}
#' Coordinates -> county (NULL offshore / outside the US).
census_county <- function(lon, lat) {
  j <- tryCatch(.get_json(sprintf("%scoordinates?x=%.6f&y=%.6f&%s", CENSUS, lon, lat, CENSUS_Q)),
                error = function(e) NULL)
  if (is.null(j)) return(NULL)
  county_of(j$result$geographies)
}

#' "47.6116, -122.3491", "47.6116 -122.3491", "41°07'08\"N 71°30'44\"W", "41 7.1 N 71 30.7 W".
#' Returns c(lon, lat) or NULL if the text is not coordinates. US-only: a positive
#' longitude without an explicit E is read as west.
parse_coords <- function(s) {
  s <- trimws(gsub("[\u2032\u2019']", "'", gsub("[\u2033\u201D\"]", "\"", s)))
  if (!grepl("^[-+0-9.,;°'\" NSEWnsew]+$", s) || !grepl("[0-9]", s)) return(NULL)
  hemi <- regmatches(toupper(s), gregexpr("[NSEW]", toupper(s)))[[1]]
  parts <- trimws(strsplit(toupper(s), "[,;]|(?<=[NSEW])\\s+", perl = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 1) {                         # "47.61 -122.35"
    nums <- regmatches(parts, gregexpr("[-+]?[0-9]+(\\.[0-9]+)?", parts))[[1]]
    if (length(nums) != 2) return(NULL)
    parts <- nums
  }
  if (length(parts) != 2) return(NULL)
  to_dd <- function(p) {
    n <- as.numeric(regmatches(p, gregexpr("[0-9]+(\\.[0-9]+)?", p))[[1]])
    if (!length(n) || length(n) > 3) return(NA)
    v <- n[1] + (if (length(n) > 1) n[2] / 60 else 0) + (if (length(n) > 2) n[3] / 3600 else 0)
    if (grepl("^\\s*-", p) || grepl("[SW]", p)) -v else v
  }
  v <- vapply(parts, to_dd, 0)
  if (anyNA(v)) return(NULL)
  lat <- v[1]; lon <- v[2]
  if (length(hemi) == 2 && hemi[1] %in% c("E", "W")) { lat <- v[2]; lon <- v[1] }
  if (lon > 0 && !"E" %in% hemi) lon <- -lon
  if (abs(lat) > 90 || abs(lon) > 180) return(NULL)
  c(lon = unname(lon), lat = unname(lat))
}

# ---- Overpass: UA, exponential backoff, endpoint failover ----------------------------
OVERPASS <- c("https://overpass-api.de/api/interpreter",
              "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
              "https://overpass.kumi.systems/api/interpreter",
              "https://overpass.private.coffee/api/interpreter")
#' POST an Overpass query; returns the path of the response. Rejects partial answers
#' (Overpass reports a timeout inside an otherwise valid document as a <remark>).
overpass <- function(query, out = tempfile(fileext = ".osm"), tries = 8, json = FALSE) {
  ok <- function(f) {
    if (!file.exists(f) || file.info(f)$size < 50) return(FALSE)
    head <- readChar(f, min(file.info(f)$size, 4000))
    if (grepl("runtime error|<remark>|\"remark\"", head)) return(FALSE)
    if (json) grepl("\"elements\"", head) else grepl("<osm", head)
  }
  for (i in seq_len(tries)) {
    ep <- OVERPASS[(i - 1) %% length(OVERPASS) + 1]
    h <- curl::new_handle(useragent = SITE_UA, timeout = 180, connecttimeout = 20,
                          postfields = paste0("data=", utils::URLencode(query, reserved = TRUE)))
    r <- tryCatch(curl::curl_fetch_disk(ep, out, handle = h), error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200 && ok(out)) {
      message(sprintf("overpass: %s (%.0f KB)", sub("^https://([^/]+).*", "\\1", ep), file.info(out)$size / 1024))
      return(out)
    }
    message(sprintf("overpass: %s failed (%s), retrying", sub("^https://([^/]+).*", "\\1", ep),
                    if (is.null(r)) "no answer" else r$status_code))
    Sys.sleep(min(2^i, 30))
  }
  stop("OpenStreetMap (Overpass) is busy and did not answer after several tries. Try again in a few minutes.")
}
ovp_bb <- function(bb) sprintf("%.6f,%.6f,%.6f,%.6f", bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]])

# ---- OSM parsing ----------------------------------------------------------------------
# All polygon features of an osmdata_sf object, with the tags snapping cares about.
TAGS <- c("name", "building", "man_made", "leisure", "landuse", "craft", "industrial", "amenity",
          "shop", "power", "waterway", "seamark:type", "addr:housenumber", "addr:street", "natural")
osm_features <- function(o) {
  take <- function(x, type, geomtype) {
    if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(NULL)
    x <- st_make_valid(x)
    d <- data.frame(osm = paste0(type, "/", x$osm_id), stringsAsFactors = FALSE)
    for (t in TAGS) d[[t]] <- if (t %in% names(x)) as.character(x[[t]]) else NA_character_
    d$geomtype <- geomtype
    st_sf(d, geometry = st_geometry(x))
  }
  parts <- list(take(o$osm_polygons, "way", "polygon"), take(o$osm_multipolygons, "relation", "polygon"),
                take(o$osm_points[!is.na(o$osm_points$name %||% NA) |
                                  !is.na(o$osm_points$`addr:housenumber` %||% NA) |
                                  !is.na(o$osm_points$power %||% NA) |
                                  !is.na(o$osm_points$man_made %||% NA), ], "node", "point"))
  parts <- parts[!vapply(parts, is.null, TRUE)]
  if (!length(parts)) return(NULL)
  f <- do.call(rbind, parts)
  # relations come back as both the relation and its outer ways; keep the relation
  f[!duplicated(f$osm), ]
}

# "2225 ALASKAN WAY, SEATTLE, WA, 98121" -> "2225 Alaskan Way, Seattle, WA 98121"
pretty_address <- function(m) {
  p <- trimws(strsplit(m, ",")[[1]])
  cap <- function(x) gsub("\\b([a-z])", "\\U\\1", tolower(x), perl = TRUE)
  if (length(p) == 4) sprintf("%s, %s, %s %s", cap(p[1]), cap(p[2]), p[3], p[4]) else cap(m)
}

# ---- matching ---------------------------------------------------------------------
STREET_ABBR <- c(st = "street", ave = "avenue", av = "avenue", blvd = "boulevard", rd = "road",
                 ln = "lane", dr = "drive", hwy = "highway", ct = "court", pl = "place", pkwy = "parkway",
                 ter = "terrace", cir = "circle", sq = "square", n = "north", s = "south", e = "east",
                 w = "west", ne = "northeast", nw = "northwest", se = "southeast", sw = "southwest")
norm_street <- function(s) {
  w <- strsplit(gsub("[^a-z0-9 ]", " ", tolower(s %||% "")), " +")[[1]]
  w <- w[nzchar(w)]
  w <- ifelse(w %in% names(STREET_ABBR), STREET_ABBR[w], w)
  paste(w, collapse = " ")
}
# "2225 ALASKAN WAY, SEATTLE, WA, 98121" -> number "2225", street "alaskan way"
split_address <- function(matched) {
  first <- trimws(strsplit(matched %||% "", ",")[[1]][1])
  m <- regmatches(first, regexec("^([0-9]+[A-Z]?)\\s+(.*)$", toupper(first)))[[1]]
  if (length(m) < 3) return(NULL)
  list(number = m[2], street = norm_street(m[3]))
}
GENERIC <- c("the", "and", "of", "at", "on", "site", "shipwright", "inc", "llc", "co", "company",
             "pier", "dock", "wharf", "terminal", "marina", "yard", "boat", "boatyard", "harbor",
             "harbour", "building", "shipyard", "street", "st", "avenue", "road", "port", "berth",
             "north", "south", "east", "west", "new", "office")
tokens <- function(s) { w <- strsplit(gsub("[^a-z0-9 ]", " ", tolower(s %||% "")), " +")[[1]]; w[nzchar(w)] }
#' How well an OSM name matches the title: share of the name's words found in the title.
#' Every distinctive word of the name (not "pier", "street"...) must be in the title, and
#' there must be at least one: "Bell Street Park" does not match "Bell Street Cruise Terminal".
name_score <- function(name, title) {
  n <- tokens(name); t <- tokens(title)
  if (!length(n) || !length(t)) return(0)
  hit <- n %in% t; distinct <- !(n %in% GENERIC)
  if (!any(distinct) || !all(hit[distinct])) return(0)
  mean(hit)
}

# ---- locate: candidates for the pin ------------------------------------------------
#' Fetch OSM around a point and rank candidate targets. `geo` is census_geocode()'s
#' result (or NULL for coordinates). Returns list(features, candidates, osm_file).
site_locate <- function(lon, lat, title = "", geo = NULL, radius = 350, name_radius = 1500, cache = NULL) {
  bb <- bbox_around(lon, lat, 2 * radius * 1.6, 2 * radius * 1.6)
  addr <- if (!is.null(geo)) split_address(geo$matched) else NULL
  name_toks <- setdiff(tokens(title), c(GENERIC, "and"))
  name_re <- if (length(name_toks)) paste(name_toks, collapse = "|") else NULL
  q <- paste0("[out:xml][timeout:120];(",
    sprintf("nwr[building](%s);", ovp_bb(bb)),
    sprintf("nwr[man_made~\"^(pier|offshore_platform|lighthouse|breakwater|quay)$\"](%s);", ovp_bb(bb)),
    sprintf("nwr[natural=water](%s);way[natural=coastline](%s);", ovp_bb(bb), ovp_bb(grow_bbox(bb, 1.5))),
    sprintf("node[power=generator](around:%d,%.6f,%.6f);", radius, lat, lon),
    if (!is.null(addr)) sprintf("nwr[\"addr:housenumber\"=\"%s\"](around:800,%.6f,%.6f);", addr$number, lat, lon) else "",
    if (!is.null(name_re)) sprintf("nwr[name~\"%s\",i][~\"^(building|man_made|leisure|craft|industrial|landuse|amenity|shop|waterway|harbour|seamark:type)$\"~\".\"](around:%d,%.6f,%.6f);",
                                   name_re, name_radius, lat, lon) else "",
    ");(._;>;);out body;")
  f <- overpass(q, out = cache %||% tempfile(fileext = ".osm"))
  o <- suppressWarnings(osmdata::osmdata_sf(doc = f))
  feats <- osm_features(o)
  list(features = feats, coast = .of_coast_from_osm(o), bbox = bb,
       candidates = rank_candidates(feats, lon, lat, title, addr, coords_input = is.null(geo), radius = radius))
}

#' Where to put the pin on a footprint: its centroid if that falls inside, else a
#' point guaranteed inside (L-shaped sheds, piers).
pin_on <- function(g, crs_m) {
  gm <- st_transform(g, crs_m)
  c0 <- suppressWarnings(st_centroid(gm))
  p <- if (lengths(st_within(c0, gm))[1] > 0) c0 else suppressWarnings(st_point_on_surface(gm))
  st_coordinates(st_transform(p, 4326))[1, 1:2]
}

rank_candidates <- function(feats, lon, lat, title, addr, coords_input, radius = 350, max_n = 6) {
  crs_m <- local_crs(lon, lat)
  P <- st_transform(pt_sf(lon, lat), crs_m)
  if (is.null(feats) || !nrow(feats)) return(data.frame())
  fm <- st_transform(feats, crs_m)
  is_poly <- feats$geomtype == "polygon"
  is_bldg <- is_poly & !is.na(feats$building) & feats$building != "no"
  is_pier <- is_poly & !is.na(feats$man_made) & feats$man_made %in% c("pier", "offshore_platform", "quay")
  target_ok <- is_bldg | is_pier
  dist <- as.numeric(st_distance(fm, P))[seq_len(nrow(fm))]
  out <- list()
  add <- function(i, score, reason) {
    out[[length(out) + 1]] <<- data.frame(i = i, score = score, reason = reason, stringsAsFactors = FALSE)
  }
  # building (or pier deck) that holds a point feature: the node is often the business, the
  # footprint around it is what we pin
  holder <- function(j) {
    inside <- which(target_ok & lengths(st_intersects(fm, fm[j, ])) > 0)
    if (length(inside)) return(inside[which.min(as.numeric(st_area(fm[inside, ])))])
    d <- as.numeric(st_distance(fm[target_ok, ], fm[j, ]))
    if (length(d) && min(d) <= 40) which(target_ok)[which.min(d)] else j
  }
  # buildings inside a named area (marina, boatyard landuse): the biggest one
  biggest_in <- function(j) {
    inside <- which(is_bldg & lengths(st_within(fm, fm[j, ])) > 0)
    if (length(inside)) inside[which.max(as.numeric(st_area(fm[inside, ])))] else j
  }
  # 1. coordinates typed or clicked inside a footprint
  if (coords_input) {
    inside <- which(target_ok & lengths(st_intersects(fm, P)) > 0)
    if (length(inside)) add(inside[which.min(as.numeric(st_area(fm[inside, ])))], 4, "contains the coordinates")
  }
  # 2. OSM address equals the geocoded address
  if (!is.null(addr)) {
    hit <- which(!is.na(feats$`addr:housenumber`) & toupper(feats$`addr:housenumber`) == addr$number &
                 vapply(feats$`addr:street`, norm_street, "") == addr$street)
    for (j in hit) add(if (feats$geomtype[j] == "point") holder(j) else j, 3.5, "OSM address matches")
  }
  # 3. a named feature that matches the title
  ns <- vapply(feats$name, name_score, 0, title = title)
  for (j in which(ns >= 0.6)) {
    k <- if (feats$geomtype[j] == "point") holder(j) else if (!target_ok[j]) biggest_in(j) else j
    add(k, 2 + ns[j], sprintf("name matches the title (%s)", feats$name[j]))
  }
  # 4. nearest footprints / structures
  near <- which((target_ok | (feats$geomtype == "point" & (!is.na(feats$power) | !is.na(feats$man_made)))) & dist <= radius)
  near <- near[order(dist[near])][seq_len(min(5, length(near)))]
  for (j in near) add(j, 1 - dist[j] / (radius + 1), sprintf("nearest, %d m away", round(dist[j])))
  if (!length(out)) return(data.frame())
  cand <- do.call(rbind, out)
  cand <- cand[order(-cand$score), ]
  reasons <- tapply(cand$reason, cand$i, function(r) paste(unique(r), collapse = "; "))
  cand <- cand[!duplicated(cand$i), ]
  cand$reason <- unname(reasons[as.character(cand$i)])
  cand <- head(cand, max_n)
  pins <- t(vapply(cand$i, function(j)
    if (feats$geomtype[j] == "point") st_coordinates(feats[j, ])[1, 1:2] else pin_on(feats[j, ], crs_m), c(0, 0)))
  # columns in a fixed order (repin prepends a clicked point with the same columns)
  out_cols <- c("rank", "osm", "name", "kind", "footprint", "lon", "lat", "dist_m", "reason")
  data.frame(rank = seq_len(nrow(cand)), osm = feats$osm[cand$i],
             footprint = target_ok[cand$i],
             name = ifelse(is.na(feats$name[cand$i]), "", feats$name[cand$i]),
             kind = describe(feats[cand$i, ]),
             lon = pins[, 1], lat = pins[, 2], dist_m = round(dist[cand$i]),
             reason = cand$reason, stringsAsFactors = FALSE)[, out_cols]
}
describe <- function(f) {
  v <- ifelse(!is.na(f$man_made), gsub("_", " ", f$man_made),
       ifelse(!is.na(f$building) & f$building != "yes", paste(gsub("_", " ", f$building), "building"),
       ifelse(!is.na(f$building), "building",
       ifelse(!is.na(f$power), paste("power", f$power), ifelse(!is.na(f$craft), f$craft, "feature")))))
  ifelse(f$geomtype == "point" & is.na(f$building), paste(v, "(point, no footprint)"), v)
}

# ---- extents from the preset --------------------------------------------------------
#' main_bbox (centred on the pin), inset_view (~1.4:1, grown to hold the whole target
#' footprint), inset_bbox (what the OSM detail is cached for) and coast_bbox (covers the
#' main map at every frame aspect the styles and sizes produce: 0.85 to 2.1).
site_extents <- function(lon, lat, preset, footprint = NULL) {
  p <- SITE_PRESETS[[preset]]
  main <- bbox_around(lon, lat, p$main_m, p$main_m / INSET_ASPECT)
  w <- p$inset_m
  if (!is.null(footprint)) {
    fb <- st_bbox(st_transform(footprint, local_crs(lon, lat)))
    need <- max(2 * max(abs(fb[c("xmin", "xmax")])), 2 * max(abs(fb[c("ymin", "ymax")])) * INSET_ASPECT) * 1.5
    w <- max(w, need)
  }
  view <- bbox_around(lon, lat, w, w / INSET_ASPECT)
  if (w > p$main_m / 4) main <- bbox_around(lon, lat, 4 * w, 4 * w / INSET_ASPECT)
  coast <- grow_bbox(union_bbox(fit_aspect(main, 0.85), fit_aspect(main, 2.1)), 1.02)
  list(main_bbox = round_bb(main), inset_view = round_bb(view), inset_bbox = round_bb(grow_bbox(view, 1.6)),
       coast_bbox = round_bb(coast), inset_m = round(w))
}

# ---- TIGER counties covering the map ----------------------------------------------
site_counties <- function(bb, lon, lat) {
  suppressPackageStartupMessages(library(tigris)); options(tigris_use_cache = TRUE)
  cs <- suppressMessages(st_transform(tigris::counties(cb = TRUE, year = 2022, progress_bar = FALSE), 4326))
  box <- st_as_sfc(st_bbox(bb, crs = 4326))
  hit <- cs[lengths(st_intersects(cs, box)) > 0, ]
  dist <- function(x) as.numeric(st_distance(st_transform(x, local_crs(lon, lat)), st_transform(pt_sf(lon, lat), local_crs(lon, lat))))
  if (!nrow(hit)) hit <- cs[which.min(dist(cs)), ]   # offshore: the nearest county
  home <- hit[which.min(dist(hit)), ]
  list(home = home$GEOID, home_name = paste0(home$NAMELSAD, ", ", home$STUSPS),
       state = home$STUSPS, all = sort(hit$GEOID))
}

# ---- inset OSM cache (same shape as florida-osm.rds) ------------------------------
subset_osm <- function(o, col, value = NULL) {
  parts <- c("osm_points", "osm_lines", "osm_polygons", "osm_multilines", "osm_multipolygons")
  ans <- setNames(vector("list", length(parts)), parts)
  for (nm in parts) {
    x <- o[[nm]]
    if (is.null(x) || !inherits(x, "sf") || !nrow(x)) { ans[[nm]] <- x; next }
    keep <- if (!col %in% names(x)) rep(FALSE, nrow(x))
            else if (is.null(value)) !is.na(x[[col]]) & nzchar(x[[col]]) & x[[col]] != "no"
            else !is.na(x[[col]]) & x[[col]] == value
    ans[[nm]] <- x[keep, , drop = FALSE]
  }
  ans
}
# Point structures that matter offshore -- wind turbines, platforms, lighthouses -- as an sf of
# points (kind, name). comp_map.R draws them in the inset when the cache has them.
site_structures <- function(o) {
  pts <- o$osm_points
  if (is.null(pts) || !nrow(pts)) return(NULL)
  col <- function(n) if (n %in% names(pts)) as.character(pts[[n]]) else rep(NA_character_, nrow(pts))
  kind <- ifelse(!is.na(col("power")) & col("power") == "generator", "turbine",
          ifelse(col("man_made") %in% c("offshore_platform", "lighthouse"), col("man_made"), NA))
  keep <- !is.na(kind)
  if (!any(keep)) return(NULL)
  st_sf(kind = kind[keep], name = col("name")[keep], geometry = st_geometry(st_transform(pts[keep, ], 4326)))
}

site_fetch_inset <- function(id, bb) {
  q <- paste0("[out:xml][timeout:180];(",
    sprintf("nwr[building](%1$s);nwr[natural=water](%1$s);nwr[man_made=pier](%1$s);way[natural=coastline](%1$s);", ovp_bb(bb)),
    sprintf("node[power=generator](%1$s);node[man_made~\"^(offshore_platform|lighthouse)$\"](%1$s);", ovp_bb(bb)),
    ");(._;>;);out body;")
  f <- overpass(q)
  on.exit(unlink(f))
  o <- suppressWarnings(osmdata::osmdata_sf(doc = f))
  cache <- list(water = subset_osm(o, "natural", "water"), buildings = subset_osm(o, "building"),
                piers = subset_osm(o, "man_made", "pier"), coast = subset_osm(o, "natural", "coastline"),
                structures = site_structures(o),
                bbox = bb, fetched_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  # the coastline for the inset goes where mod_oceanfill looks for it, so renders stay offline
  cl <- .of_coast_from_osm(o)
  saveRDS(if (is.null(cl)) st_sfc(crs = 4326) else cl,   # empty = no coastline (offshore), cached too
            file.path(SITE_CACHE, paste0("coastline-", .of_bbox_key(c(xmin = bb[[1]], ymin = bb[[2]], xmax = bb[[3]], ymax = bb[[4]])), ".rds")))
  rds <- paste0(id, "-osm.rds")
  saveRDS(cache, file.path(SITE_CACHE, rds))
  n <- function(l) sum(vapply(c("osm_polygons", "osm_multipolygons"), function(p) nrow(cache[[l]][[p]] %||% data.frame()), 0))
  message(sprintf("inset OSM: %d buildings, %d water, %d piers, %d structures -> %s", n("buildings"), n("water"),
                  n("piers"), NROW(cache$structures), rds))
  rds
}

# ---- place labels for the main map ------------------------------------------------
PLACE_RANK <- c(city = 1, town = 2, bay = 2.5, strait = 2.5, suburb = 3, island = 3.5, village = 4,
                quarter = 4.5, neighbourhood = 5, hamlet = 6, islet = 6)
site_labels <- function(main, coast, lon, lat, preset, view, max_n = 5) {
  places <- SITE_PRESETS[[preset]]$places
  q <- sprintf(paste0("[out:json][timeout:90];(node[place~\"^(%s)$\"][name](%s);",
                      "nwr[natural~\"^(bay|strait)$\"][name](%s);nwr[place~\"^(island|islet)$\"][name](%s););out tags center;"),
               paste(places, collapse = "|"), ovp_bb(main), ovp_bb(main), ovp_bb(main))
  f <- tryCatch(overpass(q, json = TRUE), error = function(e) NULL)
  if (is.null(f)) return(data.frame(name = character(), lon = numeric(), lat = numeric()))
  el <- jsonlite::fromJSON(f, simplifyVector = FALSE)$elements; unlink(f)
  d <- do.call(rbind, lapply(el, function(e) {
    c0 <- e$center %||% list(lat = e$lat, lon = e$lon)
    cls <- e$tags$natural %||% e$tags$place
    data.frame(name = e$tags$name, cls = cls, lon = c0$lon, lat = c0$lat,
               pop = suppressWarnings(as.numeric(gsub("[^0-9]", "", e$tags$population %||% "0"))),
               stringsAsFactors = FALSE)
  }))
  if (is.null(d) || !nrow(d)) return(data.frame(name = character(), lon = numeric(), lat = numeric()))
  # numbered names ("Cove 3", "Pier 91") are marina basins and berths, not map labels
  d <- d[d$cls %in% c(places, "bay", "strait") & !duplicated(d$name) & !grepl("[0-9]", d$name), ]
  d$pop[is.na(d$pop)] <- 0
  d <- d[order(PLACE_RANK[d$cls], -d$pop), ]
  # spread out: keep a label only if it is clear of the pin, the inset extent box and the others
  k <- cos(lat * pi / 180); W <- (main[[3]] - main[[1]]) * k; H <- main[[4]] - main[[2]]
  gx <- function(x) (x - main[[1]]) * k / W; gy <- function(y) (y - main[[2]]) / H
  inside <- gx(d$lon) > 0.08 & gx(d$lon) < 0.92 & gy(d$lat) > 0.1 & gy(d$lat) < 0.9
  d <- d[inside, ]
  keep <- integer()
  for (i in seq_len(nrow(d))) {
    x <- gx(d$lon[i]); y <- gy(d$lat[i])
    if (abs(x - gx(lon)) < 0.06 && abs(y - gy(lat)) < 0.06) next
    if (x > gx(view[[1]]) - 0.04 && x < gx(view[[3]]) + 0.04 && y > gy(view[[2]]) - 0.04 && y < gy(view[[4]]) + 0.04) next
    if (any(sqrt((x - gx(d$lon[keep]))^2 + (y - gy(d$lat[keep]))^2) < 0.22)) next
    keep <- c(keep, i)
    if (length(keep) >= max_n) break
  }
  d <- d[keep, ]
  # long names on two lines
  wrap <- function(s) {
    if (nchar(s) <= 14 || !grepl(" ", s)) return(s)
    sp <- gregexpr(" ", s)[[1]]; b <- sp[which.min(abs(sp - nchar(s) / 2))]
    paste0(substr(s, 1, b - 1), "\n", substr(s, b + 1, nchar(s)))
  }
  data.frame(name = vapply(d$name, wrap, ""), lon = round(d$lon, 5), lat = round(d$lat, 5), stringsAsFactors = FALSE)
}

# ---- terrain: relief worth showing? -------------------------------------------------
site_relief <- function(bb) {
  r <- tryCatch({
    poly <- st_sf(geometry = st_as_sfc(st_bbox(bb, crs = 4326)))
    dem <- suppressMessages(elevatr::get_elev_raster(poly, z = 8, clip = "bbox", verbose = FALSE))
    v <- terra::values(terra::rast(dem), mat = FALSE); v <- v[is.finite(v) & v > 0]
    if (length(v) < 50) 0 else as.numeric(stats::quantile(v, 0.98) - stats::quantile(v, 0.02))
  }, error = function(e) NA)
  r
}

# Inset caption: the pinned building's name if it fits a half-page inset (27 characters of
# small caps); if the name starts with the title, which the figure already shows, just the
# rest ("Brooklin Boat Yard Main Shop" -> "Main Shop"); offshore points say so.
inset_caption <- function(name, title, offshore = FALSE) {
  if (!nzchar(name %||% "")) return(if (offshore) "Offshore" else "")
  rest <- if (startsWith(tolower(name), tolower(title))) trimws(substring(name, nchar(title) + 1)) else ""
  if (nzchar(rest) && nchar(rest) <= 27) return(rest)
  if (nchar(name) <= 27) name else ""
}

# ---- config writer -----------------------------------------------------------------
slugify <- function(s) {
  s <- gsub("[^a-z0-9]+", "-", tolower(iconv(s, to = "ASCII//TRANSLIT")))
  s <- gsub("^-+|-+$", "", s)
  substr(if (nzchar(s)) s else "site", 1, 40)
}
unique_id <- function(s) {
  base <- slugify(s); id <- base; k <- 2
  while (file.exists(file.path(SITE_ROOT, "configs", paste0(id, ".R")))) { id <- paste0(base, "-", k); k <- k + 1 }
  id
}
fmt_bb <- function(bb, d = 5) sprintf("c(xmin = %.*f, ymin = %.*f, xmax = %.*f, ymax = %.*f)",
                                      d, bb[[1]], d, bb[[2]], d, bb[[3]], d, bb[[4]])
rq <- function(s) deparse(enc2utf8(as.character(s %||% "")))

write_site_config <- function(s) {
  lab <- s$labels
  labels_r <- if (nrow(lab)) sprintf(
'  labels = data.frame(
    name = c(%s),
    lon  = c(%s),
    lat  = c(%s),
    stringsAsFactors = FALSE
  )', paste(vapply(lab$name, rq, ""), collapse = ", "), paste(sprintf("%.5f", lab$lon), collapse = ", "),
    paste(sprintf("%.5f", lab$lat), collapse = ", "))
  else '  labels = data.frame(name = character(), lon = numeric(), lat = numeric())'
  txt <- sprintf(
'# %s — work site config, generated by scripts/new_site.R on %s.
# Defines `office` for scripts/comp_map.R. Input: %s
# Pin: %s
office <- list(
  kind     = "site",
  added    = %s,
  added_by = %s,
  slug     = %s,
  name     = %s,
  subtitle = %s,
  eyebrow  = "SITE LOCATION",
  lon      = %.7f,
  lat      = %.7f,
  target_osm = %s,                 # footprint the pin sits in; checked by scripts/check_site.R
  geocoded   = %s,
  preset     = %s,
  main_bbox  = %s,
  # inset_view = the close-up (~1.4 w:h); inset_bbox = what its OSM detail is cached for;
  # coast_bbox = OSM coastline land for the main map at every frame aspect.
  inset_bbox = %s,
  inset_view = %s,
  coast_bbox = %s,
  osm_rds       = %s,
  inset_caption = %s,
  use_terrain   = %s,              # relief across the main map: %s m
  tiger_state   = %s,
  tiger_county  = %s,
  tiger_counties = c(%s),
  tiger_year    = 2022,
%s
)
',
    s$title, format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"), s$input_note, s$pin_note,
    rq(format(Sys.Date())), rq(s$added_by), rq(s$id), rq(s$title), rq(s$subtitle), s$lon, s$lat, rq(s$target_osm),
    if (is.null(s$geocoded)) "NULL" else sprintf("c(lon = %.7f, lat = %.7f)", s$geocoded[["lon"]], s$geocoded[["lat"]]),
    rq(s$preset), fmt_bb(s$ext$main_bbox), fmt_bb(s$ext$inset_bbox), fmt_bb(s$ext$inset_view), fmt_bb(s$ext$coast_bbox),
    rq(s$osm_rds), rq(s$caption), if (isTRUE(s$use_terrain)) "TRUE" else "FALSE",
    if (is.na(s$relief)) "unknown" else round(s$relief),
    rq(substr(s$counties$home, 1, 2)), rq(substr(s$counties$home, 3, 5)),
    paste(vapply(s$counties$all, rq, ""), collapse = ", "), labels_r)
  path <- file.path(SITE_ROOT, "configs", paste0(s$id, ".R"))
  writeLines(txt, path, useBytes = TRUE)
  invisible(parse(path))      # a config that does not parse would break the whole page
  path
}
