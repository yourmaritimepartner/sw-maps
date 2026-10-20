# sw-maps web front: office maps in three styles and three sizes, rendered on demand,
# plus design comps and review notes. Started by app/serve.R; rendering shells out to
# scripts/comp_map.R so each render gets a clean R session (the modules define globals).

ROOT    <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
CONFIGS <- file.path(ROOT, "configs")
OUT     <- file.path(ROOT, "outputs/location-maps")   # legacy location_map.R renders
MAPS    <- file.path(ROOT, "outputs/maps")            # comp_map.R renders served by the app

STYLES <- c(drawing  = "B · Drawing",                 # first = default
            platform = "A · Platform",
            mainsite = "C · Main site")
SIZES  <- c(report = "Report · 6.5 × 4.6 in",
            half   = "Half page · 3.15 × 3.6 in",
            slide  = "Slide · 16:9")
FORMATS <- c(PNG = "png", PDF = "pdf", SVG = "svg")
pick <- function(x, choices) if (!is.null(x) && x %in% names(choices)) x else names(choices)[1]

office_ids <- function() sub("\\.R$", "", list.files(CONFIGS, "\\.R$"))
office_cfg <- function(id) {
  env <- new.env()
  sys.source(file.path(CONFIGS, paste0(id, ".R")), envir = env)
  env$office
}
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE);  gsub("\"", "&quot;", x, fixed = TRUE)
}
last_error <- new.env()   # id -> tail of the failed render's log

# Images that take review notes carry data-note-target (see app/static/notes.js).
note_img <- function(href, src, alt, target, lazy = FALSE)
  sprintf('<a href="%s"><img src="%s" alt="%s" data-note-target="%s"%s></a>',
          href, src, esc(alt), esc(target), if (lazy) ' loading="lazy"' else "")

card <- function(id, style, size) {
  o    <- office_cfg(id)
  stem <- sprintf("%s-%s-%s", id, style, size)
  png  <- file.path(MAPS, paste0(stem, ".png"))
  have <- file.exists(png)
  v    <- if (have) as.integer(file.mtime(png)) else 0L
  when <- if (have) format(file.mtime(png), "Rendered %Y-%m-%d %H:%M UTC", tz = "UTC") else "Not rendered yet"
  links <- vapply(names(FORMATS), function(f) {
    fn <- paste0(stem, ".", FORMATS[[f]])
    if (file.exists(file.path(MAPS, fn))) sprintf('<a href="/maps-files/%s?v=%d" download>%s</a>', fn, v, f) else ""
  }, "")
  err <- if (!is.null(last_error[[stem]])) sprintf('<pre class="err">%s</pre>', esc(last_error[[stem]])) else ""
  sprintf(
'<section id="%s">
  <header><h2>%s</h2><p>%s</p>%s</header>
  %s
  <div class="row">
    <form method="post" action="/render/%s?style=%s&amp;size=%s" onsubmit="this.querySelector(\'button\').disabled=true;this.querySelector(\'button\').textContent=\'Rendering… about 30 s\'">
      <button>%s</button></form>
    <span class="when">%s</span>
    <span class="dl">%s</span>
  </div>%s
</section>',
    id, esc(o$name), esc(o$subtitle), site_line(id, o),
    if (have) note_img(sprintf("/view?f=%s.png", stem), sprintf("/maps-files/%s.png?v=%d", stem, v),
                       paste(o$name, STYLES[[style]], SIZES[[size]], sep = ", "), paste0(stem, ".png"))
    else '<div class="empty">No render for this style and size yet. Render it below.</div>',
    id, style, size, if (have) "Re-render" else "Render", when, paste(links, collapse = ""), err)
}

switcher <- function(label, param, choices, current, other) {
  items <- vapply(names(choices), function(k) {
    q <- if (param == "style") sprintf("?style=%s&amp;size=%s", k, other) else sprintf("?style=%s&amp;size=%s", other, k)
    sprintf('<a href="/%s"%s>%s</a>', q, if (k == current) ' aria-current="true"' else "", esc(choices[[k]]))
  }, "")
  sprintf('<div class="switch"><span>%s</span>%s</div>', label, paste(items, collapse = ""))
}

STYLE <- '
@font-face{font-family:"Hanken Grotesk";font-style:normal;font-weight:400 800;font-display:swap;
src:url("/static/fonts/hanken-grotesk-var-latin.woff2") format("woff2")}
:root{--ink:#101820;--gold:#F2C14E;--paper:#F7F5F0;--line:#D9D4C8;--muted:#5A6570;--accent:#015686}
*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);position:relative;
font:16px/1.45 "Hanken Grotesk",system-ui,-apple-system,"Segoe UI",Arial,sans-serif}
.switches{display:flex;flex-wrap:wrap;gap:10px 28px;margin:-12px 0 28px}
.switch{display:flex;flex-wrap:wrap;align-items:center;gap:6px}
.switch span{font-size:13px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted);margin-right:4px}
.switch a{padding:5px 12px;border:1px solid var(--line);border-radius:4px;background:#fff;text-decoration:none;font-size:15px}
.switch a[aria-current]{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:600}
.empty{padding:48px 16px;text-align:center;color:var(--muted);border:1px dashed var(--line);border-radius:4px}
main{max-width:1040px;margin:0 auto;padding:32px 16px 96px}
main.wide{max-width:none}
h1{font-size:28px;margin:0 0 4px}h1+p{margin:0 0 32px;color:var(--muted)}
a{color:var(--ink)}
section{background:#fff;border:1px solid var(--line);border-radius:6px;padding:20px;margin:0 0 28px}
h2{margin:0;font-size:20px}header p{margin:2px 0 14px;color:var(--muted)}
img{display:block;width:100%;height:auto;border:1px solid var(--line)}
.row{display:flex;flex-wrap:wrap;gap:12px 20px;align-items:center;margin-top:14px}
button{font:inherit;font-weight:600;background:var(--ink);color:#fff;border:0;border-radius:4px;padding:8px 16px;cursor:pointer}
button:disabled{background:var(--muted);cursor:wait}
.when{color:var(--muted);font-size:14px}.dl{display:flex;gap:14px;margin-left:auto}
.dl a{font-weight:600;text-decoration:none;border-bottom:2px solid var(--gold)}
.err{white-space:pre-wrap;background:#FFF4F2;border:1px solid #E8B4AA;padding:12px;font-size:13px;margin:14px 0 0}
figure{margin:0 0 22px}figcaption{color:var(--muted);font-size:14px;margin-top:6px}
table.notes{width:100%;border-collapse:collapse;background:#fff;font-size:15px}
table.notes th{text-align:left;font-size:12px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted);padding:8px 10px;border-bottom:1px solid var(--line)}
table.notes td{padding:10px;border-bottom:1px solid var(--line);vertical-align:top}
table.notes tr:target{background:#FFF8E1}
table.notes .meta{color:var(--muted);font-size:13px}
table.notes select{font:inherit;padding:3px 4px}
h2.group{font-size:13px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted);margin:8px 0 12px;font-weight:600}
a.button{display:inline-block;font-weight:600;background:var(--accent);color:#fff;border-radius:4px;padding:8px 16px;text-decoration:none}
.site-meta{font-size:14px;margin:-8px 0 14px}.site-meta a{color:var(--accent)}
form.site{display:grid;gap:16px}form.site label{display:grid;gap:4px;font-weight:600}
form.site input[type=text],form.site input:not([type]){font:inherit;padding:8px 10px;border:1px solid var(--line);border-radius:4px}
form.site fieldset{border:1px solid var(--line);border-radius:4px;padding:10px 14px;display:grid;gap:8px}
form.site legend{font-size:13px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted);padding:0 4px}
form.site label.opt{display:block;font-weight:400}form.site label.opt b{color:var(--accent)}
form.site button{justify-self:start}
.hint{color:var(--muted);font-weight:400;font-size:14px}.lede{color:var(--muted)}
.working{font-size:18px;font-weight:600}
#preview{margin:12px 0 16px}
.near{background:#FFF8E1;border:1px solid var(--gold);border-radius:4px;padding:10px 14px}
input.filter{font:inherit;width:100%;max-width:420px;padding:8px 10px;border:1px solid var(--line);border-radius:4px;margin:0 0 14px}
table.locations td:nth-child(2),table.locations td:nth-child(6){white-space:nowrap}
button.copy{font-size:13px;font-weight:400;font-family:inherit;background:none;color:var(--ink);border:1px solid var(--line);padding:3px 8px;cursor:copy}
'

shell <- function(title, intro, body, wide = FALSE)
  paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>', esc(title), '</title>
<link rel="icon" href="/favicon.ico" sizes="48x48">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
<link rel="stylesheet" href="/static/notes.css">
<style>', STYLE, '</style></head><body><main', if (wide) ' class="wide"' else '', '>
<h1>', esc(title), '</h1>
<p>', intro, '</p>
', body, '
</main><script src="/static/notes.js" defer></script></body></html>')

#* @get /
#* @serializer html
function(style = NULL, size = NULL) {
  style <- pick(style, STYLES); size <- pick(size, SIZES)
  ids <- office_ids()
  kinds <- vapply(ids, function(i) office_cfg(i)$kind %||% "office", "")
  group <- function(label, which) if (length(which))
    paste0('<h2 class="group">', label, '</h2>', paste(vapply(which, card, "", style = style, size = size), collapse = "\n")) else ""
  shell("Shipwright Maps",
    'Location maps for project reports: our offices and any US work site. Pick a style and size, then download PNG, PDF or SVG. Press N to pin a note. <a href="/sites/new">New work site</a> · <a href="/locations">Locations</a> · <a href="/comps">Design comps</a> · <a href="/notes">Notes</a>',
    paste0('<div class="switches">', switcher("Style", "style", STYLES, style, size),
           switcher("Size", "size", SIZES, size, style), "</div>",
           group("Offices", ids[kinds != "site"]),
           group("Work sites", ids[kinds == "site"]),
           '<p class="newsite"><a class="button" href="/sites/new">New work site</a></p>'))
}

#* @post /render/<id>
function(id, style = NULL, size = NULL, res) {
  if (!id %in% office_ids()) { res$status <- 404L; res$body <- "Unknown office"; return(res) }
  style <- pick(style, STYLES); size <- pick(size, SIZES)
  stem <- sprintf("%s-%s-%s", id, style, size)
  log <- suppressWarnings(system2("Rscript", c(file.path(ROOT, "scripts/comp_map.R"), id, style, size, "outputs/maps"),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status")
  if (!is.null(status) && status != 0) last_error[[stem]] <- paste(tail(log, 25), collapse = "\n")
  else if (exists(stem, envir = last_error, inherits = FALSE)) rm(list = stem, envir = last_error)
  res$status <- 303L
  res$setHeader("Location", sprintf("/?style=%s&size=%s#%s", style, size, id))
  res$body <- ""
  res
}

COMPS <- file.path(ROOT, "outputs/comps")
COMP_STYLES <- c(platform = "A · Platform — BD apps palette, header above the map",
                 drawing  = "B · Drawing — quiet grey base, neatline ticks, title block (app default)",
                 mainsite = "C · Main site — horizontal logo plate, fuller sea and relief")

#* Design comps (scripts/comp_map.R), grouped by office.
#* @get /comps
#* @serializer html
function() {
  files <- list.files(COMPS, "\\.png$")
  ids <- unique(sub("-.*$", "", files))
  body <- vapply(ids, function(id) {
    o <- office_cfg(id)
    figs <- vapply(names(COMP_STYLES), function(st) {
      f <- grep(paste0("^", id, "-", st, "-"), files, value = TRUE)
      if (!length(f)) return("")
      v <- as.integer(file.mtime(file.path(COMPS, f[1])))
      sprintf("<figure>%s<figcaption>%s</figcaption></figure>",
              note_img(sprintf("/view?f=%s", f[1]), sprintf("/comp-files/%s?v=%d", f[1], v),
                       paste(o$name, COMP_STYLES[[st]], sep = ", "), f[1], lazy = TRUE),
              esc(COMP_STYLES[[st]]))
    }, "")
    sprintf("<section><header><h2>%s</h2><p>Report full-width, 6.5 in at 300 dpi</p></header>%s</section>",
            esc(o$name), paste(figs, collapse = ""))
  }, "")
  shell("Map design comps",
        'Three directions on the same data, at the size they print. Click a comp to view it full size; press N to pin a note on it. <a href="/">Back to maps</a>',
        paste(body, collapse = "\n"))
}

#* Full-size view of one map or comp, with its notes.
#* @get /view
#* @serializer html
function(f = "", res) {
  f <- basename(f)
  roots <- c("maps-files" = MAPS, "comp-files" = COMPS, files = OUT)
  dir <- names(roots)[file.exists(file.path(roots, f))][1]
  if (is.na(dir) || !grepl("\\.png$", f)) { res$status <- 404L; return("Not found") }
  v <- as.integer(file.mtime(file.path(roots[[dir]], f)))
  shell(f, sprintf('Press N, click a spot, type a note. <a href="%s">Back</a> · <a href="/%s/%s?v=%d" download>Download PNG</a>',
                   if (dir == "comp-files") "/comps" else "/", dir, esc(f), v),
        note_img(sprintf("/%s/%s?v=%d", dir, esc(f), v), sprintf("/%s/%s?v=%d", dir, esc(f), v), f, f),
        wide = TRUE)
}

# ---- review notes (app/static/notes.js): pinned to an image spot, kept in data/notes.json
NOTES <- file.path(ROOT, "data/notes.json")
NOTE_STATUS <- c(open = "Open", decision = "Needs decision", done = "Done")
read_notes <- function() if (file.exists(NOTES)) jsonlite::fromJSON(NOTES, simplifyDataFrame = FALSE) else list()
write_notes <- function(n) {
  dir.create(dirname(NOTES), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(NOTES, ".tmp")
  writeLines(jsonlite::toJSON(n, auto_unbox = TRUE, pretty = TRUE), tmp)
  file.rename(tmp, NOTES)
}
# JSON bodies only: a cross-site form can't send application/json without a CORS preflight.
json_body <- function(req) {
  if (!grepl("application/json", req$HTTP_CONTENT_TYPE %||% "", fixed = TRUE)) return(NULL)
  tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE), error = function(e) NULL)
}
who <- function(req) req$HTTP_X_EXEDEV_EMAIL %||% "local"
now <- function() format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")

#* @get /api/notes
#* @serializer unboxedJSON
function() read_notes()

#* @post /api/notes
#* @serializer unboxedJSON
function(req, res) {
  b <- json_body(req)
  if (is.null(b) || !is.character(b$text) || !nzchar(trimws(b$text)) || !is.numeric(b$x) || !is.numeric(b$y)) {
    res$status <- 400L; return(list(error = "Send JSON with text, x and y."))
  }
  n <- read_notes()
  id <- if (length(n)) max(vapply(n, function(x) as.integer(x$id), 1L)) + 1L else 1L
  note <- list(id = id, page = substr(b$page %||% "/", 1, 300), target = substr(b$target %||% "", 1, 200),
               x = min(max(b$x, 0), 1), y = min(max(b$y, 0), 1), text = substr(trimws(b$text), 1, 2000),
               who = who(req), t = now(), status = "open", resolution = "")
  write_notes(c(n, list(note)))
  res$status <- 201L
  note
}

#* @post /api/notes/<id:int>
#* @serializer unboxedJSON
function(id, req, res) {
  b <- json_body(req); n <- read_notes()
  i <- which(vapply(n, function(x) as.integer(x$id) == id, TRUE))
  if (is.null(b)) { res$status <- 400L; return(list(error = "Send JSON.")) }
  if (!length(i)) { res$status <- 404L; return(list(error = "No such note.")) }
  if (!is.null(b$status) && b$status %in% names(NOTE_STATUS)) n[[i]]$status <- b$status
  if (!is.null(b$resolution)) n[[i]]$resolution <- substr(as.character(b$resolution), 1, 500)
  n[[i]]$updated <- now(); n[[i]]$updated_by <- who(req)
  write_notes(n)
  n[[i]]
}

#* The review queue: every note, newest first, with status.
#* @get /notes
#* @serializer html
function() {
  n <- rev(read_notes())
  rows <- vapply(n, function(x) {
    opts <- paste(sprintf('<option value="%s"%s>%s</option>', names(NOTE_STATUS),
                          ifelse(names(NOTE_STATUS) == x$status, " selected", ""), NOTE_STATUS), collapse = "")
    where <- if (startsWith(x$target, "page:")) esc(sub("^page:", "", x$target)) else esc(x$target)
    sprintf('<tr id="n%d"><td>%d</td><td><a href="%s">%s</a></td><td>%s%s</td><td class="meta">%s<br>%s</td><td><select data-id="%d">%s</select></td></tr>',
            x$id, x$id, esc(x$page), where, esc(x$text),
            if (nzchar(x$resolution %||% "")) sprintf('<div class="meta">→ %s</div>', esc(x$resolution)) else "",
            esc(x$who), esc(x$t), x$id, opts)
  }, "")
  table <- if (length(n)) paste0('<table class="notes"><thead><tr><th>#</th><th>Where</th><th>Note</th><th>Who</th><th>Status</th></tr></thead><tbody>',
                                 paste(rows, collapse = ""), "</tbody></table>",
    "<script>document.querySelectorAll('select[data-id]').forEach(s=>s.onchange=()=>fetch('/api/notes/'+s.dataset.id,",
    "{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({status:s.value})}))</script>")
    else "<p>No notes yet. Open a map or comp, press N, and click the spot you want changed.</p>"
  open <- sum(vapply(n, function(x) x$status == "open", TRUE))
  shell("Review notes", sprintf('%d open of %d. <a href="/">Maps</a> · <a href="/comps">Comps</a>', open, length(n)), table)
}

# ---- new work sites (scripts/new_site.R runs each step in the background) ------------
# A draft is data/drafts/<token>/: input.json, status.json, draft.json, preview.png, logs.
DRAFTS <- file.path(ROOT, "data/drafts")
CHECKS <- file.path(ROOT, "data/sites")
PRESETS <- c(site   = "Site · the yard or pier and its streets, about 3 km across",
             harbor = "Harbor · the harbor and approaches, about 12 km across",
             region = "Region · the coast and nearby towns, about 45 km across")
osm_link <- function(x) if (nzchar(x %||% "")) sprintf('<a href="https://www.openstreetmap.org/%s">%s</a>', esc(x), esc(x)) else ""

# Under a site card: the preset and what the tools found (scripts/check_site.R).
site_line <- function(id, o) {
  if (!identical(o$kind, "site")) return("")
  f <- file.path(CHECKS, paste0(id, "-checks.json"))
  ck <- if (file.exists(f)) jsonlite::fromJSON(f) else NULL
  checks <- if (is.null(ck)) "not checked yet" else paste0(
    if (isTRUE(ck$pin$pass)) "" else "<strong>Check failed:</strong> ",
    if (!is.na(ck$pin$target %||% NA)) sprintf("pin inside %s", osm_link(ck$pin$target)) else esc(ck$pin$note),
    " · ", if (!isTRUE(ck$water$pass)) "<strong>" else "", esc(ck$water$note), if (!isTRUE(ck$water$pass)) "</strong>" else "")
  sprintf('<p class="site-meta">%s preset · %s</p>', esc(sub(" ·.*", "", PRESETS[[o$preset %||% "harbor"]])), checks)
}

draft_dir <- function(tok) if (grepl("^[a-f0-9]{10}$", tok %||% "")) file.path(DRAFTS, tok) else NULL
read_json_file <- function(f) if (file.exists(f)) jsonlite::fromJSON(f, simplifyVector = TRUE) else NULL
spawn <- function(tok, step, ...) {
  d <- draft_dir(tok)
  writeLines(jsonlite::toJSON(list(state = "running", step = "Starting", pid = NA,
                                   updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), auto_unbox = TRUE),
             file.path(d, "status.json"))
  log <- file.path(d, paste0("log-", step, ".txt"))
  system2("Rscript", c(file.path(ROOT, "scripts/new_site.R"), step, tok, ...), wait = FALSE, stdout = log, stderr = log)
}
redirect <- function(res, to) { res$status <- 303L; res$setHeader("Location", to); res$body <- ""; res }

site_form <- function(title = "", where = "", preset = "harbor", err = "") {
  radios <- paste(vapply(names(PRESETS), function(k) sprintf(
    '<label class="opt"><input type="radio" name="preset" value="%s"%s> <b>%s</b> %s</label>',
    k, if (k == preset) " checked" else "", esc(sub(" ·.*", "", PRESETS[[k]])), esc(sub("^[^·]*· ", "", PRESETS[[k]]))), ""), collapse = "")
  paste0(if (nzchar(err)) sprintf('<p class="err">%s</p>', esc(err)) else "",
'<section><form method="post" action="/sites" class="site" onsubmit="this.querySelector(\'button\').disabled=true;this.querySelector(\'button\').textContent=\'Looking it up…\'">
<label>Title <span class="hint">as it should read on the figure, e.g. “Pier 66 · Bell Street Cruise Terminal”</span>
<input name="title" required maxlength="60" value="', esc(title), '"></label>
<label>Address or coordinates <span class="hint">a US street address, or latitude, longitude (right-click the building in Google Maps to copy them)</span>
<input name="where" required maxlength="200" value="', esc(where), '"></label>
<fieldset><legend>Scale</legend>', radios, '</fieldset>
<button>Find it</button></form></section>')
}

#* @get /sites/new
#* @serializer html
function(title = "", where = "", preset = "harbor") {
  shell("New work site", 'Enter the site; next you confirm the building before the map is made. <a href="/">Back to maps</a>',
        site_form(title, where, if (preset %in% names(PRESETS)) preset else "harbor"))
}

#* @post /sites
#* @serializer html
function(title = "", where = "", preset = "", req, res) {
  title <- trimws(title); where <- trimws(where)
  if (!nzchar(title) || !nzchar(where) || !preset %in% names(PRESETS))
    return(shell("New work site", '<a href="/">Back to maps</a>',
                 site_form(title, where, if (preset %in% names(PRESETS)) preset else "harbor", "Fill in the title, the place and the scale.")))
  tok <- paste(sample(c(0:9, letters[1:6]), 10, TRUE), collapse = "")
  dir.create(file.path(DRAFTS, tok), recursive = TRUE)
  writeLines(jsonlite::toJSON(list(title = substr(title, 1, 60), where = substr(where, 1, 200), preset = preset,
                                   who = who(req)), auto_unbox = TRUE), file.path(DRAFTS, tok, "input.json"))
  spawn(tok, "locate")
  redirect(res, paste0("/sites/draft/", tok))
}

#* @get /sites/draft/<tok>/preview.png
#* @serializer contentType list(type = "image/png")
function(tok, res) {
  f <- file.path(draft_dir(tok) %||% "/nonexistent", "preview.png")
  if (!file.exists(f)) { res$status <- 404L; return(raw()) }
  readBin(f, "raw", file.size(f))
}

#* @post /sites/draft/<tok>/pin
function(tok, lon = "", lat = "", res) {
  lon <- suppressWarnings(as.numeric(lon)); lat <- suppressWarnings(as.numeric(lat))
  if (is.null(draft_dir(tok)) || !dir.exists(draft_dir(tok)) || is.na(lon) || is.na(lat)) { res$status <- 400L; return("Bad request") }
  spawn(tok, "repin", sprintf("%.7f", lon), sprintf("%.7f", lat))
  redirect(res, paste0("/sites/draft/", tok))
}

#* @post /sites/draft/<tok>/build
function(tok, pick = "", res) {
  if (is.null(draft_dir(tok)) || !dir.exists(draft_dir(tok)) || !grepl("^([0-9]+|point)$", pick)) { res$status <- 400L; return("Bad request") }
  spawn(tok, "build", pick)
  redirect(res, paste0("/sites/draft/", tok))
}

#* One draft: progress while a step runs, then the pin to confirm, then a link to the map.
#* @get /sites/draft/<tok>
#* @serializer html
function(tok, res) {
  d <- draft_dir(tok)
  if (is.null(d) || !dir.exists(d)) { res$status <- 404L; return("No such draft") }
  inp <- read_json_file(file.path(d, "input.json")); st <- read_json_file(file.path(d, "status.json"))
  head_ <- function(x) x
  back <- sprintf('<a href="/sites/new?title=%s&amp;where=%s&amp;preset=%s">Start over</a>',
                  utils::URLencode(inp$title, TRUE), utils::URLencode(inp$where, TRUE), inp$preset)
  # a step that died without saying so
  if (st$state %in% c("running", "building") && !is.na(st$pid %||% NA) && !isTRUE(tools::pskill(st$pid, 0L))) {
    st$state <- "error"; st$message <- "The step stopped unexpectedly."
  }
  if (st$state %in% c("running", "building")) {
    body <- sprintf('<section><p class="working">%s…</p><p class="hint">This page refreshes by itself. Fetching OpenStreetMap can take a minute or two when it is busy.</p></section>',
                    esc(if (nzchar(st$step %||% "")) st$step else "Working"))
    return(sub("<head>", '<head><meta http-equiv="refresh" content="3">',
               shell(inp$title, head_(esc(PRESETS[[inp$preset]])), body), fixed = TRUE))
  }
  if (st$state == "error") {
    logs <- list.files(d, "^log-.*\\.txt$", full.names = TRUE)
    tail_ <- if (length(logs)) paste(tail(readLines(logs[which.max(file.mtime(logs))], warn = FALSE), 12), collapse = "\n") else ""
    return(shell(inp$title, paste('Something went wrong.', back, '· <a href="/">Maps</a>'),
                 sprintf('<p class="err">%s</p><details><summary>Log</summary><pre class="err">%s</pre></details>',
                         esc(st$message), esc(tail_))))
  }
  if (st$state == "done")
    return(redirect(res, sprintf("/?style=drawing&size=report#%s", st$id)))

  # ready: confirm the pin
  dr <- read_json_file(file.path(d, "draft.json")); cands <- dr$candidates
  bb <- dr$preview_bbox
  where <- if (isTRUE(dr$entered$coords))
    sprintf("Coordinates %.6f, %.6f%s.", dr$entered$lat, dr$entered$lon,
            if (length(dr$county)) paste0(" in ", esc(dr$county$name)) else " (offshore: no county)")
  else sprintf('The Census geocoder matched <b>%s</b>; its street point is the ring. The geocoder places addresses along the street, not on the building.',
               esc(dr$geocode$matched))
  rows <- if (length(cands) && NROW(cands)) paste(vapply(seq_len(nrow(cands)), function(i) sprintf(
    '<label class="opt"><input type="radio" name="pick" value="%d"%s> <b>%d</b> %s <span class="hint">%s · %s%s</span></label>',
    cands$rank[i], if (i == dr$selected) " checked" else "", cands$rank[i],
    esc(if (nzchar(cands$name[i])) cands$name[i] else cands$kind[i]),
    esc(if (nzchar(cands$name[i])) cands$kind[i] else ""), esc(cands$reason[i]),
    if (nzchar(cands$osm[i])) paste0(" · ", osm_link(cands$osm[i])) else ""), ""), collapse = "") else ""
  point_opt <- sprintf('<label class="opt"><input type="radio" name="pick" value="point"%s> Keep the %s, no building</label>',
                       if (!NROW(cands) || identical(as.integer(dr$selected), 0L)) " checked" else "",
                       if (isTRUE(dr$entered$coords)) "coordinates as entered" else "geocoder’s street point")
  body <- sprintf(
'<section>
<p>%s %s</p>
<form method="post" action="/sites/draft/%s/pin" id="pinform"><input type="hidden" name="lon"><input type="hidden" name="lat"></form>
<img src="/sites/draft/%s/preview.png?v=%s" id="preview" alt="Buildings around the site with the candidate pins" style="cursor:crosshair"
  data-bbox="%.7f,%.7f,%.7f,%.7f">
<form method="post" action="/sites/draft/%s/build" class="site" onsubmit="this.querySelector(\'button\').disabled=true;this.querySelector(\'button\').textContent=\'Making the map…\'">
<fieldset><legend>Pin the map on</legend>%s%s</fieldset>
<p class="hint">%s scale. Next: the OSM detail is fetched and cached, the pin and water are checked with tools, and the B · Drawing report map is rendered (a minute or two).</p>
<button>Make the map</button> %s</form>
</section>
<script>
document.getElementById("preview").addEventListener("click", function (e) {
  var r = this.getBoundingClientRect(), b = this.dataset.bbox.split(",").map(Number);
  var fx = (e.clientX - r.left) / r.width, fy = (e.clientY - r.top) / r.height;
  var f = document.getElementById("pinform");
  f.lon.value = (b[0] + fx * (b[2] - b[0])).toFixed(7);
  f.lat.value = (b[3] - fy * (b[3] - b[1])).toFixed(7);
  this.style.opacity = 0.5; f.submit();
});
</script>',
    where, if (NROW(cands) && any(cands$footprint))
      "Numbered outlines are the buildings it could be; the filled one is the pick. <b>Not right? Click the building on the picture</b> to move the pin there."
    else "No building footprint here: the pin is the filled dot. Numbered rings are nearby structures. <b>Click the picture</b> to put the pin somewhere else.",
    tok, tok, gsub("[^0-9]", "", st$updated %||% ""), bb$xmin, bb$ymin, bb$xmax, bb$ymax, tok,
    rows, point_opt, esc(sub(" ·.*", "", PRESETS[[inp$preset]])), back)
  pin0 <- if (NROW(cands) && dr$selected > 0) c(cands$lon[dr$selected], cands$lat[dr$selected]) else c(dr$entered$lon, dr$entered$lat)
  near <- nearby(pin0[1], pin0[2])
  if (length(near)) body <- paste0(sprintf('<p class="near">Already in the <a href="/locations">locations</a>: %s. Reuse it rather than adding it twice?</p>',
    paste(vapply(near, function(x) sprintf('<a href="/#%s"><b>%s</b></a> (%s m away)', x$id, esc(x$name), format(x$dist_m, big.mark = ",")), ""), collapse = "; ")), body)
  shell(inp$title, head_(sprintf("You entered “%s”. %s", esc(inp$where), if (nzchar(st$message %||% "")) esc(st$message) else "")), body)
}

# ---- locations inventory: every place we have mapped, to view and reuse ---------------
dist_m <- function(lon1, lat1, lon2, lat2) {          # haversine, metres
  r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008 * asin(pmin(1, sqrt(a)))
}
blank <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
locations <- function() lapply(office_ids(), function(id) {
  o <- office_cfg(id)
  f <- file.path(CHECKS, paste0(id, "-checks.json")); ck <- if (file.exists(f)) jsonlite::fromJSON(f) else NULL
  renders <- length(list.files(MAPS, paste0("^", id, "-.*\\.png$")))
  list(id = id, kind = o$kind %||% "office", name = o$name, address = o$subtitle, lat = o$lat, lon = o$lon,
       preset = o$preset %||% NA, target_osm = o$target_osm %||% NA, added = o$added %||% NA,
       added_by = o$added_by %||% NA, renders = renders,
       checks = if (is.null(ck)) NULL else list(pass = isTRUE(ck$pass), checked = ck$checked,
                                                pin = ck$pin$note, water = ck$water$note))
})
nearby <- function(lon, lat, within = 1000) {
  L <- locations(); if (!length(L)) return(list())
  d <- vapply(L, function(x) dist_m(lon, lat, x$lon, x$lat), 0)
  lapply(order(d)[d[order(d)] <= within], function(i) c(L[[i]], list(dist_m = round(d[i]))))
}

#* @get /api/locations
#* @serializer unboxedJSON
function() locations()

#* The inventory: offices and work sites, with what was checked and how to reuse them.
#* @get /locations
#* @serializer html
function() {
  L <- locations()
  rows <- vapply(L, function(x) {
    reuse <- sprintf('/sites/new?title=%s&amp;where=%s&amp;preset=%s', utils::URLencode(sub("^Shipwright · ", "", x$name), TRUE),
                     utils::URLencode(sprintf("%.6f, %.6f", x$lat, x$lon), TRUE), if (is.na(x$preset)) "harbor" else x$preset)
    ck <- if (is.null(x$checks)) '<span class="meta">not checked</span>' else
      sprintf('%s<div class="meta">%s<br>%s</div>', if (x$checks$pass) "Pass" else "<strong>Fail</strong>", esc(x$checks$pin), esc(x$checks$water))
    sprintf('<tr data-q="%s"><td><a href="/#%s"><b>%s</b></a><div class="meta">%s</div></td><td><button type="button" class="copy" data-c="%.6f, %.6f" title="Copy">%.5f, %.5f</button></td><td>%s</td><td>%s</td><td>%s</td><td class="meta">%s</td><td><a href="/#%s">Maps (%d)</a><br><a href="%s">New map from here</a></td></tr>',
            esc(tolower(paste(x$name, x$address, x$added_by))), x$id, esc(x$name), esc(x$address), x$lat, x$lon, x$lat, x$lon,
            if (x$kind == "office") "Office" else esc(tools::toTitleCase(x$preset %||% "")),
            if (is.na(x$target_osm) || !nzchar(x$target_osm)) '<span class="meta">point</span>' else osm_link(x$target_osm),
            ck, paste(esc(blank(x$added)), esc(blank(x$added_by)), sep = "<br>"),
            x$id, x$renders, reuse)
  }, "")
  shell("Locations",
    sprintf('%d places we have mapped: offices and work sites. Reuse one for a new figure, or copy its coordinates. <a href="/">Maps</a> · <a href="/sites/new">New work site</a> · <a href="/api/locations">JSON</a>', length(L)),
    paste0('<input type="search" id="q" placeholder="Filter by name, address or who added it" class="filter">',
           '<table class="notes locations"><thead><tr><th>Location</th><th>Coordinates</th><th>Scale</th><th>Pinned on</th><th>Checks</th><th>Added</th><th></th></tr></thead><tbody>',
           paste(rows, collapse = ""), '</tbody></table>',
           "<script>document.getElementById('q').oninput=function(){var v=this.value.toLowerCase();",
           "document.querySelectorAll('tr[data-q]').forEach(function(r){r.hidden=v&&r.dataset.q.indexOf(v)<0})};",
           "document.querySelectorAll('button.copy').forEach(function(b){b.onclick=function(){navigator.clipboard.writeText(b.dataset.c);",
           "var t=b.textContent;b.textContent='Copied';setTimeout(function(){b.textContent=t},1200)}})</script>"))
}

#* @get /healthz
#* @serializer text
function() "ok"

# Icons and the notes layer live in app/static/.
static_file <- function(name) readBin(file.path(ROOT, "app/static", name), "raw",
                                      file.size(file.path(ROOT, "app/static", name)))

#* @get /favicon.ico
#* @serializer contentType list(type = "image/x-icon")
function() static_file("favicon.ico")

#* @get /apple-touch-icon.png
#* @serializer contentType list(type = "image/png")
function() static_file("apple-touch-icon.png")
