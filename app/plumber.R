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
  <header><h2>%s</h2><p>%s</p></header>
  %s
  <div class="row">
    <form method="post" action="/render/%s?style=%s&amp;size=%s" onsubmit="this.querySelector(\'button\').disabled=true;this.querySelector(\'button\').textContent=\'Rendering… about 30 s\'">
      <button>%s</button></form>
    <span class="when">%s</span>
    <span class="dl">%s</span>
  </div>%s
</section>',
    id, esc(o$name), esc(o$subtitle),
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
  shell("Shipwright Maps",
    'Office location maps for project reports. Pick a style and size, then download PNG, PDF or SVG. Press N to pin a note. <a href="/comps">Design comps</a> · <a href="/notes">Notes</a>',
    paste0('<div class="switches">', switcher("Style", "style", STYLES, style, size),
           switcher("Size", "size", SIZES, size, style), "</div>",
           paste(vapply(office_ids(), card, "", style = style, size = size), collapse = "\n")))
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
