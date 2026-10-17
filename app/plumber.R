# sw-maps web front: list offices, re-render on demand, serve the outputs.
# Started by app/serve.R; rendering shells out to scripts/location_map.R so each
# render gets a clean R session (the modules define globals).

ROOT    <- Sys.getenv("SW_MAPS_ROOT", path.expand("~/sw-projects/sw-maps"))
CONFIGS <- file.path(ROOT, "configs")
OUT     <- file.path(ROOT, "outputs/location-maps")

FORMATS <- c(PNG = "-location-map.png", SVG = "-location-map.svg",
             PDF = "-location-map.pdf", "Layered SVG" = "-location-map-layered.svg")

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

card <- function(id) {
  o   <- office_cfg(id)
  png <- file.path(OUT, paste0(o$slug, FORMATS[["PNG"]]))
  have <- file.exists(png)
  v   <- if (have) as.integer(file.mtime(png)) else 0L
  when <- if (have) format(file.mtime(png), "%Y-%m-%d %H:%M UTC", tz = "UTC") else "never"
  links <- vapply(names(FORMATS), function(f) {
    fn <- paste0(o$slug, FORMATS[[f]])
    if (file.exists(file.path(OUT, fn)))
      sprintf("<a href=\"/files/%s?v=%d\" download>%s</a>", fn, v, f) else ""
  }, "")
  err <- if (!is.null(last_error[[id]]))
    sprintf("<pre class=\"err\">%s</pre>", esc(last_error[[id]])) else ""
  sprintf(
'<section id="%s">
  <header><h2>%s</h2><p>%s</p></header>
  %s
  <div class="row">
    <form method="post" action="/render/%s" onsubmit="this.querySelector(\'button\').disabled=true;this.querySelector(\'button\').textContent=\'Rendering…\'">
      <button>Re-render</button></form>
    <span class="when">Last rendered %s</span>
    <span class="dl">%s</span>
  </div>%s
</section>',
    id, esc(o$name), esc(o$subtitle),
    if (have) sprintf("<a href=\"/files/%s?v=%d\"><img src=\"/files/%s?v=%d\" alt=\"%s location map\"></a>",
                      basename(png), v, basename(png), v, esc(o$name)) else "",
    id, when, paste(links, collapse = ""), err)
}

PAGE <- '<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Shipwright Maps</title>
<link rel="icon" href="/favicon.ico" sizes="48x48">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
<style>
:root{--ink:#101820;--gold:#F2C14E;--paper:#F7F5F0;--line:#D9D4C8;--muted:#5A6570}
*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);
font:16px/1.45 system-ui,-apple-system,"Segoe UI",Arial,sans-serif}
main{max-width:1040px;margin:0 auto;padding:32px 16px 64px}
h1{font-size:28px;margin:0 0 4px}h1+p{margin:0 0 32px;color:var(--muted)}
section{background:#fff;border:1px solid var(--line);border-radius:6px;padding:20px;margin:0 0 28px}
h2{margin:0;font-size:20px}header p{margin:2px 0 14px;color:var(--muted)}
img{display:block;width:100%%;height:auto;border:1px solid var(--line)}
.row{display:flex;flex-wrap:wrap;gap:12px 20px;align-items:center;margin-top:14px}
button{font:inherit;font-weight:600;background:var(--ink);color:#fff;border:0;border-radius:4px;padding:8px 16px;cursor:pointer}
button:disabled{background:var(--muted);cursor:wait}
.when{color:var(--muted);font-size:14px}.dl{display:flex;gap:14px;margin-left:auto}
.dl a{color:var(--ink);font-weight:600;text-decoration:none;border-bottom:2px solid var(--gold)}
.err{white-space:pre-wrap;background:#FFF4F2;border:1px solid #E8B4AA;padding:12px;font-size:13px;margin:14px 0 0}
</style></head><body><main>
<h1>Shipwright Maps</h1>
<p>Location maps for project reports. Download any format, or re-render after a config change.</p>
%s
</main></body></html>'

#* @get /
#* @serializer html
function() sprintf(PAGE, paste(vapply(office_ids(), card, ""), collapse = "\n"))

#* @post /render/<id>
function(id, res) {
  if (!id %in% office_ids()) { res$status <- 404L; res$body <- "Unknown office"; return(res) }
  log <- suppressWarnings(system2("Rscript", c(file.path(ROOT, "scripts/location_map.R"), id),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status")
  if (!is.null(status) && status != 0) last_error[[id]] <- paste(tail(log, 25), collapse = "\n")
  else if (exists(id, envir = last_error, inherits = FALSE)) rm(list = id, envir = last_error)
  res$status <- 303L
  res$setHeader("Location", paste0("/#", id))
  res$body <- ""
  res
}

#* @get /healthz
#* @serializer text
function() "ok"

# Shipwright mark from shipwright.biz, in app/static/.
static_file <- function(name) readBin(file.path(ROOT, "app/static", name), "raw",
                                      file.size(file.path(ROOT, "app/static", name)))

#* @get /favicon.ico
#* @serializer contentType list(type = "image/x-icon")
function() static_file("favicon.ico")

#* @get /apple-touch-icon.png
#* @serializer contentType list(type = "image/png")
function() static_file("apple-touch-icon.png")
