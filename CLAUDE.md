# CLAUDE.md — sw-maps

Shipwright location maps: R renderer + plumber web app on the `sw-maps` exe.dev VM.
Read README.md (architecture, deploy) and ROADMAP.md (what's next) first.

## Where things run

- App: https://sw-maps.exe.xyz → VM `sw-maps.exe.xyz` (exe.dev account **exe-shipwright**;
  `ssh exe-shipwright <cmd>` for the exe.dev CLI). Repo checkout `/home/exedev/sw-maps`,
  unit `sw-maps.service`, port 8000. R 4.6 + packages from r2u apt binaries.
- **Render on the VM.** It has the fonts on fontconfig's path and all R packages. The Mac
  may not have R.
- GitHub: `yourmaritimepartner/sw-maps`. `~/sw-projects/*` pushes as `yourmaritimepartner`
  (global `includeIf` in `~/.gitconfig`); if a push 403s as `djinna`, check
  `git config credential.https://github.com/.username`.

## Workflow

- Edit locally in `~/sw-projects/sw-maps`, `rsync` to the VM to test renders, then commit,
  push, and on the VM `git fetch && git reset --hard origin/main && sudo systemctl restart sw-maps`.
- Before restarting the service after editing `app/plumber.R`, check it parses:
  `Rscript -e 'invisible(parse("app/plumber.R"))'` (a parse error takes the site down).
- Review notes: `ssh sw-maps.exe.xyz cat ~/sw-maps/data/notes.json`; mark one done with
  `curl -H 'Content-Type: application/json' -d '{"status":"done","resolution":"<sha>"}' localhost:8000/api/notes/<id>`
  on the VM. "Work the notes" = fix open notes in a batch, re-render, mark done.
- Look at every render you change (download the PNG and view it); don't trust "Wrote …".

## Rules from the owner

- **Logo is one unit**: ship mark + "Shipwright" wordmark + tagline, never cropped or split.
  Official files: `app/static/logo/` (horizontal); full kit in the owner's Dropbox.
- **Hanken Grotesk throughout**, maps and app. It lacks hair/thin spaces, prime and ʻokina.
- Design language = the BD apps' (platform/bd `srv/static/style.css` on sw-dashboard), not
  the retired v1 on sw-designlanguage. Accent navy `#015686`; red is for the logo only.
- Default style is B · Drawing.
- Confirm before outward-facing actions on other VMs (sw-dashboard is shared production).

## Gotchas

- When writing R through the Write tool, `\uXXXX` escapes become literal characters; patch
  scripts must match the literal glyphs.
- Single-quoted R strings that hold HTML need `\'` for inner quotes.
- `inset_view` / `main_bbox` in configs are lon/lat; the renderer grows `main_bbox` to the
  figure's aspect.
