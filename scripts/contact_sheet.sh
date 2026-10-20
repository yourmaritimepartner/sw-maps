#!/usr/bin/env bash
# Render every style x size for one location and tile the PNGs on one sheet, to look at
# all nine at once. Run on the VM.
#   scripts/contact_sheet.sh <id> [outputs/maps]   -> /tmp/sheet-<id>.png
set -euo pipefail
export SW_MAPS_ROOT="${SW_MAPS_ROOT:-$HOME/sw-maps}"; cd "$SW_MAPS_ROOT"
id="${1:?location id}"; out="${2:-outputs/maps}"
for st in drawing platform mainsite; do for sz in report half slide; do echo "$st $sz"; done; done |
  xargs -P 2 -L 1 bash -c 'Rscript scripts/comp_map.R '"$id"' "$0" "$1" '"$out"' >/tmp/sheet-'"$id"'-$0-$1.log 2>&1 \
    && echo "ok   $0 $1" || { echo "FAIL $0 $1"; tail -3 /tmp/sheet-'"$id"'-$0-$1.log; }'
F="$SW_MAPS_ROOT/fonts/HankenGrotesk-Bold.ttf"
rows=()
for st in drawing platform mainsite; do
  cells=()
  for sz in report half slide; do
    f="$out/$id-$st-$sz.png"
    [[ -f "$f" ]] && cells+=("$f") || cells+=("xc:#eeeeee")
  done
  convert "${cells[0]}" -resize x700 "${cells[1]}" -resize x700 "${cells[2]}" -resize x700 \
    -bordercolor white -border 12 +append -gravity west -background white -splice 220x0 \
    -font "$F" -pointsize 34 -annotate +16+0 "$st" "/tmp/row-$id-$st.png"
  rows+=("/tmp/row-$id-$st.png")
done
convert "${rows[@]}" -background white -append "/tmp/sheet-$id.png"
echo "/tmp/sheet-$id.png"
