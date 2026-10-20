#!/usr/bin/env bash
# Golden renders for the offices, and a pixel diff against them. Run on the VM.
#   scripts/golden.sh render <set>            # PNG of every office x style x size -> outputs/golden/<set>/
#   scripts/golden.sh diff <before> <after>   # ImageMagick AE (pixels that differ) per file; exit 1 if any differ
set -euo pipefail
export SW_MAPS_ROOT="${SW_MAPS_ROOT:-$HOME/sw-maps}"; ROOT="$SW_MAPS_ROOT"
export SW_MAPS_DATE="${SW_MAPS_DATE:-2026-10-20}"   # the title block shows the date; pin it so sets compare across days
cd "$ROOT"
OFFICES="${GOLDEN_OFFICES:-hawaii florida}"
STYLES="drawing platform mainsite"
SIZES="report half slide"

case "${1:-}" in
render)
  set_="${2:?set name}"; mkdir -p "outputs/golden/$set_"
  jobs=()
  for o in $OFFICES; do for st in $STYLES; do for sz in $SIZES; do
    jobs+=("$o $st $sz")
  done; done; done
  printf '%s\n' "${jobs[@]}" | xargs -P 2 -L 1 bash -c \
    'COMP_PNG_ONLY=1 Rscript scripts/comp_map.R "$0" "$1" "$2" "outputs/golden/'"$set_"'" >/dev/null 2>&1 \
       && echo "ok   $0 $1 $2" || echo "FAIL $0 $1 $2"'
  ;;
diff)
  a="outputs/golden/${2:?before}"; b="outputs/golden/${3:?after}"; bad=0
  for f in "$a"/*.png; do
    n=$(basename "$f")
    if [[ ! -f "$b/$n" ]]; then echo "MISSING $n"; bad=1; continue; fi
    ae=$(compare -metric AE "$f" "$b/$n" /dev/null 2>&1 || true)
    [[ "$ae" == "0" ]] && echo "same    $n" || { echo "DIFF    $n  ($ae px)"; bad=1; }
  done
  exit $bad
  ;;
*) sed -n '2,5p' "$0"; exit 2 ;;
esac
