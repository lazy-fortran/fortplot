#!/usr/bin/env bash
set -euo pipefail

# Heuristic Fortran procedure complexity checker.
# Counts procedures per file and total using ripgrep.
# Fails if budgets are exceeded.

ROOT_DIR=${1:-"src"}

# Budgets (can be overridden via environment)
MAX_TOTAL_PROCS=${MAX_TOTAL_PROCS:-2000}
MAX_PROCS_PER_FILE=${MAX_PROCS_PER_FILE:-60}

if ! command -v rg >/dev/null 2>&1; then
  echo "ERROR: ripgrep (rg) is required for complexity check" >&2
  exit 2
fi

shopt -s nullglob

pattern='^[ \t]*(pure|elemental|recursive|module[ \t]+)?[ \t]*(subroutine|function)[ \t]+'

mapfile -t lines < <(rg -n -i --glob "${ROOT_DIR}/**" -g '!**/thirdparty/**' "$pattern" || true)

declare -A per_file
total=0
for entry in "${lines[@]}"; do
  file=${entry%%:*}
  per_file["$file"]=$(( ${per_file["$file"]:-0} + 1 ))
  total=$(( total + 1 ))
done

status=0
printf 'Fortran procedure complexity report (heuristic)\n'
printf 'Root: %s\n' "$ROOT_DIR"
printf 'Total procedures: %d (budget: %d)\n' "$total" "$MAX_TOTAL_PROCS"

if (( total > MAX_TOTAL_PROCS )); then
  echo "VIOLATION: total procedures $total exceeds budget $MAX_TOTAL_PROCS" >&2
  status=1
fi

printf '\nTop files by procedure count (desc):\n'
for k in "${!per_file[@]}"; do
  printf '%5d %s\n' "${per_file[$k]}" "$k"
done | sort -nr | head -50

violations=0
while IFS= read -r line; do
  count=${line%% *}
  file=${line#* }
  if (( count > MAX_PROCS_PER_FILE )); then
    echo "VIOLATION: $file has $count procedures (budget $MAX_PROCS_PER_FILE)" >&2
    violations=$(( violations + 1 ))
  fi
done < <(for k in "${!per_file[@]}"; do printf '%d %s\n' "${per_file[$k]}" "$k"; done | sort -nr)

if (( violations > 0 )); then
  status=1
fi

exit $status

