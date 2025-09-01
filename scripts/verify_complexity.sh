#!/usr/bin/env sh
set -eu

# Heuristic Fortran procedure complexity checker.
# Counts procedures per file and total using ripgrep.
# Fails if budgets are exceeded.

ROOT_DIR=${1:-src}

# Budgets (can be overridden via environment)
MAX_TOTAL_PROCS=${MAX_TOTAL_PROCS:-2000}
MAX_PROCS_PER_FILE=${MAX_PROCS_PER_FILE:-60}

if ! command -v rg >/dev/null 2>&1; then
  echo "ERROR: ripgrep (rg) is required for complexity check" >&2
  echo "Install: https://github.com/BurntSushi/ripgrep#installation" >&2
  exit 2
fi

# Pattern: leading whitespace, optional qualifiers, then subroutine|function
pattern='^[[:space:]]*(pure|elemental|recursive|module[[:space:]]+)?[[:space:]]*(subroutine|function)[[:space:]]+'

# Gather matches (may be empty)
matches=$(rg -n -i --glob "${ROOT_DIR}/**" -g '!**/thirdparty/**' "$pattern" || true)

total=0
if [ -n "$matches" ]; then
  total=$(printf "%s\n" "$matches" | wc -l | awk '{print $1}')
fi

status=0
printf 'Fortran procedure complexity report (heuristic)\n'
printf 'Root: %s\n' "$ROOT_DIR"
printf 'Total procedures: %d (budget: %d)\n' "$total" "$MAX_TOTAL_PROCS"

if [ "$total" -gt "$MAX_TOTAL_PROCS" ]; then
  echo "VIOLATION: total procedures $total exceeds budget $MAX_TOTAL_PROCS" >&2
  status=1
fi

printf '\nTop files by procedure count (desc):\n'
if [ -n "$matches" ]; then
  # Count per file and show top 50
  printf "%s\n" "$matches" | cut -d: -f1 | sort | uniq -c | sort -nr | sed 's/^ *//' | head -50

  # Check per-file violations
  violations=0
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    count=${line%% *}
    file=${line#* }
    if [ "$count" -gt "$MAX_PROCS_PER_FILE" ]; then
      echo "VIOLATION: $file has $count procedures (budget $MAX_PROCS_PER_FILE)" >&2
      violations=$((violations + 1))
    fi
  done <<EOF
$(printf "%s\n" "$matches" | cut -d: -f1 | sort | uniq -c | sort -nr | sed 's/^ *//')
EOF

  if [ "$violations" -gt 0 ]; then
    status=1
  fi
fi

exit "$status"
