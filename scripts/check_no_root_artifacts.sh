#!/usr/bin/env bash
set -euo pipefail

# Guard against stray artifacts in the repository
# Fails if PNG/PDF/TXT files are found outside allowed directories

root_dir="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$root_dir"

echo "Running repository artifact guard..."

# Allowed directories (prefix anchored)
allowed_prefixes=(
  "./build/"
  "./doc/"
  "./example/"
  "./output/"
  "./test/output/"
  "./media/"
)

mapfile -t matches < <(find . -type f \( -name "*.png" -o -name "*.pdf" -o -name "*.txt" \))

offenders=()
for f in "${matches[@]}"; do
  allowed=false
  for p in "${allowed_prefixes[@]}"; do
    if [[ "$f" == $p* ]]; then
      allowed=true
      break
    fi
  done
  # Allow files explicitly referenced in .gitignore exceptions
  if [[ "$allowed" == false ]]; then
    # Permit README, configs, etc., not matching patterns
    offenders+=("$f")
  fi
done

if (( ${#offenders[@]} > 0 )); then
  echo "ERROR: Found generated artifacts outside allowed directories:" >&2
  printf '  %s\n' "${offenders[@]}" >&2
  echo "Hint: Use test/output/ or build/test/output/ for tests; output/example/... for examples." >&2
  exit 1
fi

echo "Artifact guard passed: no stray files detected."

