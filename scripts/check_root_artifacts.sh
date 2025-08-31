#!/usr/bin/env bash
set -euo pipefail

# Check for forbidden artifacts in repository root (Issue #990)
# Allowed locations: build/, doc/, output/, example/, test/, .git/

shopt -s nullglob

violations=()

while IFS= read -r -d '' file; do
  # Skip allowed paths
  case "$file" in
    ./build/*|./doc/*|./output/*|./example/*|./test/*|./.git/*)
      continue
      ;;
  esac
  violations+=("$file")
done < <(find . -type f \( \
  -name "*.png" -o -name "*.pdf" -o -name "*.txt" -o -name "*.log" -o -name "*.ppm" \
  -o -name "*.mp4" -o -name "*.avi" -o -name "*.mkv" \) -print0)

# Also explicitly check for common stray executables in root
for bin in ./test_basic_user_workflow ./test_readme_promises; do
  if [[ -f "$bin" ]]; then
    violations+=("$bin")
  fi
done

if (( ${#violations[@]} > 0 )); then
  echo "ERROR: Found forbidden artifacts in repository root (Issue #990):"
  printf '  %s\n' "${violations[@]}" | sort -u
  echo "\nPlease move test outputs to test/output/ or build/ and ensure .gitignore covers these patterns."
  exit 1
fi

echo "Root artifact hygiene check passed."
