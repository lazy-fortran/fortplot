#!/usr/bin/env bash
set -euo pipefail

# Check for forbidden artifacts in repository root (Issue #990)
# Allowed locations: build/, doc/, output/, example/, test/, .git/
# Also verify required files exist (CMakeLists.txt for downstream CMake integration)

shopt -s nullglob

# Check for REQUIRED CMakeLists.txt - MANDATORY for downstream CMake integration
if [[ ! -f "./CMakeLists.txt" ]]; then
  echo "ERROR: CMakeLists.txt is MISSING from repository root!"
  echo "This file is MANDATORY for downstream CMake integration (FetchContent, etc.)"
  echo "Both CMake and FPM build systems must be maintained in parallel."
  echo "Please restore CMakeLists.txt immediately."
  exit 1
fi

violations=()

while IFS= read -r -d '' file; do
  # Skip allowed paths
  case "$file" in
    ./build/*|./doc/*|./output/*|./example/*|./test/*|./.git/*)
      continue
      ;;
    ./CMakeLists.txt)
      # CMakeLists.txt is explicitly allowed and required
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
  printf '\nPlease move test outputs to build/test/output/ and ensure .gitignore covers these patterns.\n'
  exit 1
fi

echo "Root artifact hygiene check passed."
echo "CMakeLists.txt present for downstream CMake integration: PASS"
