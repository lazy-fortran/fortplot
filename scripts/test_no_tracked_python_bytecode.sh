#!/usr/bin/env bash
set -euo pipefail

# Guard against Python bytecode being tracked in git
# Fails if any '*.pyc' files or '__pycache__' directories are tracked.

tracked=$(git ls-files -z | tr '\0' '\n' | rg -n '(^|/)__pycache__\/|\.pyc$' || true)

if [[ -n "$tracked" ]]; then
  echo "ERROR: Tracked Python bytecode detected:" >&2
  echo "$tracked" >&2
  echo "Please remove these files from git history and working tree." >&2
  exit 1
fi

echo "OK: No tracked Python bytecode (pyc/__pycache__)"

