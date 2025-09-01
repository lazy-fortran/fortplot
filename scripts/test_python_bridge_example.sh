#!/usr/bin/env bash
set -euo pipefail

# Basic non-interactive validation of fortplot_python_bridge using an example command file.
# Generates a small PNG and verifies it exists and is non-empty.

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CMD_FILE="$ROOT_DIR/example/python_bridge/commands_basic.txt"
OUT_FILE="$ROOT_DIR/output/python_bridge_demo.png"

# Ensure output dir exists
mkdir -p "$ROOT_DIR/output"

# Locate bridge executable via the Python helper (avoids hardcoding build paths)
# Use PYTHONPATH to include the repo's python/ directory (robust when running from stdin)
BRIDGE_EXE=$(PYTHONPATH="$ROOT_DIR/python${PYTHONPATH+:$PYTHONPATH}" python3 - <<'PY'
try:
    from fortplot.fortplot_wrapper import FortplotModule  # type: ignore
    print(FortplotModule()._find_bridge_executable())
except Exception:
    # Print nothing; caller will fall back
    pass
PY
)

# Fallback to a common build path if Python detection failed
if [ -z "${BRIDGE_EXE:-}" ]; then
  if [ -x "$ROOT_DIR/build/app/fortplot_python_bridge" ]; then
    BRIDGE_EXE="$ROOT_DIR/build/app/fortplot_python_bridge"
  fi
fi

if [ -z "${BRIDGE_EXE:-}" ] || { [ ! -x "$BRIDGE_EXE" ] && ! command -v "$BRIDGE_EXE" >/dev/null 2>&1; }; then
  echo "Bridge executable not found or not executable: ${BRIDGE_EXE:-<empty>}" >&2
  exit 1
fi

# Run bridge with example commands
timeout 10s bash -c "cat '$CMD_FILE' | '$BRIDGE_EXE'"

# Validate output
if [ ! -f "$OUT_FILE" ]; then
  echo "Expected output file not created: $OUT_FILE" >&2
  exit 2
fi

size=$(stat -c%s "$OUT_FILE" 2>/dev/null || wc -c < "$OUT_FILE")
if [ "${size:-0}" -lt 1000 ]; then
  echo "Output file too small (${size} bytes): $OUT_FILE" >&2
  exit 3
fi

echo "✓ Python bridge example produced $OUT_FILE (${size} bytes)"
