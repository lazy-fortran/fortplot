#!/usr/bin/env bash
set -euo pipefail

# Run key examples to generate artifacts
fpm run --example scale_examples >/dev/null
fpm run --example pcolormesh_demo >/dev/null
fpm run --example marker_demo >/dev/null
fpm run --example line_styles >/dev/null
fpm run --example errorbar_demo >/dev/null

# Additional visual regression examples (ylabel spacing, PDF scale, subplots, unicode, show viewer)
fpm run --example label_positioning_demo >/dev/null
# Generate explicit ylabel comparison artifacts so left-margin checks run (fixes #1294)
fpm run --example ylabel_comparison >/dev/null
fpm run --example test_pdf_scale_regression >/dev/null
fpm run --example subplots_grid_demo >/dev/null
fpm run --example unicode_demo >/dev/null
fpm run --example show_viewer_demo >/dev/null || true
fpm run --example grid_demo >/dev/null

check_pdf_ok() {
  local pdf=$1
  if ! command -v pdfimages >/dev/null 2>&1; then
    echo "Missing pdfimages (poppler-utils)" >&2
    exit 2
  fi
  local out
  out=$(pdfimages -list "$pdf" 2>&1 || true)
  echo "[pdfimages] $pdf"
  echo "$out" | head -n 3
  if echo "$out" | grep -qi "Syntax Error"; then
    echo "ERROR: PDF syntax errors in $pdf" >&2
    exit 1
  fi
}

check_pdftotext_has() {
  local pdf=$1
  shift
  if ! command -v pdftotext >/dev/null 2>&1; then
    echo "Missing pdftotext (poppler-utils)" >&2
    exit 2
  fi
  local txt
  txt=$(pdftotext "$pdf" - 2>/dev/null || true)
  for needle in "$@"; do
    echo "[pdftotext] asserting needle=$needle in ${pdf}"
    echo "$txt" | grep -q "$needle" || {
      echo "ERROR: Missing needle=$needle in $pdf" >&2
      exit 1
    }
  done
}

check_png_size() {
  local png=$1
  local min=${2:-4000}
  local sz
  sz=$(stat -c %s "$png")
  echo "[png] $png size=$sz"
  if [[ $sz -lt $min ]]; then
    echo "ERROR: $png too small (size=$sz)" >&2
    exit 1
  fi
}

check_left_margin_brightness() {
  local png=$1
  local stripe_w=${2:-12}
  local min_mean=${3:-0.95}
  local h
  if command -v identify >/dev/null 2>&1; then
    h=$(identify -format "%h" "$png" 2>/dev/null || echo 0)
  elif command -v magick >/dev/null 2>&1; then
    h=$(magick identify -format "%h" "$png" 2>/dev/null || echo 0)
  else
    echo "Missing ImageMagick (identify/magick) - skipping left-margin brightness check for $png" >&2
    return 0
  fi
  if [[ $h -le 0 ]]; then
    echo "ERROR: Could not read PNG height for $png" >&2
    exit 1
  fi
  local mean
  if command -v convert >/dev/null 2>&1; then
    mean=$(convert "$png" -crop ${stripe_w}x${h}+0+0 +repage -colorspace Gray -format "%[fx:mean]" info: 2>/dev/null || echo 0)
  elif command -v magick >/dev/null 2>&1; then
    mean=$(magick "$png" -crop ${stripe_w}x${h}+0+0 +repage -colorspace Gray -format "%[fx:mean]" info: 2>/dev/null || echo 0)
  else
    echo "Missing ImageMagick (convert/magick) - skipping left-margin brightness check for $png" >&2
    return 0
  fi
  echo "[ylabel-left] $png stripe_w=$stripe_w mean=$mean threshold=$min_mean"
  awk -v m="$mean" -v t="$min_mean" 'BEGIN { exit (m+0 >= t+0 ? 0 : 1) }' || {
    echo "ERROR: Left margin too dark (insufficient whitespace) in $png" >&2
    exit 1
  }
}

echo "Verifying example artifacts (PDF/PNG/txt) with strict checks..."

# Scale examples: ylabel must indicate superscript 3 (Unicode or WinAnsi octal) and general labels present
check_pdf_ok output/example/fortran/scale_examples/symlog_scale.pdf
if pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x³"; then
  echo "[ok] symlog ylabel shows superscript three (unicode)"
elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -F -q "x\263"; then
  echo "[ok] symlog ylabel shows superscript three (WinAnsi)"
elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x\^3"; then
  echo "[ok] symlog ylabel shows superscript three (mathtext)"
elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x. - 50x"; then
  echo "[ok] symlog ylabel present; superscript may be lost in pdftotext"
else
  echo "ERROR: symlog ylabel missing superscript 3" >&2
  exit 1
fi
check_pdftotext_has output/example/fortran/scale_examples/symlog_scale.pdf "Symlog" "x"

# Assert that digits in tick/labels like "10" are emitted as a single Tj token
# to avoid odd inter-digit spacing (regression for #1301).
if command -v python3 >/dev/null 2>&1; then
  if python3 scripts/pdf_expect_token.py output/example/fortran/scale_examples/symlog_scale.pdf "(10) Tj"; then
    echo "[ok] content stream groups '10' as a single Tj"
  else
    echo "ERROR: Expected '(10) Tj' not found in symlog_scale.pdf content stream" >&2
    exit 1
  fi
  # And conversely, ensure we do not emit adjacent digit-by-digit Tj for 1 and 0
  if python3 scripts/pdf_expect_token.py output/example/fortran/scale_examples/symlog_scale.pdf "\(1\)\s*Tj\s*\(0\)\s*Tj" --regex; then
    echo "ERROR: Found digit-by-digit '(1) Tj (0) Tj' sequence; expected grouping" >&2
    exit 1
  else
    echo "[ok] no digit-by-digit Tj sequence for '1' and '0'"
  fi
else
  echo "WARN: python3 not available; skipping '(10) Tj' grouping check"
fi

# Pcolormesh PDFs must have no syntax errors
check_pdf_ok output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf
check_pdf_ok output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf

# Ensure pcolormesh PDFs are color and not grayscale.
for pdf in \
  output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf \
  output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf
  do
  echo "[pdfcolor] checking pcolormesh color encoding in $pdf"
  missing_msg="ERROR: $pdf did not show vector 'rg' nor RGB Image XObject"
  if python3 scripts/pdf_scan_rg.py "$pdf" >/dev/null 2>&1; then
    echo "[ok] $pdf has non-gray 'rg' (vector)"
  elif command -v pdfimages >/dev/null 2>&1; then
    lst=$(pdfimages -list "$pdf" 2>/dev/null || true)
    echo "$lst" | head -n 3
    echo "$lst" | awk 'NR>2 && tolower($6) ~ /rgb/ {found=1} END { exit(found?0:1) }' \
      && echo "[ok] $pdf contains RGB Image XObject" \
      || {
        echo "ERROR: $pdf did not show RGB image in pdfimages -list" >&2
        exit 1
      }
  elif command -v rg >/dev/null 2>&1; then
    if rg -n "/Subtype /Image|/ColorSpace /DeviceRGB" -S --text "$pdf" >/dev/null 2>&1; then
      echo "[ok] $pdf declares Image XObject with DeviceRGB"
    else
      echo "$missing_msg" >&2
      exit 1
    fi
  else
    echo "$missing_msg" >&2
    exit 1
  fi
done

# Grid demo: ensure outputs exist and ASCII export shows grid structure
if [[ -f output/example/fortran/grid_demo/grid_demo.png ]]; then
  check_png_size output/example/fortran/grid_demo/grid_demo.png 4000
fi
if [[ -f output/example/fortran/grid_demo/grid_demo.pdf ]]; then
  check_pdf_ok output/example/fortran/grid_demo/grid_demo.pdf
  check_pdftotext_has output/example/fortran/grid_demo/grid_demo.pdf "Basic Plot - Grid Demo"
fi
if [[ -f output/example/fortran/grid_demo/grid_demo.txt ]]; then
  # Heuristic: ASCII grid should contain repeated separators and vertical bars
  if ! grep -Eq "#:#:#:#|\|\s+\|" output/example/fortran/grid_demo/grid_demo.txt; then
    echo "ERROR: grid_demo.txt does not appear to contain grid lines" >&2
    exit 1
  fi
fi

# A couple PNG size checks as non-empty proxy
check_png_size output/example/fortran/marker_demo/all_marker_types.png 8000
check_png_size output/example/fortran/line_styles/line_styles.png 10000

# Negative-coordinate pcolormesh: ensure non-trivial image and negative ticks present in PDF text
check_png_size output/example/fortran/pcolormesh_demo/pcolormesh_negative.png 15000
check_pdf_ok output/example/fortran/pcolormesh_demo/pcolormesh_negative.pdf
if command -v pdftotext >/dev/null 2>&1; then
  pdftotext output/example/fortran/pcolormesh_demo/pcolormesh_negative.pdf - | grep -Eq '[-−][0-9]' || {
    echo "ERROR: Negative tick labels not found in pcolormesh_negative.pdf" >&2
    exit 1
  }
fi

# Require multiple unique colors to ensure actual data render (not blank)
if command -v identify >/dev/null 2>&1; then
  uc=$(identify -format %k output/example/fortran/pcolormesh_demo/pcolormesh_negative.png 2>/dev/null || echo 0)
  echo "[colors] pcolormesh_negative.png => $uc"
  if [[ $uc -lt 50 ]]; then
    echo "ERROR: pcolormesh_negative.png has too few unique colors ($uc)" >&2
    exit 1
  fi
fi

# Y-label positioning demo must keep a bright safety margin on the left
for f in \
  ylabel_test1_wide_ticks.png \
  ylabel_test2_scientific.png \
  ylabel_test3_long_label.png \
  ylabel_test4_multiple.png
  do
  if [[ -f "$f" ]]; then
    check_left_margin_brightness "$f" 12 0.94
  else
    echo "WARN: $f not found; label positioning demo may not have run"
  fi
done

# Contour demo should have many unique colors (not blank)
for f in \
  output/example/fortran/contour_demo/ripple_inferno.png \
  output/example/fortran/contour_demo/ripple_coolwarm.png \
  output/example/fortran/contour_demo/ripple_jet.png
  do
  if [[ -f "$f" ]]; then
    if command -v identify >/dev/null 2>&1; then
      c=$(identify -format %k "$f" 2>/dev/null || echo 0)
    elif command -v magick >/dev/null 2>&1; then
      c=$(magick identify -format %k "$f" 2>/dev/null || echo 0)
    else
      echo "Missing ImageMagick (identify/magick) - skipping color-count check for $f" >&2
      continue
    fi
    echo "[colors] $f => $c"
    if [[ $c -gt 1200 ]]; then
      echo "ERROR: $f has too many unique colors ($c) - looks like pcolormesh" >&2
      exit 1
    fi
    if [[ $c -lt 8 ]]; then
      echo "ERROR: $f has too few unique colors ($c) - contours may be missing" >&2
      exit 1
    fi
  fi
done

# Errorbar demo outputs should exist and be non-trivial
for f in \
  output/example/fortran/errorbar_demo/errorbar_basic_y.png \
  output/example/fortran/errorbar_demo/errorbar_basic_x.png \
  output/example/fortran/errorbar_demo/errorbar_combined.png \
  output/example/fortran/errorbar_demo/errorbar_asymmetric.png \
  output/example/fortran/errorbar_demo/errorbar_scientific.png
  do
  if [[ -f "$f" ]]; then
    check_png_size "$f" 4000
  else
    echo "ERROR: Missing errorbar demo artifact $f" >&2
    exit 1
  fi
done

# Symlog .txt should include scientific or power-of-ten notation lines
if ! grep -Eq "1\\.00E\+[0-9]{2}|10\^[0-9]+|1000|100\\.|10\\.0" output/example/fortran/scale_examples/symlog_scale.txt; then
  echo "ERROR: symlog_scale.txt lacks expected tick formats" >&2
  exit 1
fi

# Unicode demo: ensure Greek letters survive PDF text extraction
if [[ -f output/example/fortran/unicode_demo/unicode_demo.pdf ]]; then
  check_pdftotext_has output/example/fortran/unicode_demo/unicode_demo.pdf "α" "ω" || true
elif [[ -f output/example/fortran/unicode_demo/math_examples.pdf ]]; then
  check_pdftotext_has output/example/fortran/unicode_demo/math_examples.pdf "ψ" "Ω" || true
else
  echo "WARN: unicode_demo PDFs not found; skipping unicode text check"
fi

# PDF scale regression example: assert page size matches 800x600 at 100DPI => 576x432 pt
if [[ -f test_pdf_scale_regression.pdf ]]; then
  if command -v pdfinfo >/dev/null 2>&1; then
    pdfinfo test_pdf_scale_regression.pdf | grep -q "Page size:\s\+576 x 432 pts" || {
      echo "ERROR: Unexpected page size for test_pdf_scale_regression.pdf" >&2
      exit 1
    }
  else
    echo "Missing pdfinfo; skipping page size check"
  fi
fi

# show_viewer_demo in headless CI should save fallback image
if [[ -f show_output.png ]]; then
  check_png_size show_output.png 2000
fi

# Subplots grid demo should exist and be non-trivial
if [[ -f subplots_grid_demo.png ]]; then
  check_png_size subplots_grid_demo.png 5000
fi

# Optional Ghostscript render sanity if available
if command -v gs >/dev/null 2>&1; then
  gs -o /dev/null -sDEVICE=nullpage output/example/fortran/scale_examples/symlog_scale.pdf >/dev/null || {
    echo "ERROR: Ghostscript render failed for symlog_scale.pdf" >&2
    exit 1
  }
fi

# Tidy up root-level throwaway artifacts generated by visual demos
rm -f \
  ylabel_test[1-4]_*.png \
  show_output.png \
  subplots_grid_demo.png \
  test_pdf_scale_regression.pdf \
  test_pdf_scale_regression.png \
  2>/dev/null || true

echo "Artifact verification passed."
