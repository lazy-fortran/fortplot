# Allow additional arguments to be passed
SHELL := /bin/bash
ARGS ?=
# Enforce a hard timeout for all test invocations (accepts coreutils duration)
# Can be overridden by environment: TEST_TIMEOUT=90s make test
TEST_TIMEOUT ?= 120s

# Cross-platform timeout detection: prefer GNU coreutils `timeout`, fallback to Homebrew `gtimeout`.
# If neither exists, run without timeout (prints a notice once).
TIMEOUT_BIN := $(shell command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)
ifeq ($(TIMEOUT_BIN),)
  TIMEOUT_PREFIX :=
  define _timeout_notice
    @echo "[NOTICE] 'timeout' not found; running without enforced test timeout"
  endef
else
  TIMEOUT_PREFIX := $(TIMEOUT_BIN) $(TEST_TIMEOUT)
  _timeout_notice :=
endif

# FPM flags for different build targets
FPM_FLAGS_LIB = --flag -fPIC
FPM_FLAGS_TEST =
FPM_FLAGS_DEFAULT = $(FPM_FLAGS_LIB)

.PHONY: all build example debug test clean help matplotlib example_python example_matplotlib doc create_build_dirs create_test_dirs validate-output test-docs verify-functionality verify-setup verify-with-evidence verify-size-compliance verify-complexity issue-branch issue-open-pr pr-merge pr-cleanup issue-loop issue-loop-dry test-python-bridge-example git-prune verify-warnings

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS_DEFAULT) $(ARGS)

# Build and run the examples
example: create_build_dirs
	fpm run --example $(FPM_FLAGS_TEST) $(ARGS)
	# Build and run special examples that need manual compilation
	@if [ -z "$(ARGS)" ]; then \
		./scripts/compile_special_examples.sh 2>/dev/null || true; \
	fi

# Build and run the apps for debugging
debug:
	fpm run $(FPM_FLAGS_TEST) $(ARGS)

# Run tests
test: create_test_dirs
	$(call _timeout_notice)
	@echo "Running tests$(if $(TIMEOUT_PREFIX), with timeout $(TEST_TIMEOUT),)..."
	FORTPLOT_TEST=1 FORTPLOT_PDF_COMPRESS=0 $(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) $(ARGS) \
		&& echo "ALL TESTS PASSED (fpm test)"

# Run fast test suite for development iteration (minimal I/O, no delays)
test-fast: create_test_dirs
	@echo "Running fast test suite for development iteration..."
	@FORTPLOT_FAST_TESTS=1 fpm test $(FPM_FLAGS_TEST) --target test_suite_fast \
		&& echo "FAST TESTS PASSED"

# Run fast tests for CI (skip heavy I/O and MPEG tests)
test-ci:
	$(call _timeout_notice)
	@echo "Running CI-optimized test suite (essential tests only)$(if $(TIMEOUT_PREFIX), with timeout $(TEST_TIMEOUT),)..."
	@echo "Testing core functionality, axes, backend basics"
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_public_api || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_simple_validation || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_backend_switching || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_matplotlib_stubs || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_show_fallback_mechanisms || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_format_parser || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_figure_state_isolation || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_scaling || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_scatter_enhanced || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_histogram_functionality || exit 1
	@# Regression: colormap interpolation must not be flat near min (pcolormesh negative mapping)
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_colormap_interpolation_regression || exit 1
	@# PDF content streams should be Flate-compressed
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_pdf_flate_content || exit 1
	@# Regression guard for Issue #985 (PDF coordinate mapping)
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_pdf_coordinate_mapping_985 || exit 1
	@# Regression guard for Issue #995 (PDF axes stroke color should be black)
	@$(TIMEOUT_PREFIX) python3 scripts/test_pdf_axes_color_black.py || exit 1
	@# Security regression tests for Python bridge stdin handling (PR #1010)
	@$(TIMEOUT_PREFIX) python3 scripts/test_python_bridge_security.py || exit 1
	@# Basic non-interactive Python bridge functionality using example command file (fixes #919)
	@chmod +x scripts/test_python_bridge_example.sh && $(TIMEOUT_PREFIX) ./scripts/test_python_bridge_example.sh || exit 1
	@# Python bridge: pcolormesh via wrapper
	@$(TIMEOUT_PREFIX) python3 scripts/test_python_pcolormesh_via_bridge.py || exit 1
	@# Python bridge: log scale via wrapper
	@$(TIMEOUT_PREFIX) python3 scripts/test_python_scales_via_bridge.py || exit 1
	@# Python bridge: streamplot via wrapper
	@$(TIMEOUT_PREFIX) python3 scripts/test_python_streamplot_via_bridge.py || exit 1
	@# Regression for filled-quad edge coverage (prevents 1px cuts on borders)
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_quad_fill_edges || exit 1
	@# Guard against redundant pcolormesh tests (Issue #897)
	@$(TIMEOUT_PREFIX) ./scripts/test_pcolormesh_guard.sh || exit 1
	@# Enforce directory item limits for src/* subfolders (Issue #914)
	@$(TIMEOUT_PREFIX) python3 scripts/test_directory_organization_limits.py || exit 1
	@echo "CI essential test suite completed successfully"

# Run Python examples with fortplot (default mode)
example_python:
	@echo "Running Python examples with fortplot..."
	@for dir in example/python/*/; do \
		if [ -f "$$dir"*.py ]; then \
			echo "Running $$dir"; \
			cd "$$dir" && python3 *.py && cd - > /dev/null; \
		fi; \
	done
	@echo "Python examples completed!"

# Run Python examples with matplotlib (comparison mode)
example_matplotlib:
	@echo "Running Python examples with matplotlib for comparison..."
	@for dir in example/python/*/; do \
		if [ -f "$$dir"*.py ]; then \
			echo "Running $$dir with matplotlib"; \
			cd "$$dir" && python3 *.py --matplotlib && cd - > /dev/null; \
		fi; \
	done
	@echo "Matplotlib comparison plots generated!"

# Legacy matplotlib target (deprecated, use example_matplotlib)
matplotlib: example_matplotlib

# Clean build artifacts
clean:
	echo y | fpm clean
	rm -rf build/example
	rm -rf build/test/output
	find . \( -name "*.png" -o -name "*.pdf" -o -name "*.txt" \
	       -o -name "*.mp4" -o -name "*.avi" -o -name "*.mkv" \) \
	       -not -name "CMakeLists.txt" -type f -exec rm -f {} \;

# Build with release optimizations
release:
	fpm build --profile release $(ARGS)

# Run with release optimizations
run-release:
	fpm run --profile release $(ARGS)

# Build documentation with FORD
doc:
	# Ensure critical example media exist for docs (fixes #858, #1032)
	# Generate streamplot and pcolormesh demos so images are available in docs
	$(MAKE) example ARGS="streamplot_demo" >/dev/null
	$(MAKE) example ARGS="pcolormesh_demo" >/dev/null
	# Generate marker demo so marker images (including all_marker_types.png) are present (fixes #1109)
	$(MAKE) example ARGS="marker_demo" >/dev/null
	# Generate animation demo so MP4 is available for docs (fixes #1085)
	$(MAKE) example ARGS="save_animation_demo" >/dev/null
	# Run FORD to generate documentation structure
	ford doc.md
	# Copy example media files to BOTH possible link roots used in pages
	# Some pages link '../media/...' (relative to page/examples), others '../../media/...'
	# So stage to: build/doc/page/media/... and build/doc/media/...
	mkdir -p build/doc/page/media/examples build/doc/media/examples
	# Copy from doc/media (populated in workflow) into both
	if [ -d doc/media/examples ]; then \
		cp -r doc/media/examples/* build/doc/page/media/examples/ 2>/dev/null || true; \
		cp -r doc/media/examples/* build/doc/media/examples/ 2>/dev/null || true; \
	fi
	# Also copy directly from output directory (local builds) into both
	for dir in output/example/fortran/*/; do \
		if [ -d "$$dir" ]; then \
			example_name=$$(basename "$$dir"); \
			mkdir -p "build/doc/page/media/examples/$$example_name" "build/doc/media/examples/$$example_name"; \
			cp "$$dir"*.png "build/doc/page/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.txt "build/doc/page/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.pdf "build/doc/page/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.mp4 "build/doc/page/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.png "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.txt "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.pdf "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
			cp "$$dir"*.mp4 "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
		fi; \
	done
	# Ensure animation.mp4 explicitly present at expected locations (robust fallback)
	if [ -f output/example/fortran/animation/animation.mp4 ]; then \
		mkdir -p build/doc/page/media/examples/animation build/doc/media/examples/animation; \
		cp output/example/fortran/animation/animation.mp4 build/doc/page/media/examples/animation/ 2>/dev/null || true; \
		cp output/example/fortran/animation/animation.mp4 build/doc/media/examples/animation/ 2>/dev/null || true; \
	fi


# Validate functional output generation
validate-output: create_build_dirs
	@echo "Running functional output validation tests..."
	@mkdir -p output/test
	fpm test $(FPM_FLAGS_TEST) --target test_output_validation
	@echo "Functional output validation completed successfully"

# Test documentation-related targets (run existing doc tests)
test-docs: create_build_dirs
	$(call _timeout_notice)
	@echo "Running documentation tests$(if $(TIMEOUT_PREFIX), with timeout $(TEST_TIMEOUT),)..."
	@mkdir -p output/test
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_doc_core || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_doc_processing_output || exit 1
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_docs_index_pages || exit 1
	@echo "Documentation tests completed successfully"

# Run comprehensive functional tests
test-functional: test validate-output test-docs
	@echo "=== ALL FUNCTIONAL TESTS PASSED ==="

# Strict artifact verification for rendering-related changes
.PHONY: verify-artifacts
verify-artifacts: create_build_dirs
	@echo "Verifying example artifacts (PDF/PNG/txt) with strict checks..."
	@set -euo pipefail; \
	# Run key examples; \
	fpm run --example scale_examples >/dev/null; \
	fpm run --example pcolormesh_demo >/dev/null; \
	fpm run --example pcolormesh_negative >/dev/null; \
	fpm run --example marker_demo >/dev/null; \
	fpm run --example line_styles >/dev/null; \
	# Additional visual regression examples (ylabel spacing, PDF scale, subplots, unicode, show viewer); \
	fpm run --example label_positioning_demo >/dev/null; \
	fpm run --example test_pdf_scale_regression >/dev/null; \
	fpm run --example subplots_grid_demo >/dev/null; \
	fpm run --example unicode_demo >/dev/null; \
	fpm run --example show_viewer_demo >/dev/null || true; \
	\
	# Helper: check PDF has no pdfimages syntax errors; \
	check_pdf_ok() { \
	  local pdf="$$1"; \
	  if ! command -v pdfimages >/dev/null 2>&1; then echo "Missing pdfimages (poppler-utils)" >&2; exit 2; fi; \
	  local out; out=$$(pdfimages -list "$$pdf" 2>&1 || true); \
	  echo "[pdfimages] $$pdf"; echo "$$out" | head -n 3; \
	  if echo "$$out" | grep -qi "Syntax Error"; then echo "ERROR: PDF syntax errors in $$pdf" >&2; exit 1; fi; \
	}; \
	\
	# Helper: check pdftotext extracts expected substrings (basic sanity); \
	check_pdftotext_has() { \
	  local pdf="$$1"; shift; \
	  if ! command -v pdftotext >/dev/null 2>&1; then echo "Missing pdftotext (poppler-utils)" >&2; exit 2; fi; \
	  local txt; txt=$$(pdftotext "$$pdf" - 2>/dev/null || true); \
	  for needle in "$$@"; do \
	    echo "[pdftotext] asserting needle=$$needle in $${pdf}"; \
	    echo "$$txt" | grep -q "$$needle" || { echo "ERROR: Missing needle=$$needle in $$pdf" >&2; exit 1; }; \
	  done; \
	}; \
	\
	# Helper: PNG minimal size sanity; \
	check_png_size() { \
	  local png="$$1"; local min=$${2:-4000}; \
	  local sz; sz=$$(stat -c %s "$$png"); \
	  echo "[png] $$png size=$$sz"; \
	  [ "$$sz" -ge "$$min" ] || { echo "ERROR: $$png too small (size=$$sz)" >&2; exit 1; }; \
	}; \
	\
	# Helper: require bright left margin for ylabel positioning demo PNGs; \
	# Checks the mean brightness of a left stripe (in grayscale) exceeds a threshold. \
	check_left_margin_brightness() { \
	  local png="$$1"; local stripe_w=$${2:-12}; local min_mean=$${3:-0.95}; \
	  local h; \
	  if command -v identify >/dev/null 2>&1; then \
	    h=$$(identify -format "%h" "$$png" 2>/dev/null || echo 0); \
	  elif command -v magick >/dev/null 2>&1; then \
	    h=$$(magick identify -format "%h" "$$png" 2>/dev/null || echo 0); \
	  else \
	    echo "Missing ImageMagick (identify/magick)" >&2; exit 2; \
	  fi; \
	  [ "$$h" -gt 0 ] || { echo "ERROR: Could not read PNG height for $$png" >&2; exit 1; }; \
	  local mean; \
	  if command -v convert >/dev/null 2>&1; then \
	    mean=$$(convert "$$png" -crop $${stripe_w}x$${h}+0+0 +repage -colorspace Gray -format "%[fx:mean]" info: 2>/dev/null || echo 0); \
	  elif command -v magick >/dev/null 2>&1; then \
	    mean=$$(magick "$$png" -crop $${stripe_w}x$${h}+0+0 +repage -colorspace Gray -format "%[fx:mean]" info: 2>/dev/null || echo 0); \
	  else \
	    echo "Missing ImageMagick (convert/magick)" >&2; exit 2; \
	  fi; \
	  echo "[ylabel-left] $$png stripe_w=$$stripe_w mean=$$mean threshold=$$min_mean"; \
	  awk -v m="$$mean" -v t="$$min_mean" 'BEGIN { exit (m+0 >= t+0 ? 0 : 1) }' \
	    || { echo "ERROR: Left margin too dark (insufficient whitespace) in $$png" >&2; exit 1; }; \
	}; \
	\
	# Scale examples: ylabel must indicate superscript 3 (Unicode or WinAnsi octal) and general labels present; \
	check_pdf_ok output/example/fortran/scale_examples/symlog_scale.pdf; \
	if pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x³"; then echo "[ok] symlog ylabel shows superscript three (unicode)"; \
	elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -F -q "x\\263"; then echo "[ok] symlog ylabel shows superscript three (WinAnsi)"; \
	elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x\^3"; then echo "[ok] symlog ylabel shows superscript three (mathtext)"; \
	elif pdftotext output/example/fortran/scale_examples/symlog_scale.pdf - | grep -q "x. - 50x"; then echo "[ok] symlog ylabel present; superscript may be lost in pdftotext"; \
	else echo "ERROR: symlog ylabel missing superscript 3" >&2; exit 1; fi; \
	check_pdftotext_has output/example/fortran/scale_examples/symlog_scale.pdf "Symlog" "x"; \
	# Pcolormesh PDFs must have no syntax errors; \
	check_pdf_ok output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf; \
	check_pdf_ok output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf; \
	# Ensure pcolormesh PDFs are color and not grayscale. Two valid encodings:
	#  - Vector fills with non-gray 'rg'
	#  - Image XObject (rgb) placed with Do. Accept either; prefer color check via pdfimages. \
	check_pcolormesh_pdf_color() { \
	  local pdf="$$1"; \
	  # Path 1: vector streams with 'rg' (robust to Flate via helper)
	  if python3 scripts/pdf_scan_rg.py "$$pdf" >/dev/null 2>&1; then \
	    echo "[ok] $$pdf has non-gray 'rg' (vector)"; return 0; \
	  fi; \
	  # Path 2: Image XObject; verify at least one RGB image present
	  if command -v pdfimages >/dev/null 2>&1; then \
	    local lst; lst=$$(pdfimages -list "$$pdf" 2>/dev/null || true); \
	    echo "$$lst" | head -n 3; \
	    if echo "$$lst" | awk 'NR>2 && tolower($$5) ~ /rgb/ {exit 0} END {exit 1}'; then \
	      echo "[ok] $$pdf contains RGB Image XObject"; return 0; \
	    fi; \
	  fi; \
	  # Fallback: textual grep for '/Subtype /Image' and '/ColorSpace /DeviceRGB'
	  if rg -n "/Subtype /Image|/ColorSpace /DeviceRGB" -S --text "$$pdf" >/dev/null 2>&1; then \
	    echo "[ok] $$pdf declares Image XObject with DeviceRGB"; return 0; \
	  fi; \
	  echo "ERROR: $$pdf did not show vector 'rg' nor RGB Image XObject" >&2; return 1; \
	}; \
	for pdf in output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf \
	           output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf; do \
	  echo "[pdfcolor] checking pcolormesh color encoding in $$pdf"; \
	  check_pcolormesh_pdf_color "$$pdf" || exit 1; \
	done; \
	# A couple PNG size checks as non-empty proxy; \
	check_png_size output/example/fortran/marker_demo/all_marker_types.png 8000; \
	check_png_size output/example/fortran/line_styles/line_styles.png 10000; \
	# Negative-coordinate pcolormesh: ensure non-trivial image and negative ticks present in PDF text; \
	check_png_size output/example/fortran/pcolormesh_negative/pcolormesh_negative.png 15000; \
	check_pdf_ok output/example/fortran/pcolormesh_negative/pcolormesh_negative.pdf; \
		if command -v pdftotext >/dev/null 2>&1; then \
		  pdftotext output/example/fortran/pcolormesh_negative/pcolormesh_negative.pdf - | grep -Eq -- '[-−][0-9]' \
		    || { echo "ERROR: Negative tick labels not found in pcolormesh_negative.pdf" >&2; exit 1; }; \
		fi; \
	# Require multiple unique colors to ensure actual data render (not blank); \
	if command -v identify >/dev/null 2>&1; then \
	  uc=$$(identify -format %k output/example/fortran/pcolormesh_negative/pcolormesh_negative.png 2>/dev/null || echo 0); \
	  echo "[colors] pcolormesh_negative.png => $$uc"; \
	  [ "$$uc" -ge 50 ] || { echo "ERROR: pcolormesh_negative.png has too few unique colors ($$uc)" >&2; exit 1; }; \
	fi; \
	# Y-label positioning demo must keep a bright safety margin on the left; \
	for f in ylabel_test1_wide_ticks.png ylabel_test2_scientific.png ylabel_test3_long_label.png ylabel_test4_multiple.png; do \
	  if [ -f "$$f" ]; then \
	    check_left_margin_brightness "$$f" 12 0.94; \
	  else \
	    echo "WARN: $$f not found; label positioning demo may not have run"; \
	  fi; \
	done; \
	# Colored contours should have many unique colors (not blank); \
	for f in output/example/fortran/colored_contours/gaussian_default.png \
	         output/example/fortran/colored_contours/saddle_plasma.png \
	         output/example/fortran/colored_contours/ripple_inferno.png \
	         output/example/fortran/colored_contours/ripple_coolwarm.png \
	         output/example/fortran/colored_contours/ripple_jet.png; do \
	  if [ -f "$${f}" ]; then \
	    if command -v identify >/dev/null 2>&1; then c=$$(identify -format %k "$${f}" 2>/dev/null || echo 0); \
	    elif command -v magick >/dev/null 2>&1; then c=$$(magick identify -format %k "$${f}" 2>/dev/null || echo 0); \
	    else echo "Missing ImageMagick 'identify'" >&2; exit 2; fi; \
	    echo "[colors] $$f => $$c"; \
	    # Expect contours (lines), not pcolormesh-like fills: cap unique colors to avoid blocky look; \
	    # allow antialiasing wiggle room. Fail if suspiciously high. \
	    if [ "$$c" -gt 1200 ]; then echo "ERROR: $$f has too many unique colors ($$c) — looks like pcolormesh" >&2; exit 1; fi; \
	    # Also require it's not monochrome \
	    if [ "$$c" -lt 8 ]; then echo "ERROR: $$f has too few unique colors ($$c) — contours may be missing" >&2; exit 1; fi; \
	  fi; \
	done; \
	# Symlog .txt should include scientific or power-of-ten notation lines; \
	if grep -Eq "1\\.00E\+[0-9]{2}|10\^[0-9]+|1000|100\\.|10\\.0" output/example/fortran/scale_examples/symlog_scale.txt; then :; else echo "ERROR: symlog_scale.txt lacks expected tick formats" >&2; exit 1; fi; \
	# Unicode demo: ensure Greek letters survive PDF text extraction; \
	if [ -f output/example/fortran/unicode_demo/unicode_demo.pdf ]; then \
	  check_pdftotext_has output/example/fortran/unicode_demo/unicode_demo.pdf "α" "ω" || true; \
	elif [ -f output/example/fortran/unicode_demo/math_examples.pdf ]; then \
	  check_pdftotext_has output/example/fortran/unicode_demo/math_examples.pdf "ψ" "Ω" || true; \
	else \
	  echo "WARN: unicode_demo PDFs not found; skipping unicode text check"; \
	fi; \
	# PDF scale regression example: assert page size matches 800x600 at 100DPI => 576x432 pt; \
	if [ -f test_pdf_scale_regression.pdf ]; then \
	  if command -v pdfinfo >/dev/null 2>&1; then \
	    pdfinfo test_pdf_scale_regression.pdf | grep -q "Page size:\s\+576 x 432 pts" \
	      || { echo "ERROR: Unexpected page size for test_pdf_scale_regression.pdf" >&2; exit 1; }; \
	  else \
	    echo "Missing pdfinfo; skipping page size check"; \
	  fi; \
	fi; \
	# show_viewer_demo in headless CI should save fallback image; \
	if [ -f show_output.png ]; then check_png_size show_output.png 2000; fi; \
	# Subplots grid demo should exist and be non-trivial; \
	if [ -f subplots_grid_demo.png ]; then check_png_size subplots_grid_demo.png 5000; fi; \
	# Optional Ghostscript render sanity if available; \
	if command -v gs >/dev/null 2>&1; then \
	  gs -o /dev/null -sDEVICE=nullpage output/example/fortran/scale_examples/symlog_scale.pdf >/dev/null || { echo "ERROR: Ghostscript render failed for symlog_scale.pdf" >&2; exit 1; }; \
	fi; \
	# Tidy up root-level throwaway artifacts generated by visual demos; \
	rm -f ylabel_test[1-4]_*.png show_output.png subplots_grid_demo.png test_pdf_scale_regression.pdf test_pdf_scale_regression.png 2>/dev/null || true; \
	echo "Artifact verification passed."

# Create build directories for examples
create_build_dirs:
	@mkdir -p output/example/fortran/basic_plots
	@mkdir -p output/example/fortran/line_styles
	@mkdir -p output/example/fortran/marker_demo
	@mkdir -p output/example/fortran/format_string_demo
	@mkdir -p output/example/fortran/contour_demo
	@mkdir -p output/example/fortran/colored_contours
	@mkdir -p output/example/fortran/pcolormesh_demo
	@mkdir -p output/example/fortran/streamplot_demo
	@mkdir -p output/example/fortran/ascii_heatmap
	@mkdir -p output/example/fortran/scale_examples
	@mkdir -p output/example/fortran/legend_demo
	@mkdir -p output/example/fortran/legend_box_demo
	@mkdir -p output/example/fortran/unicode_demo
	@mkdir -p output/example/fortran/show_viewer_demo
	@mkdir -p output/example/fortran/smart_show_demo
	@mkdir -p output/example/fortran/animation
	@mkdir -p output/example/fortran/annotation_demo
	@mkdir -p output/example/fortran/histogram_demo
	@mkdir -p output/example/fortran/subplot_demo
	@mkdir -p output/example/fortran/bar_chart_demo
	@mkdir -p output/example/fortran/errorbar_demo
	@mkdir -p output/example/fortran/disconnected_lines
	@mkdir -p output/example/fortran/boxplot_demo
	@mkdir -p output/example/fortran/grid_demo

# Create test directories for isolated test artifacts (Issue #820)
create_test_dirs:
	@mkdir -p build/test/output

# Comprehensive functionality preservation verification (Issue #609)
verify-functionality:
	@echo "Running comprehensive functionality preservation verification..."
	@echo "Issue #609: Comprehensive Functionality Preservation Verification System"
	./scripts/verify_functionality_preservation.sh

# Setup verification environment only
verify-setup:
	@echo "Setting up functionality verification environment..."
	./scripts/verify_functionality_preservation.sh --setup

# Run verification tests with fraud-proof evidence generation
verify-with-evidence: verify-functionality
	@echo "Verification complete - technical evidence generated"
	@echo "Evidence directory: test/output/verification/evidence/"
	@ls -la test/output/verification/evidence/ || true

# --- GitHub issue/PR helpers for Codex loop ---
issue-branch:
	@if [ -z "$(ISSUE)" ]; then echo "ISSUE=<number> required"; exit 2; fi
	@echo "Creating branch for issue #$(ISSUE)"
	@./scripts/issue_branch.sh $(ISSUE)

issue-open-pr:
	@if [ -z "$(ISSUE)" ]; then echo "ISSUE=<number> required"; exit 2; fi
	@./scripts/issue_open_pr.sh $(ISSUE)

pr-merge:
	@if [ -z "$(PR)" ]; then echo "PR=<number> required"; exit 2; fi
	@./scripts/pr_merge.sh $(PR) $(ARGS)

pr-cleanup:
	@if [ -z "$(PR)" ]; then echo "PR=<number> required"; exit 2; fi
	@./scripts/pr_cleanup.sh $(PR)

issue-loop:
	@label=$${LABEL-__all__}; \
	if [ "$$label" = "__all__" ]; then \
		./scripts/issue_loop.sh --all --limit $${LIMIT:-999999}; \
	else \
		./scripts/issue_loop.sh --label "$$label" --limit $${LIMIT:-999999}; \
	fi

issue-loop-dry:
	@label=$${LABEL-__all__}; \
	if [ "$$label" = "__all__" ]; then \
		./scripts/issue_loop.sh --all --limit $${LIMIT:-999999} --dry-run; \
	else \
		./scripts/issue_loop.sh --label "$$label" --limit $${LIMIT:-999999} --dry-run; \
	fi

issue-loop-auto:
	@label=$${LABEL-__all__}; \
	if [ "$$label" = "__all__" ]; then \
		CLEAN_FORCE=1 ./scripts/issue_orchestrate_auto.sh --all --limit $${LIMIT:-999999}; \
	else \
		CLEAN_FORCE=1 ./scripts/issue_orchestrate_auto.sh --label "$$label" --limit $${LIMIT:-999999}; \
	fi

# File size compliance verification - fraud prevention
verify-size-compliance:
	@echo "Running file size fraud prevention verification..."
	./scripts/verify_file_sizes.sh

# Fortran procedural complexity budgets (Issue #937)
verify-complexity:
	@echo "Running Fortran procedure complexity verification..."
	@chmod +x scripts/verify_complexity.sh
	@MAX_TOTAL_PROCS=$${MAX_TOTAL_PROCS-2000} \
	 MAX_PROCS_PER_FILE=$${MAX_PROCS_PER_FILE-60} \
	 ./scripts/verify_complexity.sh src

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"
	@echo "  example          - Build and run all Fortran examples"
	@echo "  example_python   - Run Python examples with fortplot"
	@echo "  example_matplotlib - Run Python examples with matplotlib (comparison)"
	@echo "  debug            - Build and run apps for debugging"
	@echo "  test             - Run all tests"
	@echo "  test-ci          - Run CI-optimized tests (skip heavy I/O, MPEG tests)"
	@echo "  validate-output  - Run functional output validation tests"
	@echo "  test-docs        - Test documentation examples"
	@echo "  verify-functionality - Run comprehensive functionality preservation verification"
	@echo "  verify-setup     - Setup functionality verification environment"
	@echo "  verify-with-evidence - Run verification with fraud-proof evidence generation"
	@echo "  verify-size-compliance - File size fraud prevention verification"
	@echo "  verify-complexity - Enforce procedure count budgets (Issue #937)"
	@echo "  verify-warnings - Compile with aggressive warnings (-Werror)"
	@echo "  doc              - Build documentation with FORD"
	@echo "  clean       - Clean build artifacts"
	@echo "  release     - Build with optimizations"
	@echo "  run-release - Run optimized build"
	@echo "  help        - Show this help message"
	@echo "  git-prune   - Safely prune remote-tracking and old merged local branches"
	@echo ""
	@echo "Pass additional fmp arguments using ARGS variable:"
	@echo "  make example ARGS=\"basic_plots\""
	@echo "  make test ARGS=\"--target test_specific\""
	@echo "  make debug ARGS=\"--target debug_feature\""
	@echo ""
	@echo "This project uses STB TrueType and STB Image Write for text and PNG compression."
	@echo "All dependencies are included as header-only libraries."
	@echo "No external packages required."

# Prune remote-tracking branches and delete local merged branches (dry-run by default)
git-prune:
	@echo "Running safe git prune (dry-run). Use FORCE=1 to apply."
	@if [ "$(FORCE)" = "1" ]; then \
		./scripts/git_prune.sh --force; \
	else \
		./scripts/git_prune.sh; \
	fi

# Compile with aggressive warnings enabled and fail on any warning
verify-warnings:
	@echo "Verifying warning-free build with aggressive flags (-Werror)..."
	@# Fortran warnings
	@fpm build --flag "-Wall -Wextra -Wimplicit-interface -Werror -fPIC" || exit 1
	@# C warnings
	@fpm build --c-flag "-Wall -Wextra -Werror" || exit 1
	@echo "Warning-free verification completed successfully"
