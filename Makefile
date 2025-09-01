# Allow additional arguments to be passed
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

.PHONY: all build example debug test clean help matplotlib example_python example_matplotlib doc coverage create_build_dirs create_test_dirs validate-output test-docs verify-functionality verify-setup verify-size-compliance issue-branch issue-open-pr pr-merge pr-cleanup issue-loop issue-loop-dry test-python-bridge-example

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
	$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) $(ARGS)

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
	@# Regression guard for Issue #985 (PDF coordinate mapping)
	@$(TIMEOUT_PREFIX) fpm test $(FPM_FLAGS_TEST) --target test_pdf_coordinate_mapping_985 || exit 1
	@# Regression guard for Issue #995 (PDF axes stroke color should be black)
	@$(TIMEOUT_PREFIX) python3 scripts/test_pdf_axes_color_black.py || exit 1
	@# Security regression tests for Python bridge stdin handling (PR #1010)
	@$(TIMEOUT_PREFIX) python3 scripts/test_python_bridge_security.py || exit 1
	@# Basic non-interactive Python bridge functionality using example command file (fixes #919)
	@chmod +x scripts/test_python_bridge_example.sh && $(TIMEOUT_PREFIX) ./scripts/test_python_bridge_example.sh || exit 1
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
	# Run FORD to generate documentation structure
	ford doc.md
	# Copy example media files to doc build directory AFTER running FORD
	mkdir -p build/doc/media/examples
	# Copy from doc/media if it exists (GitHub Actions workflow populates this)
	if [ -d doc/media/examples ]; then cp -r doc/media/examples/* build/doc/media/examples/ 2>/dev/null || true; fi
	# Also copy directly from output directory if available (for local builds)
	for dir in output/example/fortran/*/; do \
        if [ -d "$$dir" ]; then \
            example_name=$$(basename "$$dir"); \
            mkdir -p "build/doc/media/examples/$$example_name"; \
            cp "$$dir"*.png "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
            cp "$$dir"*.txt "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
            cp "$$dir"*.pdf "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
            cp "$$dir"*.mp4 "build/doc/media/examples/$$example_name/" 2>/dev/null || true; \
        fi; \
    done

# Generate coverage report
coverage:
	@echo "Cleaning old coverage data..."
	# Be tolerant to permission or missing-file issues during cleanup
	find . -name '*.gcda' -delete 2>/dev/null || true
	find . -name '*.gcno' -delete 2>/dev/null || true
	@echo "Building with coverage flags..."
	fpm build --flag '-fprofile-arcs -ftest-coverage'
	@echo "Running tests with coverage..."
	fpm test --flag '-fprofile-arcs -ftest-coverage'
	@echo "Generating coverage report..."
	@echo "Attempting coverage generation with gcovr..." ; \
	if gcovr --root . \
	    --exclude 'thirdparty/*' --exclude 'build/*' --exclude 'doc/*' \
	    --exclude 'example/*' --exclude 'test/*' --exclude 'app/*' \
	    --keep --txt -o coverage.txt --print-summary 2>/dev/null ; then \
	  echo "gcovr completed successfully" ; \
	else \
	  echo "GCOVR WARNING: Coverage analysis had processing issues (common with FPM-generated coverage data)" ; \
	  echo "Coverage files found: $$(find . -name '*.gcda' | wc -l) data files" ; \
	  echo "Coverage analysis attempted but may be incomplete due to FPM/gcovr compatibility issues" > coverage.txt ; \
	fi
	@echo "Cleaning up intermediate coverage files..."
	find . -name '*.gcov.json.gz' -delete 2>/dev/null || true
	find . -name '*.gcda' -delete 2>/dev/null || true
	find . -name '*.gcno' -delete 2>/dev/null || true
	@echo "Coverage analysis completed: coverage.txt"

# Validate functional output generation
validate-output: create_build_dirs
	@echo "Running functional output validation tests..."
	@mkdir -p output/test
	fpm test $(FPM_FLAGS_TEST) --target test_output_validation
	@echo "Functional output validation completed successfully"

# Test documentation examples
test-docs: create_build_dirs
	@echo "Running documentation example validation..."
	@mkdir -p output/test
	fpm test $(FPM_FLAGS_TEST) --target test_documentation_examples
	@echo "Documentation example validation completed successfully"

# Run comprehensive functional tests
test-functional: test validate-output test-docs
	@echo "=== ALL FUNCTIONAL TESTS PASSED ==="

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
	@echo "  coverage         - Generate coverage report"
	@echo "  doc              - Build documentation with FORD"
	@echo "  clean       - Clean build artifacts"
	@echo "  release     - Build with optimizations"
	@echo "  run-release - Run optimized build"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Pass additional fmp arguments using ARGS variable:"
	@echo "  make example ARGS=\"basic_plots\""
	@echo "  make test ARGS=\"--target test_specific\""
	@echo "  make debug ARGS=\"--target debug_feature\""
	@echo ""
	@echo "This project uses STB TrueType and STB Image Write for text and PNG compression."
	@echo "All dependencies are included as header-only libraries."
	@echo "No external packages required."
