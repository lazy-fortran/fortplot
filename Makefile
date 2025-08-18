# Allow additional arguments to be passed
ARGS ?=

# FPM flags for C compilation compatibility
FPM_FLAGS = --flag -fPIC

.PHONY: all build example debug test clean help matplotlib example_python example_matplotlib doc coverage create_build_dirs validate-output test-docs

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS) $(ARGS)

# Build and run the examples
example: create_build_dirs
	fpm run --example $(FPM_FLAGS) $(ARGS)

# Build and run the apps for debugging
debug:
	fpm run $(FPM_FLAGS) $(ARGS)

# Run tests
test:
	fpm test $(FPM_FLAGS) $(ARGS)

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
	ford doc.md
	# Copy example media files to doc build directory for proper linking
	mkdir -p build/doc/media/examples
	if [ -d doc/media/examples ]; then cp -r doc/media/examples/* build/doc/media/examples/ 2>/dev/null || true; fi

# Generate coverage report
coverage:
	@echo "Cleaning old coverage data..."
	find . -name '*.gcda' -delete
	find . -name '*.gcno' -delete
	@echo "Building with coverage flags..."
	fpm build --flag '-fprofile-arcs -ftest-coverage'
	@echo "Running tests with coverage..."
	fpm test --flag '-fprofile-arcs -ftest-coverage'
	@echo "Generating coverage report..."
	@echo "Attempting coverage generation with gcovr..." && \
	(gcovr --root . --exclude 'thirdparty/*' --exclude 'build/*' --exclude 'doc/*' --exclude 'example/*' --exclude 'test/*' --exclude 'app/*' --keep --txt -o coverage.txt --print-summary 2>/dev/null || \
	 echo "GCOVR WARNING: Coverage analysis had processing issues (common with FPM-generated coverage data)" && \
	 echo "Coverage files found: $$(find . -name '*.gcda' | wc -l) data files" && \
	 echo "Coverage analysis attempted but may be incomplete due to FPM/gcovr compatibility issues" > coverage.txt)
	@echo "Coverage analysis completed: coverage.txt"

# Validate functional output generation
validate-output: create_build_dirs
	@echo "Running functional output validation tests..."
	@mkdir -p output/test
	fpm test $(FPM_FLAGS) --target test_output_validation
	@echo "Functional output validation completed successfully"

# Test documentation examples
test-docs: create_build_dirs
	@echo "Running documentation example validation..."
	@mkdir -p output/test
	fpm test $(FPM_FLAGS) --target test_documentation_examples
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
	@mkdir -p output/example/fortran/stateful_streamplot
	@mkdir -p output/example/fortran/histogram_demo
	@mkdir -p output/example/fortran/subplot_demo
	@mkdir -p output/example/fortran/bar_chart_demo
	@mkdir -p output/example/fortran/errorbar_demo
	@mkdir -p output/example/fortran/disconnected_lines

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"
	@echo "  example          - Build and run all Fortran examples"
	@echo "  example_python   - Run Python examples with fortplot"
	@echo "  example_matplotlib - Run Python examples with matplotlib (comparison)"
	@echo "  debug            - Build and run apps for debugging"
	@echo "  test             - Run all tests"
	@echo "  validate-output  - Run functional output validation tests"
	@echo "  test-docs        - Test documentation examples"
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
