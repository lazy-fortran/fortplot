# Allow additional arguments to be passed
ARGS ?=

# FPM flags for C compilation compatibility
FPM_FLAGS = --flag -fPIC

.PHONY: all build example debug test clean help matplotlib example_python example_matplotlib doc coverage create_build_dirs

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
	mkdir -p build/doc/example
	if [ -d build/example ]; then cp -r build/example/* build/doc/example/ 2>/dev/null || true; fi

# Generate coverage report
coverage:
	@echo "Cleaning old coverage data..."
	find . -name '*.gcda' -delete
	@echo "Building with coverage flags..."
	fpm build --flag '-fprofile-arcs -ftest-coverage'
	@echo "Running tests with coverage..."
	fpm test --flag '-fprofile-arcs -ftest-coverage'
	@echo "Generating coverage report..."
	gcovr --root . --exclude 'thirdparty/*' --exclude 'build/*' --exclude 'doc/*' --exclude 'example/*' --exclude 'test/*' --txt -o coverage.txt --print-summary
	@echo "Coverage report generated: coverage.txt"

# Create build directories for examples
create_build_dirs:
	@mkdir -p build/example/basic_plots
	@mkdir -p build/example/line_styles
	@mkdir -p build/example/marker_demo
	@mkdir -p build/example/format_string_demo
	@mkdir -p build/example/contour_demo
	@mkdir -p build/example/colored_contours
	@mkdir -p build/example/pcolormesh_demo
	@mkdir -p build/example/streamplot_demo
	@mkdir -p build/example/ascii_heatmap
	@mkdir -p build/example/scale_examples
	@mkdir -p build/example/legend_demo
	@mkdir -p build/example/legend_box_demo
	@mkdir -p build/example/unicode_demo
	@mkdir -p build/example/show_viewer_demo
	@mkdir -p build/example/smart_show_demo
	@mkdir -p build/example/animation
	@mkdir -p build/example/stateful_streamplot

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"
	@echo "  example          - Build and run all Fortran examples"
	@echo "  example_python   - Run Python examples with fortplot"
	@echo "  example_matplotlib - Run Python examples with matplotlib (comparison)"
	@echo "  debug            - Build and run apps for debugging"
	@echo "  test             - Run all tests"
	@echo "  coverage         - Generate coverage report"
	@echo "  doc              - Build documentation with FORD"
	@echo "  clean       - Clean build artifacts"
	@echo "  release     - Build with optimizations"
	@echo "  run-release - Run optimized build"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Pass additional fpm arguments using ARGS variable:"
	@echo "  make example ARGS=\"basic_plots\""
	@echo "  make test ARGS=\"--target test_specific\""
	@echo "  make debug ARGS=\"--target debug_feature\""
	@echo ""
	@echo "This project uses STB TrueType and STB Image Write for text and PNG compression."
	@echo "All dependencies are included as header-only libraries."
	@echo "No external packages required."
