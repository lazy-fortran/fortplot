# Allow additional arguments to be passed
ARGS ?=

.PHONY: all build example debug test clean help matplotlib example_python example_matplotlib doc

# Default target
all: build

# Build the project
build:
	fpm build $(ARGS)

# Build and run the examples
example:
	fpm run --example $(ARGS)

# Build and run the apps for debugging
debug:
	fpm run $(ARGS)

# Run tests
test:
	fpm test $(ARGS)

# Run Python examples with fortplotlib (default mode)
example_python:
	@echo "Running Python examples with fortplotlib..."
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

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"
	@echo "  example          - Build and run all Fortran examples"
	@echo "  example_python   - Run Python examples with fortplotlib"
	@echo "  example_matplotlib - Run Python examples with matplotlib (comparison)"
	@echo "  debug            - Build and run apps for debugging"
	@echo "  test             - Run all tests"
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
