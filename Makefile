# Makefile for fortplotlib Fortran Project with STB TrueType
# Handles compilation with STB TrueType C wrapper and all required libraries

# No external library dependencies needed - using STB headers only
ALL_CFLAGS = 
ALL_LIBS =

# FPM commands with full library support
FPM_FLAGS = --c-flag "$(ALL_CFLAGS)" --link-flag "$(ALL_LIBS)"

# Allow additional arguments to be passed
ARGS ?=

.PHONY: all build example debug test clean help check-deps matplotlib example_python example_matplotlib

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS) $(ARGS)

# Build and run the examples
example:
	fpm run $(FPM_FLAGS) --example $(ARGS)

# Build and run the apps for debugging
debug:
	fpm run $(FPM_FLAGS) $(ARGS)

# Run tests
test:
	fpm test $(FPM_FLAGS) $(ARGS)

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
	rm -f *.png *.pdf
	find example/ -name "*.png" -o -name "*.pdf" -o -name "*.txt" -not -name "CMakeLists.txt" | xargs rm -f

# Build with release optimizations
release:
	fpm build --profile release $(FPM_FLAGS) $(ARGS)

# Run with release optimizations
run-release:
	fpm run --profile release $(FPM_FLAGS) $(ARGS)

# Check dependencies and show detected flags
check-deps:
	@echo "Checking dependencies..."
	@echo "Combined CFLAGS: $(ALL_CFLAGS)"
	@echo "Combined LIBS:   $(ALL_LIBS)"
	@echo ""
	@echo "FPM command will be:"
	@echo "fpm <target> $(FPM_FLAGS)"

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"
	@echo "  example          - Build and run all Fortran examples"
	@echo "  example_python   - Run Python examples with fortplotlib"
	@echo "  example_matplotlib - Run Python examples with matplotlib (comparison)"
	@echo "  debug            - Build and run apps for debugging"
	@echo "  test             - Run all tests"
	@echo "  clean       - Clean build artifacts"
	@echo "  release     - Build with optimizations"
	@echo "  run-release - Run optimized build"
	@echo "  check-deps  - Show detected library flags"
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
