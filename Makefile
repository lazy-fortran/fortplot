# Makefile for PNG Draft Fortran Project with FreeType
# Handles compilation with FreeType C wrapper and all required libraries

# Use pkg-config to get dynamic library flags
FREETYPE_CFLAGS := $(shell pkg-config --cflags freetype2)
FREETYPE_LIBS := $(shell pkg-config --libs freetype2)
ZLIB_LIBS := $(shell pkg-config --libs zlib || echo "-lz")

# Combine all flags
ALL_CFLAGS = $(FREETYPE_CFLAGS)
ALL_LIBS = $(FREETYPE_LIBS) $(ZLIB_LIBS)

# FPM commands with full library support
FPM_FLAGS = --c-flag "$(ALL_CFLAGS)" --link-flag "$(ALL_LIBS)"

# Allow additional arguments to be passed
ARGS ?=

.PHONY: all build example debug test clean help check-deps ref

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

# Generate Python reference plots for visual comparison
ref:
	@echo "Generating Python matplotlib reference plots..."
	@python3 example/basic_plots/basic_plots_ref.py
	@python3 example/line_styles/line_styles_ref.py
	@python3 example/scale_examples/scale_examples_ref.py
	@python3 example/contour_demo/contour_demo_ref.py
	@echo "Reference plots generated! Compare *_ref.png files with fortplotlib output."

# Clean build artifacts
clean:
	echo y | fpm clean
	rm -f *.png *.pdf
	find example/ -name "*.png" -o -name "*.pdf" -o -name "*.txt" | xargs rm -f

# Build with release optimizations
release:
	fpm build --profile release $(FPM_FLAGS) $(ARGS)

# Run with release optimizations
run-release:
	fpm run --profile release $(FPM_FLAGS) $(ARGS)

# Check dependencies and show detected flags
check-deps:
	@echo "Checking dependencies with pkg-config..."
	@echo "FreeType CFLAGS: $(FREETYPE_CFLAGS)"
	@echo "FreeType LIBS:   $(FREETYPE_LIBS)"
	@echo "Zlib LIBS:       $(ZLIB_LIBS)"
	@echo "Combined CFLAGS: $(ALL_CFLAGS)"
	@echo "Combined LIBS:   $(ALL_LIBS)"
	@echo ""
	@echo "FPM command will be:"
	@echo "fpm <target> $(FPM_FLAGS)"

# Help target
help:
	@echo "Available targets:"
	@echo "  build       - Compile the project"
	@echo "  example     - Build and run all examples"
	@echo "  debug       - Build and run apps for debugging"
	@echo "  ref         - Generate Python matplotlib reference plots"
	@echo "  test        - Run all tests"
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
	@echo "This project uses FreeType for text rendering via a C wrapper."
	@echo "Library detection uses pkg-config for portability."
	@echo "Required packages: freetype2, zlib"
