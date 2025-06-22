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

.PHONY: all build run test clean help check-deps

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS)

# Build and run the main program
run:
	fpm run $(FPM_FLAGS) --example

# Run tests
test:
	fpm test $(FPM_FLAGS)

# Clean build artifacts
clean:
	echo y | fpm clean
	rm -f *.png *.pdf *.txt
	find example/ -name "*.png" -o -name "*.pdf" -o -name "*.txt" | xargs rm -f

# Build with release optimizations
release:
	fpm build --profile release $(FPM_FLAGS)

# Run with release optimizations
run-release:
	fpm run --profile release $(FPM_FLAGS)

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
	@echo "  run         - Build and run the main program"
	@echo "  test        - Run all tests"
	@echo "  clean       - Clean build artifacts"
	@echo "  release     - Build with optimizations"
	@echo "  run-release - Run optimized build"
	@echo "  check-deps  - Show detected library flags"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "This project uses FreeType for text rendering via a C wrapper."
	@echo "Library detection uses pkg-config for portability."
	@echo "Required packages: freetype2, zlib"
