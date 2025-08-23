#!/bin/bash
# Clean all non-ASCII characters from example documentation files
# This ensures FORD can parse them correctly in all CI environments

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
EXAMPLE_DIR="$PROJECT_ROOT/doc/example"

echo "=== Cleaning UTF-8 non-ASCII characters from example documentation ==="
echo "Project root: $PROJECT_ROOT"
echo "Example directory: $EXAMPLE_DIR"
echo ""

if [[ ! -d "$EXAMPLE_DIR" ]]; then
    echo "ERROR: Example directory not found: $EXAMPLE_DIR"
    exit 1
fi

# Function to clean a single file
clean_file() {
    local file="$1"
    local basename=$(basename "$file")
    
    echo "Processing: $basename"
    
    # Check current encoding
    local encoding=$(file -bi "$file" | cut -d'=' -f2)
    echo "  Current encoding: $encoding"
    
    # Create backup
    cp "$file" "${file}.backup"
    
    # Use tr to remove all non-ASCII characters (safer than sed with complex UTF-8)
    # Keep only printable ASCII characters: tab(9), newline(10), carriage return(13), space(32) through tilde(126)
    tr -cd '\11\12\15\40-\176' < "$file" > "${file}.temp"
    mv "${file}.temp" "$file"
    
    # Check final encoding
    local new_encoding=$(file -bi "$file" | cut -d'=' -f2)
    echo "  New encoding: $new_encoding"
    
    # Show what changed
    if ! cmp -s "$file" "${file}.backup"; then
        echo "  Changes made to file"
        # Show diff but limit output
        diff -u "${file}.backup" "$file" | head -20 || true
        echo "  ..."
    else
        echo "  No changes needed"
    fi
    
    echo ""
}

# Find and process all markdown files
echo "Finding markdown files in $EXAMPLE_DIR..."
find "$EXAMPLE_DIR" -name "*.md" -type f | while read -r file; do
    clean_file "$file"
done

echo "=== Summary ==="
echo "Processed files:"
find "$EXAMPLE_DIR" -name "*.md" -type f | while read -r file; do
    basename=$(basename "$file")
    encoding=$(file -bi "$file" | cut -d'=' -f2)
    echo "  $basename: $encoding"
done

echo ""
echo "Backup files created with .backup extension"
echo "To remove backups: find $EXAMPLE_DIR -name '*.backup' -delete"
echo ""
echo "=== Cleaning completed ==="