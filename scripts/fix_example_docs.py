#!/usr/bin/env python3
"""
Fix example documentation files that have PDF content instead of ASCII content.
This addresses the GitHub issue about "Some text is PDF instead of readable content".
"""

import os
import re
import glob
from pathlib import Path

def fix_ascii_content(md_file):
    """Fix ASCII content in markdown file by replacing PDF content with actual ASCII."""
    print(f"Fixing {md_file}...")
    
    # Read the markdown file
    with open(md_file, 'r') as f:
        content = f.read()
    
    # Check if it contains PDF content
    if '%PDF' not in content:
        print(f"  No PDF content found, skipping.")
        return False
    
    # Extract example name from file name
    example_name = Path(md_file).stem
    
    # Find corresponding ASCII files
    ascii_files = glob.glob(f"output/example/fortran/{example_name}/*.txt")
    
    if not ascii_files:
        print(f"  No ASCII files found for {example_name}")
        return False
    
    # For each ASCII file, replace the PDF content with ASCII content
    changes_made = False
    for ascii_file in ascii_files:
        ascii_filename = Path(ascii_file).name
        ascii_basename = ascii_filename.replace('.txt', '')
        
        # Read the actual ASCII content
        try:
            with open(ascii_file, 'r') as f:
                ascii_content = f.read()
        except:
            print(f"  Could not read {ascii_file}")
            continue
        
        # Find PDF blocks and replace them with ASCII content
        # Look for pattern: ```\n%PDF-1.4 ... %%EOF\n```
        pdf_pattern = r'```\n%PDF-1\.4.*?%%EOF\n```'
        
        if re.search(pdf_pattern, content, re.DOTALL):
            # Replace the PDF block with ASCII content
            new_block = f"```\n{ascii_content.strip()}\n```"
            content = re.sub(pdf_pattern, new_block, content, flags=re.DOTALL)
            changes_made = True
            print(f"  Replaced PDF content with ASCII for {ascii_basename}")
    
    if changes_made:
        # Write the fixed content back
        with open(md_file, 'w') as f:
            f.write(content)
        print(f"  Fixed {md_file}")
        return True
    else:
        print(f"  No changes made to {md_file}")
        return False

def main():
    """Fix all example documentation files."""
    # Find all markdown files in doc/example/
    md_files = glob.glob("doc/example/*.md")
    
    print(f"Found {len(md_files)} markdown files to check")
    
    fixed_count = 0
    for md_file in sorted(md_files):
        if fix_ascii_content(md_file):
            fixed_count += 1
    
    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()