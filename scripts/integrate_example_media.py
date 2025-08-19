#!/usr/bin/env python3
"""
GitHub Pages Documentation Integration Script

Given: Generated example outputs in output/example/fortran/
When: Documentation is built for GitHub Pages deployment
Then: Example images must be accessible in the generated documentation

This script ensures example outputs are properly integrated into GitHub Pages
deployment by copying files to the documentation media directory and updating
documentation links to reference the GitHub Pages URL structure.
"""

import os
import shutil
import sys
from pathlib import Path
from typing import List, Dict, Set


def copy_example_outputs_to_media():
    """
    Given: Generated example outputs in output/example/fortran/
    When: Copying to documentation media directory
    Then: All PNG, PDF, and video files must be accessible for GitHub Pages
    """
    output_dir = Path("output/example/fortran")
    media_dir = Path("doc/media/examples")
    
    # Ensure media directory exists
    media_dir.mkdir(parents=True, exist_ok=True)
    
    copied_files = []
    
    # Copy all example outputs
    for file_pattern in ["*.png", "*.pdf", "*.mp4", "*.glb", "*.gltf"]:
        for file_path in output_dir.rglob(file_pattern):
            dest_path = media_dir / file_path.name
            shutil.copy2(file_path, dest_path)
            copied_files.append(file_path.name)
            print(f"Copied: {file_path.name}")
    
    print(f"Total files copied: {len(copied_files)}")
    return copied_files


def verify_example_documentation_links():
    """
    Given: Documentation markdown files in doc/example/
    When: Checking for image references
    Then: All example documentation must have correct media links
    """
    doc_example_dir = Path("doc/example")
    media_prefix = "../media/examples/"
    
    files_to_check = []
    missing_links = []
    
    for md_file in doc_example_dir.glob("*.md"):
        files_to_check.append(md_file)
        
        content = md_file.read_text()
        
        # Look for expected image references based on the example name
        example_name = md_file.stem
        expected_images = [
            f"{example_name}.png",
            f"simple_plot.png" if example_name == "basic_plots" else None,
            f"multi_line.png" if example_name == "basic_plots" else None,
        ]
        
        for img in expected_images:
            if img and f"{media_prefix}{img}" not in content:
                missing_links.append(f"{md_file.name}: missing {img}")
    
    print(f"Checked {len(files_to_check)} documentation files")
    if missing_links:
        print("Missing image links:")
        for link in missing_links:
            print(f"  - {link}")
        return False
    else:
        print("All expected image links found")
        return True


def update_ford_media_references():
    """
    Given: FORD documentation generator configuration
    When: Processing media files for GitHub Pages
    Then: Media directory must be properly configured for GitHub Pages deployment
    """
    build_media_dir = Path("build/doc/media/examples")
    doc_media_dir = Path("doc/media/examples")
    
    # Ensure build media directory exists
    build_media_dir.mkdir(parents=True, exist_ok=True)
    
    # Copy media files to build directory
    if doc_media_dir.exists():
        for media_file in doc_media_dir.iterdir():
            if media_file.is_file():
                dest_file = build_media_dir / media_file.name
                shutil.copy2(media_file, dest_file)
                print(f"Media for FORD: {media_file.name}")
    
    print("FORD media directory updated")


def verify_github_pages_structure():
    """
    Given: Built documentation in build/doc/
    When: Verifying GitHub Pages deployment structure
    Then: All example images must be accessible at expected URLs
    """
    build_dir = Path("build/doc")
    media_dir = build_dir / "media" / "examples"
    
    if not build_dir.exists():
        print("ERROR: Documentation build directory missing - run make doc first")
        return False
    
    if not media_dir.exists():
        print("ERROR: Media directory missing in build")
        return False
    
    # Expected files that should be accessible on GitHub Pages
    expected_files = [
        "simple_plot.png",
        "multi_line.png", 
        "line_styles.png",
        "all_marker_types.png",
        "contour_gaussian.png",
        "gaussian_default.png",
        "pcolormesh_basic.png",
        "streamplot_demo.png",
        "basic_legend.png",
        "unicode_demo.png"
    ]
    
    missing_files = []
    for expected_file in expected_files:
        file_path = media_dir / expected_file
        if not file_path.exists():
            missing_files.append(expected_file)
    
    if missing_files:
        print(f"ERROR: Missing files for GitHub Pages deployment:")
        for missing in missing_files:
            print(f"  - {missing}")
        return False
    else:
        print(f"GitHub Pages structure verified: {len(expected_files)} files ready")
        return True


def main():
    """
    Main integration workflow for GitHub Pages deployment.
    
    This script implements the four-phase solution:
    1. Documentation Integration: Copy example outputs to media directory
    2. FORD Configuration: Update media handling for documentation generator
    3. Content Integration: Verify documentation links to example outputs
    4. GitHub Pages Validation: Ensure deployment structure is correct
    """
    print("=== GitHub Pages Documentation Integration ===")
    print("Integrating example outputs for GitHub Pages deployment")
    print()
    
    success = True
    
    # Phase 1: Documentation Integration
    print("Phase 1: Copying example outputs to media directory...")
    try:
        copied_files = copy_example_outputs_to_media()
        print(f"✓ Phase 1 complete: {len(copied_files)} files copied")
    except Exception as e:
        print(f"✗ Phase 1 failed: {e}")
        success = False
    
    print()
    
    # Phase 2: FORD Configuration  
    print("Phase 2: Updating FORD media references...")
    try:
        update_ford_media_references()
        print("✓ Phase 2 complete: FORD media updated")
    except Exception as e:
        print(f"✗ Phase 2 failed: {e}")
        success = False
    
    print()
    
    # Phase 3: Content Integration Verification
    print("Phase 3: Verifying documentation links...")
    try:
        links_ok = verify_example_documentation_links()
        if links_ok:
            print("✓ Phase 3 complete: Documentation links verified")
        else:
            print("✗ Phase 3 failed: Missing documentation links")
            success = False
    except Exception as e:
        print(f"✗ Phase 3 failed: {e}")
        success = False
    
    print()
    
    # Phase 4: GitHub Pages Structure Validation
    print("Phase 4: Verifying GitHub Pages deployment structure...")
    try:
        structure_ok = verify_github_pages_structure()
        if structure_ok:
            print("✓ Phase 4 complete: GitHub Pages structure ready")
        else:
            print("✗ Phase 4 failed: GitHub Pages structure incomplete")
            success = False
    except Exception as e:
        print(f"✗ Phase 4 failed: {e}")
        success = False
    
    print()
    print("=== Integration Summary ===")
    if success:
        print("✓ GitHub Pages documentation integration SUCCESSFUL")
        print("Example outputs are ready for GitHub Pages deployment")
        sys.exit(0)
    else:
        print("✗ GitHub Pages documentation integration FAILED")
        print("Some phases failed - check errors above")
        sys.exit(1)


if __name__ == "__main__":
    main()