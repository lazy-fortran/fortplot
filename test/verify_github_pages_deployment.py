#!/usr/bin/env python3
"""
Given: GitHub Pages documentation is deployed
When: User visits the deployed site
Then: Example images must be visible and accessible

This script verifies GitHub Pages deployment functionality by checking
that example outputs are accessible on the deployed documentation site.
"""

import requests
import sys
import os
from typing import List, Tuple, Dict
from urllib.parse import urljoin

# GitHub Pages deployment URL for fortplot
GITHUB_PAGES_BASE_URL = "https://lazy-fortran.github.io/fortplot/"

# Expected example images that should be visible on GitHub Pages
EXPECTED_EXAMPLE_IMAGES = [
    "page/example/basic_plots.html",
    "page/example/line_styles.html", 
    "page/example/marker_demo.html",
    "page/example/contour_demo.html",
    "page/example/colored_contours.html",
    "page/example/pcolormesh_demo.html",
    "page/example/streamplot_demo.html",
    "page/example/legend_demo.html",
    "page/example/unicode_demo.html"
]

# Example image files that should be accessible
EXAMPLE_IMAGE_FILES = [
    "media/examples/simple_plot.png",
    "media/examples/multi_line.png", 
    "media/examples/line_styles.png",
    "media/examples/all_marker_types.png",
    "media/examples/contour_gaussian.png",
    "media/examples/gaussian_default.png",
    "media/examples/pcolormesh_basic.png",
    "media/examples/streamplot_demo.png",
    "media/examples/basic_legend.png",
    "media/examples/unicode_demo.png"
]


def check_page_accessibility(url: str) -> Tuple[bool, int, str]:
    """
    Given: A GitHub Pages URL
    When: HTTP request is made
    Then: Page must be accessible with status 200
    """
    try:
        response = requests.get(url, timeout=10)
        return response.status_code == 200, response.status_code, response.text[:500]
    except requests.RequestException as e:
        return False, 0, str(e)


def check_image_references_in_page(page_content: str, page_url: str) -> List[str]:
    """
    Given: HTML page content from GitHub Pages
    When: Page is parsed for image references
    Then: All example image references must be valid
    """
    missing_images = []
    
    # Look for image references in the HTML content with proper src attribute matching
    for image_file in EXAMPLE_IMAGE_FILES:
        image_name = os.path.basename(image_file)
        # Check for img tag with src containing the image name
        # Matches patterns like: src="../media/examples/simple_plot.png"
        expected_src_pattern = f'src="../media/examples/{image_name}"'
        if expected_src_pattern not in page_content:
            missing_images.append(f"Image {image_name} not found in {page_url}")
    
    return missing_images


def verify_github_pages_deployment() -> bool:
    """
    Given: GitHub Pages deployment should be complete
    When: Verification checks are run
    Then: All example images must be accessible on deployed site
    """
    print("=== GitHub Pages Deployment Verification ===")
    print(f"Checking deployment at: {GITHUB_PAGES_BASE_URL}")
    
    all_tests_passed = True
    test_results = []
    
    # Test 1: Check main documentation pages are accessible
    print("\n1. Testing page accessibility...")
    for page_path in EXPECTED_EXAMPLE_IMAGES:
        page_url = urljoin(GITHUB_PAGES_BASE_URL, page_path)
        accessible, status_code, content = check_page_accessibility(page_url)
        
        if accessible:
            print(f"  ✓ {page_path} - Status: {status_code}")
            
            # Check for image references in the page
            missing_images = check_image_references_in_page(content, page_url)
            if missing_images:
                print(f"    ✗ Missing image references:")
                for missing in missing_images:
                    print(f"      - {missing}")
                all_tests_passed = False
        else:
            print(f"  ✗ {page_path} - Status: {status_code}")
            all_tests_passed = False
    
    # Test 2: Check example image files are directly accessible
    print("\n2. Testing image file accessibility...")
    for image_path in EXAMPLE_IMAGE_FILES:
        image_url = urljoin(GITHUB_PAGES_BASE_URL, image_path)
        accessible, status_code, _ = check_page_accessibility(image_url)
        
        if accessible:
            print(f"  ✓ {image_path} - Status: {status_code}")
        else:
            print(f"  ✗ {image_path} - Status: {status_code}")
            all_tests_passed = False
    
    # Test 3: Check main documentation index
    print("\n3. Testing main documentation index...")
    index_url = urljoin(GITHUB_PAGES_BASE_URL, "index.html")
    accessible, status_code, content = check_page_accessibility(index_url)
    
    if accessible:
        print(f"  ✓ index.html - Status: {status_code}")
    else:
        print(f"  ✗ index.html - Status: {status_code}")
        all_tests_passed = False
    
    print(f"\n=== Deployment Verification Summary ===")
    if all_tests_passed:
        print("✓ All GitHub Pages deployment tests PASSED")
        print("Example images are visible on deployed documentation")
        return True
    else:
        print("✗ GitHub Pages deployment tests FAILED")
        print("Example images are NOT visible on deployed documentation")
        print("This confirms the regression reported in Issue #117")
        return False


def main():
    """
    Main entry point for GitHub Pages deployment verification.
    
    This script MUST FAIL until GitHub Pages deployment is fixed,
    confirming the regression described in Issue #117.
    """
    print("GitHub Pages Deployment Verification Script")
    print("This script verifies that example outputs are visible on GitHub Pages")
    print("https://lazy-fortran.github.io/fortplot/")
    print()
    
    success = verify_github_pages_deployment()
    
    if success:
        print("\nGitHub Pages deployment is working correctly!")
        sys.exit(0)
    else:
        print("\nGitHub Pages deployment verification FAILED!")
        print("This confirms Issue #117: plot outputs not visible on GitHub Pages")
        sys.exit(1)


if __name__ == "__main__":
    main()