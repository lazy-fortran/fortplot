# Development Backlog

## CURRENT SPRINT (Critical PNG/PDF Rendering Issues)

**🚨 CRITICAL: User-visible PNG/PDF rendering completely broken**
- [x] #338: Fix - No axes visible and plots strangely stretched and shifted in PDF (COMPLETED)
- [x] #337: Fix - Title too far right in PNGs - check matplotlib placement (COMPLETED)
- [x] #335: Fix - Axes wrong and no labels visible on scale_examples.html (COMPLETED)
- [x] #334: Fix - No output visible on pcolormesh_demo.html (COMPLETED - PR #351)
- [x] #333: Fix - Circles seem not centered with line plot in marker_demo.html (COMPLETED)
- [x] #332: Fix - Dashed and dash-dotted look funny on line_styles.html (COMPLETED)
- [x] #330: Fix - Old plot not cleared in second figure (figure() call) in contour_demo.html (COMPLETED)
**Critical Infrastructure Issues (High Priority)**

**User-Facing Issues (Medium Priority)**

**Infrastructure & Documentation Issues (Lower Priority)**
- [ ] #344: Refactor - add format threshold constants in axes module
- [ ] #350: Refactor - improve documentation comments in raster drawing module
- [ ] #357: Docs - standardize colormap documentation across examples
- [ ] #358: Refactor - consolidate ASCII output formatting in example docs
- [ ] #360: Refactor - split fortplot_raster.f90 to comply with file size limits

## DOING (Current Work)
- [ ] #344: Refactor - add format threshold constants in axes module

## BLOCKED (Infrastructure Issues)

## FUTURE SPRINTS - Systematic Restoration

**Sprint 2: PNG Rendering Quality Restoration**
- PNG antialiasing system restoration
- Line styles and markers pipeline fixes
- Color accuracy improvements

**Sprint 3: PDF Coordinate System Overhaul**  
- Coordinate transformation restoration
- Scaling and positioning fixes
- Page layout system improvements

**Sprint 4: Cross-Backend Integration**
- Legend system restoration
- Consistent rendering across backends
- Text and label positioning fixes

**Sprint 5: Quality Assurance & Testing**
- Visual regression testing framework
- Automated quality metrics
- Performance impact validation

## DONE (Completed)
- [x] #343: Refactor - extract label positioning constants - FIXED: Label positioning constants extracted to shared module with descriptive names (XLABEL_VERTICAL_OFFSET, YLABEL_HORIZONTAL_OFFSET, TICK_MARK_LENGTH) replacing magic numbers for improved maintainability and consistency (PR #370 merged)
- [x] #342: Refactor - complete symlog tick generation implementation - FIXED: Implemented symlog tick generation with proper exponential positioning and overlap handling in dedicated subroutines (PR #369 merged)
- [x] #324: Refactor - define epsilon constant for numerical comparisons - FIXED: Extracted epsilon constants to shared fortplot_constants module, eliminating code duplication across multiple modules and creating centralized location for mathematical constants (PR #368 merged)
- [x] #323: Test - add edge case tests for PDF heatmap color validation - FIXED: Added comprehensive edge case tests validating constant fields, NaN/infinity values, extreme ranges, and epsilon precision in PDF heatmap rendering (PR #367 merged)
- [x] #347: Fix - Remove funny header from https://lazy-fortran.github.io/fortplot/ - FIXED: Removed duplicate 'project: fortplotlib' line from doc.md that was creating redundant header on GitHub Pages, documented FORD configuration format to prevent recurrence (PR #366 merged)
- [x] #327: Fix - MP4 link not showing on animation.html - FIXED: Corrected incorrect reference from animation.mp4 to animated_plot.mp4 in documentation examples (PR #365 merged)
- [x] #328: Fix - One legend entry too much in basic_plots.html second plot - FIXED: Empty labels no longer create legend entries, resolving extra legend entry issue (PR #364 merged)
- [x] #361: Fix - Windows CI: test_legend_comprehensive and test_streamplot runtime failures - FIXED: Added Windows-compatible file system delays and enhanced error handling for CI stability (PR #363 merged)
- [x] #336: Fix - Streamplot and stateful streamplot example redundant - FIXED: Removed duplicate stateful_streamplot example, consolidated to single streamplot_demo implementation (PR #362 merged)
- [x] #355: Fix - First plot is empty (likely from figure CLEAR logic regression) - FIXED: Implemented proper backend initialization to prevent first plot rendering empty, major refactoring split 1776-line file into 3 modules (PR #359 merged)
- [x] #329: Fix - No output visible on colored_contours.html - FIXED: Added comprehensive visual outputs and documentation for colored contours example on GitHub Pages showcase with 5 different colormap demonstrations (crest, plasma, jet, coolwarm, inferno) and complete ASCII output sections (PR #356 merged)
- [x] #330: Fix - Old plot not cleared in second figure (figure() call) in contour_demo.html - FIXED: Implemented unconditional plot_count reset during figure initialization to ensure proper plot isolation between figure() calls and prevent overlapping contours (PR #354 merged)
- [x] #331: Fix - No legend visible in format_string_demo.html - FIXED: Restored legend visibility by removing automatic reset of show_legend during figure re-initialization, allowing user legend settings to persist across plot operations (PR #353 merged)
- [x] #334: Fix - No output visible on pcolormesh_demo.html - FIXED: Added comprehensive documentation content with PNG images, ASCII outputs, and PDF download links to resolve GitHub Pages visibility issue (PR #351 merged)
- [x] #333: Fix - Circles seem not centered with line plot in marker_demo.html - FIXED: Resolved marker centering coordinate consistency through sub-pixel adjustment (-0.5, -0.5) to align markers with line drawing pixel centers (PR #346 merged)
- [x] #335: Fix - Axes wrong and no labels visible on scale_examples.html - FIXED: Resolved axis label rendering issues with proper tick marks, axis labels, and tick labels in ASCII and raster backends (PR #341 merged)
- [x] #337: Fix - Title too far right in PNGs - check matplotlib placement - FIXED: Centered PNG titles over plot area instead of data coordinates through corrected matplotlib placement (PR #340 merged)
- [x] #338: Fix - No axes visible and plots strangely stretched and shifted in PDF - FIXED: Restored PDF axes visibility and prevented plot stretching/shifting through corrected coordinate transformation and scaling (PR #339 merged)
- [x] #321: Refactor - apply consistent validation pattern to other PDF write functions - FIXED: Extended robust validation pattern to pdf_write_move, pdf_write_line, and pdf_write_line_width with comprehensive test coverage and zero performance impact (PR #326 merged)
- [x] #320: Feature - add debug logging for invalid color value corrections - FIXED: Implemented debug logging for RGB color corrections in PDF output with zero performance impact and comprehensive test coverage (PR #325 merged)
- [x] #319: Refactor - investigate source of invalid RGB values in contour color mapping - FIXED: Replaced direct color writing with validated pdf_write_color method, added proper edge case handling for division by zero scenarios (PR #322 merged)
- [x] #317: Fix GitHub Pages deployment failure - colored_contours PDF runtime crash in pdf_write_color - FIXED: Added robust RGB validation to prevent PDF color crashes, restored visual showcase (PR #318 merged)
- [x] #312: Legend Demo plots dont show up on github pages - FIXED: GitHub Pages examples not generating due to auto-examples=false in fpm.toml, enabled automatic example discovery (PR #316 merged)  
- [x] #311: Simple Sine Wave and Gaussian Default plots shows just axes in examples but no content - FIXED: PNG data rendering regression caused by figure initialization resetting plot_count (PR #315 merged)
- [x] #272: fix: rescue BACKLOG.md commits from main branch protection - RESCUED: Historical PLAY workflow planning commits (c610cce, ad82dbc) documenting issues #262-271 (PR #314 merged)
- [x] #293: fix: rescue BACKLOG.md update commit from main branch protection - FIXED: comprehensive rescue procedure with prevention safeguards (PR #313 merged)
- [x] #285: refactor: remove hist() and bar() stub implementations - FIXED: replaced broken stubs with clear error messages and TODO comments (PR #310 merged)
- [x] #271: Consolidate duplicate documentation content - remove redundant sections - FIXED: consolidated duplicate docs, updated configurations (PR #309 merged)
- [x] #299: Regression: https://lazy-fortran.github.io/fortplot/ examples broken - FIXED: replaced PDF content with ASCII visualizations (PR #308 merged)
- [x] #297: Windows test failures on PR #290 - precision and directory issues - FIXED: Windows-safe tolerances applied to failing tests (PR #307 merged)
- [x] #300: fix: comprehensive Windows CI environment compatibility - FIXED: Windows CI tests pass with environment compatibility fixes (PR #306 merged)
- [x] #292: regression: legend placement does not work - FIXED: legend positioning uses data coordinates (PR #305 merged)
- [x] #278: Line styles dont work - all styling broken since 690b9834 - FIXED: partial fix for line styles in PNG backend (PR #303 merged)
- [x] #280: regression: contours dont work - FIXED: contour functionality restored with coordinate constants (PR #304 merged)
- [x] #276: Markers dont work - PNG examples completely broken - FIXED: marker rendering restored in PNG backend (PR #302 merged)
- [x] #296: regression: PDF plot strangely stretched - FIXED: PDF coordinate transformation preserves aspect ratios (PR #298 merged)
- [x] #291: regression: y text not rotated in PNGs - FIXED: raster_render_ylabel implementation with bitmap rotation (PR #301 merged)
- [x] #270: Broken example output documentation - fix PDF binary content display (PR #290 merged)
- [x] #277: PDF page size still completely broken - FIXED: proper A4 dimensions and coordinate system (PR #294 merged)
- [x] #269: Incomplete scale system implementation - finish logarithmic scales (branch: fix-incomplete-scale-system-269)
- [x] #268: PNG dimension overflow errors - fix 64000x48000 dimension calculations (branch: fix-png-dimension-overflow-268)
- [x] #267: Dead code cleanup - remove unused imports and unreachable code paths (branch: cleanup-dead-code-267)
- [x] #266: Missing advertised features - implement or remove 5+ unimplemented functions (branch: fix-missing-advertised-features-266)
- [x] #265: Documentation line count inconsistency - fix 1,119 vs 233 line discrepancy (branch: fix-documentation-inconsistency-265)
- [x] #264: PNG backend regression - fix fixed-size file generation issue (branch: fix-png-backend-regression-264)
- [x] #263: Test suite infrastructure broken - restore automated testing capability (branch: fix-test-infrastructure-263)
- [x] #262: Python interface completely fake/mock - implement actual F2PY bindings (branch: fix-python-interface-262)
- [x] #236: Add division by zero protection in PDF coordinate transformation (branch: fix-division-by-zero-236)
- [x] #251: Legend rendering system not visible (branch: fix-legend-visibility-251)
- [x] #250: PNG line styles and markers rendering pipeline degradation (branch: fix-png-line-styles-250)
- [x] #249: PDF coordinate system and scaling fundamental issues (branch: fix-pdf-coordinate-system-249)
- [x] #248: PNG antialiasing quality degradation since 690b9834 (branch: fix-png-antialiasing-248)
- [x] #252: Create comprehensive rendering comparison framework (branch: enhance-rendering-comparison-252)
- [x] #254: Emergency rescue commits from main branch protection (branch: fix/rescue-main-commits)
- [x] #239: Complete PDF grid functionality or remove stub
- [x] #238: Make PDF tick count configurable instead of hardcoded
- [x] #237: Add division by zero protection in PDF coordinate transformation
- [x] #232: PNG regression - axes/text blue, antialiasing and line styles broken
- [x] #231: PDF scaling regression - plots extend out of page, no axes/text visible
- [x] #227: PNG backend generates ASCII text instead of binary PNG data (branch: fix-png-backend-227)
- [x] #224: PDF files not deployed to GitHub Pages (404 errors)
- [x] #223: ASCII backend generates empty plots (only frames/borders)
- [x] #220: Fix ASCII backend generating PDF binary content instead of text output