# Development Backlog

## CURRENT SPRINT (Foundation Quality Enforcement)

## SPRINT_BACKLOG (Foundation Quality Enforcement Sprint)

### EPIC: CRITICAL Quality Gate Enforcement (TOP PRIORITY)
- [ ] #443: Critical: fortplot_figure_core.f90 exceeds 1000-line hard limit (1258 lines)
- [ ] #446: Critical: 16 disabled test files indicate systematic functionality breakdown
- [ ] #444: defect: 8 stub implementations with error_stop in fortplot_matplotlib.f90
- [ ] #447: defect: Memory safety issues identified in 7 test files
- [ ] #445: defect: 12 TODO comments indicate incomplete critical implementations

### EPIC: Edge Case and Stability Foundation
- [ ] #432: Critical: Zero-size arrays and single points produce blank/invisible plots
- [ ] #433: Critical: Numeric limits cause corrupted/blank plot output
- [ ] #435: Critical: Machine precision coordinate handling produces blank outputs
- [ ] #431: Bug: Inconsistent error reporting - reports success after failure

### EPIC: Core API Implementation Completion
- [ ] #417: defect: stub implementations returning error stop
- [ ] #422: defect: empty/no-op subroutines in multiple modules
- [ ] #421: defect: file I/O operations without proper validation
- [ ] #420: defect: potential memory leak - deallocate without error checking
- [ ] #429: Bug: Animation save implementation not initialized

### EPIC: Visual Output Quality Restoration
- [ ] #311: Simple Sine Wave and Gaussian Default plots shows just axes in examples but no content
- [ ] #312: Legend Demo plots dont show up on github pages
- [ ] #414: PDF plots completely broken
- [ ] #413: title in png plots too far right
- [ ] #408: ASCII plots don't show full curves in basic plots
- [ ] #409: Limited height of ASCII plots in contour demo
- [ ] #410: Format string markers (dashed, circles, x) completely non-functional
- [ ] #411: Axes and labels at wrong positions in scale examples
- [ ] #412: Streamplot demo too colorful

### EPIC: Codebase Health and Technical Debt Cleanup
- [ ] #416: defect: commented-out code in fortplot_matplotlib.f90
- [ ] #418: defect: TODO comments indicate incomplete implementations
- [ ] #419: defect: magic numbers without named constants
- [ ] #423: defect: 18 disabled test files indicate broken functionality
- [ ] #424: defect: test file duplication and proliferation
- [ ] #425: defect: unused module fortplot_forensic_comparison
- [ ] #426: defect: unused test security module
- [ ] #427: defect: performance issues - large allocations and inefficient patterns
- [ ] #428: defect: PLAY workflow audit summary - 13 defect categories found

### EPIC: Documentation Quality Assurance
- [ ] #415: Documentation Defects: Broken references, duplicated content, empty READMEs
- [ ] #448: defect: Documentation consolidation for Foundation Quality Enforcement Sprint

## DOING (Current Work)

## PRODUCT_BACKLOG (Long-term Features)

- [ ] Advanced Plotting Features Enhancement
- [ ] Cross-Backend Integration Framework
- [ ] Automated Visual Regression Testing
- [ ] Performance Optimization Framework
- [ ] Advanced Animation Pipeline
- [ ] Scientific Data Visualization Extensions

## DONE
- [x] Repository Management and Branch Protection Recovery
- [x] PLAY Workflow Defect Discovery System
- [x] Critical Foundation Recovery (Partial - 40% achieved)
- [x] Core Segfault Resolution and State Management

