# Development Backlog

## CURRENT SPRINT (Critical Foundation Recovery)

## SPRINT_BACKLOG (Critical Foundation Recovery Sprint)

### EPIC: System Stability and Core Functionality
- [ ] #432: Critical: Zero-size arrays and single points produce blank/invisible plots
- [ ] #433: Critical: Numeric limits cause corrupted/blank plot output
- [ ] #435: Critical: Machine precision coordinate handling produces blank outputs

### EPIC: API Foundation Integrity
- [ ] #417: defect: stub implementations returning error stop
- [ ] #422: defect: empty/no-op subroutines in multiple modules
- [ ] #421: defect: file I/O operations without proper validation
- [ ] #420: defect: potential memory leak - deallocate without error checking
- [ ] #431: Bug: Inconsistent error reporting - reports success after failure

### EPIC: Rendering Pipeline Recovery
- [ ] #311: Simple Sine Wave and Gaussian Default plots shows just axes in examples but no content
- [ ] #312: Legend Demo plots dont show up on github pages
- [ ] #414: PDF plots completely broken
- [ ] #413: title in png plots too far right
- [ ] #429: Bug: Animation save implementation not initialized

### EPIC: Examples and Documentation Quality
- [ ] #408: ASCII plots don't show full curves in basic plots
- [ ] #409: Limited height of ASCII plots in contour demo
- [ ] #410: Format string markers (dashed, circles, x) completely non-functional
- [ ] #411: Axes and labels at wrong positions in scale examples
- [ ] #412: Streamplot demo too colorful
- [ ] #415: Documentation Defects: Broken references, duplicated content, empty READMEs
- [ ] #438: defect: Documentation consolidation for Critical Foundation Recovery Sprint

### EPIC: Code Quality and Technical Debt
- [ ] #416: defect: commented-out code in fortplot_matplotlib.f90
- [ ] #418: defect: TODO comments indicate incomplete implementations
- [ ] #419: defect: magic numbers without named constants
- [ ] #423: defect: 18 disabled test files indicate broken functionality
- [ ] #424: defect: test file duplication and proliferation
- [ ] #425: defect: unused module fortplot_forensic_comparison
- [ ] #426: defect: unused test security module
- [ ] #427: defect: performance issues - large allocations and inefficient patterns
- [ ] #428: defect: PLAY workflow audit summary - 13 defect categories found

## DOING (Current Work)
- [ ] #436: Critical: Single point plotting failure across all backends [EPIC: System Stability and Core Functionality]

## PRODUCT_BACKLOG (Long-term Features)

- [ ] PNG Rendering Quality Enhancement System
- [ ] PDF Coordinate System Architecture Overhaul
- [ ] Cross-Backend Integration Framework
- [ ] Automated Visual Regression Testing
- [ ] Performance Optimization Framework
- [ ] Advanced Animation Pipeline

## DONE
- [x] Repository Management and Branch Protection Recovery
- [x] PLAY Workflow Defect Discovery System
- [x] Core Rendering Pipeline Foundation
- [x] Basic Plotting Functionality Restoration

