# Development Backlog

## CURRENT SPRINT (CRITICAL DEFECT RESOLUTION - 3 Issues)

## SPRINT_BACKLOG (CRITICAL BLOCKERS - Development Infrastructure Recovery)

**CRISIS MODE**: Test infrastructure completely broken. Security vulnerabilities active. Development blocked.

### EPIC: DEVELOPMENT UNBLOCKING
- [ ] #573: CRITICAL: Full test suite hangs indefinitely - systematic test infrastructure failure
- [ ] #574: CRITICAL: CI test targets completely broken - all return 'No tests to run'
- [ ] #576: CRITICAL: Command injection vulnerability STILL EXISTS in fortplot_pipe_timeout.c

## DOING (Current Work)

*Infrastructure completely broken - focus on unblocking development*

## PRODUCT_BACKLOG (CONSOLIDATED DEFECT REPOSITORY)

**CRITICAL DEFECTS** (Next Sprint Priority):
- [ ] #577: CRITICAL: PNG backend has 100x dimension calculation error
- [ ] #575: CRITICAL: Python bridge executable not in PATH - Python integration broken
- [ ] #569: FUNCTIONALITY DESTROYED: ImageMagick disabled breaking visual processing capabilities
- [ ] #578: ARCHITECTURAL DISASTER: 119 files in src/ directory violates limits

**PROCESS DEFECTS**:
- [ ] #562: PROCESS VIOLATION: PR #560 BACKLOG.md status inconsistent with completion claims

**TECHNICAL DEFECTS** (Deferred Until Trust Restored):
- [ ] #548: defect: Duplicate directory creation functions across modules
- [ ] #553: DEFECT: GitHub Pages visual showcase system degraded by missing README files
- [ ] #499: defect: binary executables and unreferenced files polluting repository (limited cleanup only)

**PREVIOUS ARCHITECTURAL DEFECTS** (Deferred due to trust restoration focus):
- [ ] #507: defect: unused fortplot_forensic_comparison module is dead code with security risks
- [ ] #504: defect: potential memory leaks and unsafe memory management patterns  
- [ ] #500: defect: 22 disabled test files indicate systematic test infrastructure failure
- [ ] #512: arch: inconsistent error handling patterns across backend modules
- [ ] #514: arch: module dependency cycles in figure subsystem
- [ ] #515: arch: inconsistent coordinate system handling across backends
- [ ] #513: arch: performance monitoring infrastructure gaps
- [ ] #509: Bug: add_3d_plot method documented but not implemented
- [ ] #510: Bug: errorbar documented as figure method but only available as global function
- [ ] #503: defect: build system artifacts and temporary files polluting repository
- [ ] #505: defect: 11 stub implementations indicate incomplete functionality
- [ ] #508: CRITICAL: Comprehensive PLAY audit findings consolidation - team documentation failures
- [ ] #415: Documentation Defects: Broken references, duplicated content, empty READMEs

**Long-term Features (when trust restored)**:
- [ ] Visual Output Quality Enhancement System
- [ ] Advanced Animation Pipeline  
- [ ] Scientific Data Visualization Extensions
- [ ] Cross-Backend Integration Framework
- [ ] Automated Visual Regression Testing
- [ ] Enhanced Feature Implementation (boxplot improvements)

## DONE
- [x] Infrastructure Restoration Sprint (2/3 SUCCESS - #568 FPM operations and #570 temp directory creation fixed, #569 ImageMagick remains broken)
- [x] Trust Restoration Sprint (2/2 COMPLETE SUCCESS - Issues #506 and #511 both resolved with security implementation and module splitting)
- [x] Crisis Recovery Sprint (1/1 SINGLE TASK SUCCESS - Documentation accuracy restored, evidence-based reporting implemented)
- [x] Architectural Debt Resolution Sprint (90% Success - Major architectural violations resolved, quality foundation maintained)
- [x] Module Architecture Refactoring (PARTIAL SUCCESS - Most QADS limits met, but #511 remains unfixed at 979 lines)
- [x] Foundation Quality Enforcement (85% Success - Major quality gates, infrastructure, API reliability, visual output achieved)
- [x] Critical Foundation Recovery (Partial - 40% achieved)
- [x] Core Segfault Resolution and State Management
- [x] PLAY Workflow Defect Discovery System  
- [x] Repository Management and Branch Protection Recovery
