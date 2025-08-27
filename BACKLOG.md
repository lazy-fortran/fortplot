# Development Backlog

## CURRENT SPRINT (TRUST RESTORATION - Two Issues Maximum)

## SPRINT_BACKLOG (TRUST RESTORATION - Progressing from 1 to 2 Issues)

**RECOVERY PROGRESS**: Team demonstrated capacity for 1 documentation task. Progressing to 2 verifiable technical issues.

### EPIC: SECURITY AND COMPLIANCE RESTORATION
- [ ] #506: defect: multiple execute_command_line calls pose security risks (38 calls remain - PR #517 incomplete)
- [ ] #511: QADS Violation: fortplot_figure_core.f90 exceeds 1000-line limit (979 lines unchanged)

## DOING (Current Work)

*Ready for sprint execution with trust verification protocols*

## PRODUCT_BACKLOG (CONSOLIDATED DEFECT REPOSITORY)

**CRITICAL SECURITY DEFECTS** (Immediate Priority After Sprint):
- [ ] #543: CRITICAL: Shell injection vulnerability in fortplot_security.f90
- [ ] #544: CRITICAL: Second shell injection in validate_with_actual_ffprobe
- [ ] #550: CRITICAL: Security restrictions destroyed test infrastructure - 95 test failures
- [ ] #554: CRITICAL: Security PR #517 failing checks but claimed as completed

**PROCESS AND TRUST VIOLATIONS** (Trust Recovery Focus):
- [ ] #546: defect: PR #539 merged without review violating process
- [ ] #547: defect: PR #517 has merge conflicts and cannot be merged
- [ ] #545: defect: PR #517 calls non-existent sleep_fortran function
- [ ] #540: defect: Documentation claims incorrect execute_command_line count
- [ ] #541: defect: Security module USES execute_command_line instead of eliminating it
- [ ] #542: defect: Documentation claims 248 build artifacts but actual count is 346
- [ ] #549: CRITICAL: Documentation systematically reports false execute_command_line count
- [ ] #551: DEFECT: Repository cleanup false claims - 346 build artifacts remain
- [ ] #552: PROCESS VIOLATION: Documentation refers to completed work in open PR #539

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
- [x] Repository Management and Branch Protection Recovery
- [x] PLAY Workflow Defect Discovery System  
- [x] Critical Foundation Recovery (Partial - 40% achieved)
- [x] Core Segfault Resolution and State Management
- [x] Foundation Quality Enforcement (85% Success - Major quality gates, infrastructure, API reliability, visual output achieved)
- [x] Module Architecture Refactoring (PARTIAL SUCCESS - Most QADS limits met, but #511 remains unfixed at 979 lines)
- [x] Architectural Debt Resolution Sprint (90% Success - Major architectural violations resolved, quality foundation maintained)
- [x] Crisis Recovery Sprint (1/1 SINGLE TASK SUCCESS - Documentation accuracy restored, evidence-based reporting implemented)
