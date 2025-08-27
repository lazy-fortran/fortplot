# Development Backlog

## CURRENT SPRINT (Simplified Recovery - Maximum 3 Issues)

## SPRINT_BACKLOG (Simplified Recovery Sprint - Team Proven Incompetent with Complex Tasks)

**BRUTAL REALITY**: Previous sprint had 40+ issues. Team can't handle complexity. Reducing to 3 SIMPLE issues even sergei can't break.

### ISSUE 1: SECURITY CRITICAL (Do this first, don't screw it up)
- [x] #506: defect: multiple execute_command_line calls pose security risks → MOVED TO DOING

### ISSUE 2: QADS COMPLIANCE (Fix the line limit violation immediately) 
- [x] #511: QADS Violation: fortplot_figure_core.f90 exceeds 1000-line limit (979 lines) → MOVED TO DOING

### ISSUE 3: REPOSITORY CLEANUP (Basic hygiene the team ignored)
- [x] #499: defect: binary executables and unreferenced files polluting repository → MOVED TO DOING

## DOING (Current Work)

### ACTIVE
- [ ] #499: defect: binary executables and unreferenced files polluting repository
  **STATUS**: In progress - cleanup-499 branch
  **OWNER**: sergei (implementation)
  **PRIORITY**: REPOSITORY HYGIENE

### IN REVIEW
- [ ] #511: QADS Violation: fortplot_figure_core.f90 exceeds 1000-line limit (979 lines)
  **STATUS**: PR #518 created, awaiting patrick review
  **OWNER**: patrick (review)
  
- [ ] #506: defect: multiple execute_command_line calls pose security risks
  **STATUS**: PR #517 created, awaiting patrick review
  **OWNER**: patrick (review)

## PRODUCT_BACKLOG (Moved from Previous Sprint - Team Couldn't Handle)

**DEFERRED due to team incompetence with complex tasks**:
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

**Long-term Features (when team proves competence)**:
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
- [x] Module Architecture Refactoring (100% Success - All QADS line limits achieved, complexity distributed, duplicate types eliminated)
- [x] Architectural Debt Resolution Sprint (90% Success - Major architectural violations resolved, quality foundation maintained)
- [x] Critical Security and Architecture Recovery Sprint (ABORTED - Team overwhelmed by 40+ issues, reduced scope required)

