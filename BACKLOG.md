# Development Backlog

## TODO (Ordered by Priority)

### CURRENT SPRINT - Forensic Analysis & Restoration Framework
- [ ] #251: Legend rendering system not visible
- [ ] #236: Add division by zero protection in PDF coordinate transformation

### FUTURE SPRINTS - Systematic Restoration
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

## DOING (Current Work)
- [ ] #250: PNG line styles and markers rendering pipeline degradation

## DONE (Completed)
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