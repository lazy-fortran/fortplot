# Development Backlog

## TODO (Ordered by Priority)

### CURRENT SPRINT - Forensic Analysis & Restoration Framework
- [ ] #248: PNG antialiasing quality degradation since 690b9834
- [ ] #249: PDF coordinate system and scaling fundamental issues
- [ ] #250: PNG line styles and markers rendering pipeline degradation
- [ ] #251: Legend rendering system not visible

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
- [x] #252: Create comprehensive rendering comparison framework (branch: feature-252)

## DONE (Completed)
- [x] #239: Complete PDF grid functionality or remove stub
- [x] #238: Make PDF tick count configurable instead of hardcoded
- [x] #237: Add division by zero protection in PDF coordinate transformation
- [x] #232: PNG regression - axes/text blue, antialiasing and line styles broken
- [x] #231: PDF scaling regression - plots extend out of page, no axes/text visible
- [x] #227: PNG backend generates ASCII text instead of binary PNG data (branch: fix-png-backend-227)
- [x] #224: PDF files not deployed to GitHub Pages (404 errors)
- [x] #223: ASCII backend generates empty plots (only frames/borders)
- [x] #220: Fix ASCII backend generating PDF binary content instead of text output