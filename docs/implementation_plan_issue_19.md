# Implementation Plan for Issue #19: Add Comprehensive Docstrings to Public Interface

## Overview
Add comprehensive Fortran docstrings to all public procedures in `src/fortplot.f90` to improve API documentation and usability.

## Documentation Standards

### Fortran Docstring Format
Based on existing patterns in the codebase and FORD documentation standards:

```fortran
subroutine procedure_name(arg1, arg2, arg3)
    !! Brief description of the procedure
    !!
    !! Detailed description explaining the purpose, behavior,
    !! and any important implementation details.
    !!
    !! Arguments:
    !!   arg1: Description of first argument [units/range]
    !!   arg2: Description of second argument (optional)
    !!   arg3: Description with valid values ('option1', 'option2')
    !!
    !! Examples:
    !!   ! Basic usage example
    !!   call procedure_name(x, y)
    !!   
    !!   ! Advanced usage with options
    !!   call procedure_name(x, y, arg3='option2')
    !!
    !! Notes:
    !!   - Important usage notes or caveats
    !!   - Performance considerations
    !!
    !! See Also:
    !!   related_procedure, other_procedure
```

### Documentation Components Required

1. **Brief Description** (mandatory)
   - One-line summary of the procedure's purpose
   - Should be concise and action-oriented

2. **Detailed Description** (for complex procedures)
   - Extended explanation of functionality
   - Use cases and context

3. **Arguments Section** (mandatory)
   - Document ALL arguments including optionals
   - Include data types, units, valid ranges
   - Specify default values for optional arguments
   - List valid options for string arguments

4. **Examples Section** (for key procedures)
   - Show basic usage pattern
   - Include advanced usage when applicable
   - Use realistic variable names

5. **Notes Section** (when needed)
   - Performance considerations
   - Backend-specific behavior
   - Important limitations or caveats

6. **See Also Section** (for related procedures)
   - Cross-reference related functionality
   - Help users discover similar features

## Procedures Requiring Documentation

### Category 1: Core Plotting Functions (HIGH PRIORITY)
Complete documentation with examples needed:

1. **plot** - Has basic docs, needs examples and argument details
2. **scatter** - Has good docs with examples (reference standard)
3. **contour** - Has basic docs, needs enhancement
4. **contour_filled** - Has basic docs, needs examples
5. **pcolormesh** - Minimal docs, needs full documentation
6. **streamplot** - Minimal docs, needs full documentation
7. **errorbar** - Has basic docs, needs examples
8. **boxplot** - Minimal docs, needs full documentation
9. **bar/barh** - Minimal docs, needs full documentation
10. **hist/histogram** - Minimal docs, needs full documentation

### Category 2: Figure Management (MEDIUM PRIORITY)
Need complete documentation:

1. **figure** - Has basic docs, needs argument details
2. **show/show_viewer** - Minimal docs, needs full documentation
3. **savefig** - Has basic docs, needs format details

### Category 3: Axis and Label Functions (MEDIUM PRIORITY)
Need enhancement:

1. **xlabel/ylabel** - Minimal docs, needs LaTeX support notes
2. **title** - Minimal docs, needs formatting details
3. **legend** - Minimal docs, needs location options
4. **xlim/ylim** - No docs, needs full documentation
5. **set_xscale/set_yscale** - No docs, needs scale options

### Category 4: Add Functions (LOW PRIORITY)
Mirror documentation from primary functions:

1. **add_plot** - Mirror from plot
2. **add_contour** - Mirror from contour
3. **add_contour_filled** - Mirror from contour_filled
4. **add_pcolormesh** - Mirror from pcolormesh
5. **add_errorbar** - Mirror from errorbar
6. **add_3d_plot** - Needs full documentation
7. **add_surface** - Needs full documentation
8. **add_scatter** - Mirror from scatter

### Category 5: Utility Functions (LOW PRIORITY)

1. **set_line_width** - No docs, needs documentation
2. **set_ydata** - No docs, needs documentation
3. **ensure_global_figure_initialized** - Has minimal docs

## Implementation Strategy

### Phase 1: Document Core Plotting Functions
Priority: HIGH
- Focus on most-used functions first
- Use scatter documentation as template
- Include practical examples
- Document all optional arguments

### Phase 2: Document Figure Management
Priority: MEDIUM  
- Document backend auto-detection in savefig
- Explain blocking parameter behavior
- Document viewer functionality

### Phase 3: Document Axis/Label Functions
Priority: MEDIUM
- Document LaTeX support where applicable
- List all valid location options for legend
- Document scale types (linear, log, symlog)

### Phase 4: Document Add Functions
Priority: LOW
- Reference main function documentation
- Note differences if any
- Keep consistent with primary functions

### Phase 5: Document Utility Functions
Priority: LOW
- Focus on user-facing utilities
- Document internal helpers minimally

## Documentation Template Examples

### Example 1: Enhanced plot documentation
```fortran
subroutine plot(x, y, label, linestyle)
    !! Add a line plot to the global figure (pyplot-fortran compatible)
    !!
    !! Creates a 2D line plot with optional styling. Supports both
    !! matplotlib-style format strings and explicit style parameters.
    !!
    !! Arguments:
    !!   x: X-axis data array
    !!   y: Y-axis data array (same size as x)
    !!   label: Legend label for this plot (optional)
    !!   linestyle: Format string for line and marker style (optional)
    !!             Supports matplotlib format: '[color][marker][line]'
    !!             Examples: 'r-' (red solid), 'b--o' (blue dashed with circles),
    !!                      'g:' (green dotted), 'ko' (black circles only)
    !!
    !! Examples:
    !!   ! Basic line plot
    !!   call plot(x, y)
    !!   
    !!   ! Plot with label for legend
    !!   call plot(x, y, label='Temperature')
    !!   
    !!   ! Red dashed line with square markers
    !!   call plot(x, y, label='Data', linestyle='r--s')
    !!
    !! Notes:
    !!   - Automatically initializes global figure if needed
    !!   - Use savefig() to save or show() to display
    !!
    !! See Also:
    !!   add_plot, scatter, errorbar
```

### Example 2: Enhanced savefig documentation
```fortran
subroutine savefig(filename, blocking)
    !! Save the global figure to file (backend auto-detected from extension)
    !!
    !! Automatically selects the appropriate backend based on file extension.
    !! Supports PNG, PDF, ASCII text, and GLTF 3D formats.
    !!
    !! Arguments:
    !!   filename: Output filename with extension
    !!            '.png' - Raster image (default)
    !!            '.pdf' - Vector graphics
    !!            '.txt' - ASCII art representation  
    !!            '.gltf'/'.glb' - 3D model format
    !!   blocking: Wait for user input after save (optional, default: false)
    !!            Useful for keeping plot windows open
    !!
    !! Examples:
    !!   ! Save as PNG image
    !!   call savefig('plot.png')
    !!   
    !!   ! Save as PDF and wait for user
    !!   call savefig('figure.pdf', blocking=.true.)
    !!   
    !!   ! Save as ASCII art
    !!   call savefig('chart.txt')
    !!
    !! Notes:
    !!   - PNG backend requires zlib support
    !!   - PDF backend generates vector graphics
    !!   - ASCII backend works in terminal environments
    !!   - File is overwritten if it exists
```

## Risk Assessment

### Technical Risks

1. **Documentation Drift** (MEDIUM)
   - Risk: Documentation becomes outdated as code evolves
   - Mitigation: Enforce documentation updates in PR reviews
   - Mitigation: Add documentation tests to verify examples compile

2. **Over-Documentation** (LOW)  
   - Risk: Too verbose documentation reduces readability
   - Mitigation: Follow KISS principle - be concise but complete
   - Mitigation: Use examples instead of lengthy descriptions

3. **Inconsistent Style** (LOW)
   - Risk: Different documentation styles across procedures
   - Mitigation: Use template and review for consistency
   - Mitigation: Reference scatter documentation as gold standard

### Schedule Risks

1. **Scope Creep** (MEDIUM)
   - Risk: Adding new features while documenting
   - Mitigation: Document existing functionality only
   - Mitigation: File separate issues for improvements

2. **Time Estimation** (LOW)
   - Risk: Underestimating documentation effort
   - Mitigation: Start with high-priority functions
   - Mitigation: Incremental documentation acceptable

## Success Criteria

1. All public procedures in fortplot.f90 have at minimum:
   - Brief description
   - Arguments section with all parameters documented

2. Core plotting functions have:
   - Detailed descriptions
   - Multiple examples
   - Notes on backend-specific behavior

3. Documentation follows consistent format across all procedures

4. Examples are tested and working

5. Cross-references help users discover related functionality

## Testing Strategy

1. **Compilation Testing**
   - Ensure all example code in docstrings compiles
   - Create test program that exercises all examples

2. **Documentation Generation**
   - Verify FORD can process all docstrings
   - Check generated HTML documentation

3. **User Testing**
   - Have new users try to use API with only docstring help
   - Gather feedback on clarity and completeness

## Estimated Timeline

- Phase 1 (Core Functions): 2 hours
- Phase 2 (Figure Management): 1 hour  
- Phase 3 (Axis/Labels): 1 hour
- Phase 4 (Add Functions): 1 hour
- Phase 5 (Utilities): 30 minutes
- Testing and Review: 1.5 hours

**Total Estimate: 7 hours**

## References

- Existing scatter documentation (good example)
- FORD documentation standards
- Matplotlib API documentation patterns
- pyplot-fortran interface design