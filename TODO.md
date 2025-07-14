# TODO List

## Compiler Warnings Cleanup Plan

### Phase 1: Critical Issues (High Priority)
1. **Fix aliasing issue in fortplot_pdf.f90**
   - Line 518: Same variable used for input and output parameters
   - This could cause undefined behavior

### Phase 2: Backend-Specific Placeholder Functions
2. **ASCII backend color functions**
   - Add comment explaining these are intentionally no-op
   - Consider using preprocessor directives to suppress warnings
   
3. **PDF backend color functions**
   - Implement actual color support or document as placeholder

### Phase 3: Dead Code Removal
4. **Remove unused functions**
   - `clean_scientific_notation` in fortplot_axes.f90
   - `remove_trailing_zeros` in fortplot_axes.f90
   - `draw_single_tick` in fortplot_axes.f90
   - `render_simple_character_block` in fortplot_text.f90
   - `render_character_bitmap` in fortplot_text.f90
   - `format_tick_value_consistent` in fortplot_ticks.f90

5. **Remove unused variables**
   - Clean up loop variables and temporary variables

### Phase 4: Interface Consistency
6. **Document intentionally unused parameters**
   - Add comments explaining why parameters are required for interface consistency
   - Consider using Fortran's `associate` construct or compiler directives

### Phase 5: Symlog Implementation
7. **Complete symlog tick functions**
   - `add_positive_symlog_ticks`
   - `add_linear_symlog_ticks`
   - `add_negative_symlog_ticks`
   - These appear to be incomplete implementations

## Implementation Strategy

1. Create tests for each module before making changes
2. Fix one module at a time
3. Run full test suite after each module
4. Use compiler pragma where appropriate to suppress intentional warnings
5. Document all intentional unused parameters

## Compiler Pragma Options

For intentionally unused parameters, consider:
```fortran
!GCC$ ATTRIBUTES UNUSED :: parameter_name
```

Or restructure interfaces to use optional parameters where appropriate.