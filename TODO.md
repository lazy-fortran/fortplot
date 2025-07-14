# TODO List

## ✅ Compiler Warnings Cleanup - COMPLETED!

**MAJOR SUCCESS**: Main FPM build is now completely WARNING-FREE!

### ✅ Phase 1: Critical Issues - COMPLETED
1. **✅ Fixed aliasing issue in fortplot_pdf.f90**
   - Fixed same variable used for input and output parameters
   - Eliminated potential undefined behavior

### ✅ Phase 2: Backend-Specific Placeholder Functions - COMPLETED  
2. **✅ ASCII backend color functions**
   - Added associate blocks to suppress unused parameter warnings
   - Documented as intentional no-op implementations
   
3. **✅ PDF backend color functions**
   - Added associate blocks for interface compliance
   - Documented as placeholder implementations

### ✅ Phase 3: Dead Code Removal - MOSTLY COMPLETED
4. **✅ Removed unused functions**
   - ✅ `clean_scientific_notation` in fortplot_axes.f90
   - ✅ `remove_trailing_zeros` in fortplot_axes.f90  
   - ✅ `draw_single_tick` in fortplot_axes.f90
   - ✅ `format_tick_value_consistent` in fortplot_ticks.f90
   - 🚧 `render_simple_character_block` in fortplot_text.f90 (can be removed)
   - 🚧 `render_character_bitmap` in fortplot_text.f90 (can be removed)

5. **✅ Removed unused variables**
   - ✅ Cleaned up loop variables and temporary variables across multiple modules

### ✅ Phase 4: Interface Consistency - COMPLETED
6. **✅ Documented intentionally unused parameters**
   - Successfully used associate blocks to suppress warnings
   - Maintained clean interface contracts

### 🚧 Phase 5: Symlog Implementation - PLACEHOLDER DOCUMENTED
7. **🚧 Symlog tick functions now properly documented**
   - ✅ Added associate blocks to suppress warnings in stub functions
   - ✅ `add_positive_symlog_ticks` - documented as placeholder
   - ✅ `add_linear_symlog_ticks` - documented as placeholder  
   - ✅ `add_negative_symlog_ticks` - documented as placeholder
   - Future: Complete actual implementations when needed

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