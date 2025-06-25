# CLAUDE.md

This file provides **MANDATORY** guidance to Claude Code (claude.ai/code) when working with code in this repository.

**⚠️ CRITICAL: YOU MUST ADHERE TO ALL PRINCIPLES BELOW ⚠️**
These are not suggestions - they are strict requirements that MUST be followed in every code change.

**⚠️ COMMUNICATION REQUIREMENTS ⚠️**
- Keep responses minimal and direct
- No flattery, congratulations, or celebration language
- Be brutally honest about issues and mistakes
- Answer questions with facts only
- Skip explanations unless specifically requested

## Project Overview

**fortplotlib** is a modern Fortran plotting library providing scientific visualization with PNG, PDF, and ASCII backends.

## API Usage

```fortran
use fortplot
type(figure_t) :: fig

call fig%initialize(width, height)
call fig%add_plot(x, y, label="data")
call fig%add_contour(x_grid, y_grid, z_grid)
call fig%savefig('output.png')  ! Auto-detects backend from extension
```

## Development Commands

**⚠️ CRITICAL: ALWAYS USE MAKE FOR ALL DEVELOPMENT TASKS ⚠️**

All development work must use the Makefile. Never run `fpm` commands directly.

**⚠️ SPELLING: It is "fpm" (Fortran Package Manager) - NEVER "fmp" ⚠️**

### Primary Development Commands

- `make example` - Build and run all examples (default development workflow)
- `make debug` - Build and run apps in app/ directory for debugging
- `make test` - Run all unit tests in test/ directory
- `make build` - Compile the project
- `make clean` - Clean build artifacts and generated plots

### Reference and Release Commands

- `make ref` - Generate Python matplotlib reference plots for visual comparison
- `make release` - Build with release optimizations
- `make run-release` - Run optimized build
- `make check-deps` - Show detected library flags and dependencies
- `make help` - Show all available targets

### Command Line Arguments Support

All make targets support passing additional fpm arguments. Use `ARGS` to pass extra parameters:

```bash
# Run specific example
make example ARGS="basic_plots"

# Run specific test
make test ARGS="--target test_specific_feature"

# Run specific app for debugging
make debug ARGS="--target debug_feature"

# Build with verbose output
make build ARGS="--verbose"
```

## File Organization

**Library Sources**: Place library sources in `src/` directory
**Debugging**: Place debugging sources in `app/` directory and execute with `make debug`
**Unit Tests**: Place unit tests in `test/` directory  
**Examples**: Place examples in `example/` directory

## External References and Inspiration

**Matplotlib Source Code**: Located in `external/matplotlib/` for layout, styling, and API reference
- **Core Reference**: `external/matplotlib/lib/matplotlib/` contains the main matplotlib implementation
- Use for understanding margin calculations, text positioning, plot layout algorithms
- Reference for color schemes, tick generation, and professional plot styling
- Study layout managers and backend implementations for consistency

**Pyplot-Fortran Wrapper**: Located in `external/pyplot-fortran/src/pyplot_module.F90`  
- **API Inspiration**: Study the Fortran wrapper patterns and interface design
- Reference for clean Fortran API design and Python integration patterns
- Learn from error handling and parameter passing approaches
- Use as inspiration for functional API design (not implementation)

### FPM Automatic Discovery

FPM automatically discovers all sources and resolves module dependencies when run via make:

- **Sources**: All `.f90` files in `src/`, `example/`, `test/`, and `app/` are automatically found
- **Dependencies**: Module dependencies are automatically resolved - no manual specification needed
- **Executables**: Each program in `example/`, `test/`, or `app/` becomes a buildable target

### Finding Available Targets

To discover available executable targets:

```bash
# List example programs
ls example/

# List test programs  
ls test/

# List debug/app programs
ls app/

# Find available modules in src/
grep -r "^module " src/
```

Example usage with discovered targets:
```bash
# Run specific example
make example ARGS="basic_plots"

# Run specific test
make test ARGS="--target test_figure_basics" 

# Run specific debug app
make debug ARGS="--target debug_symlog_data"
```

```fortran
! app/debug_feature.f90 - For debugging/development
program debug_feature
    use fortplot
    ! Your debugging code here
end program
```

## Coding Standards

### SOLID Principles (Adapted for Fortran) - MANDATORY

**⚠️ CRITICAL: ALL SOLID PRINCIPLES MUST BE FOLLOWED ⚠️**

**S - Single Responsibility**: Each routine has one clear purpose, max 30 lines - **ENFORCED**
**O - Open/Closed**: Extend through inheritance/composition, not modification - **REQUIRED**
**L - Liskov Substitution**: Derived types must work wherever base types do - **MANDATORY**
**I - Interface Segregation**: Keep interfaces focused and minimal - **ENFORCED**
**D - Dependency Inversion**: Depend on abstractions (abstract types), not concrete implementations - **REQUIRED**

### DRY and KISS Principles - MANDATORY

**⚠️ CRITICAL: DRY AND KISS ARE STRICTLY ENFORCED ⚠️**

**DRY - Don't Repeat Yourself**: Extract common functionality into shared modules - **REQUIRED**
- Create common modules for shared logic (e.g., `fortplot_margins` for margin calculations)
- Use procedure pointers for backend-agnostic operations  
- Centralize constants and magic numbers in one place
- ✓ IMPLEMENTED: All scales (linear, log, symlog) use consistent tick formatting via shared `format_tick_value()` function

**KISS - Keep It Simple, Stupid**: Favor simplicity over cleverness - **MANDATORY**
- Write clear, readable code over "clever" optimizations
- Use straightforward algorithms unless performance demands complexity
- Prefer explicit over implicit behavior
- Choose clear variable names over short abbreviations
- ✓ IMPLEMENTED: Coordinate transformation properly separated - original coordinates for tick generation, transformed coordinates for data rendering

```fortran
! DRY: Common functionality extracted to shared module
use fortplot_margins, only: plot_margins_t, calculate_plot_area

! KISS: Clear, simple coordinate transformation
pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * plot_width + plot_left
```

### Test-Driven Development (MANDATORY)

**⚠️ CRITICAL: TDD IS MANDATORY FOR ALL FEATURES AND REFACTORING ⚠️**
**⚠️ WRITE TESTS FIRST - NO EXCEPTIONS ⚠️**
**⚠️ DO NOT WRITE ANY CODE WITHOUT A FAILING TEST FIRST ⚠️**

**MANDATORY TDD WORKFLOW - NEVER DEVIATE:**

1. **WRITE FAILING TEST FIRST** in `test/test_*.f90` - **ALWAYS START HERE**
2. **RUN `make test`** to confirm the test fails (RED)
3. **Write minimal code** to make test pass (GREEN)
4. **Refactor** while keeping tests green (REFACTOR)
5. **Repeat RED-GREEN-REFACTOR** for next feature

**FORBIDDEN:**
- Writing implementation code before tests
- Changing code without a test covering the change
- Assuming existing code works without tests
- Skipping tests "just this once"

**TDD is not optional** - it is the foundation of all development in this codebase.
**TESTS FIRST, ALWAYS. NO CODE WITHOUT TESTS.**

```fortran
! test/test_new_feature.f90
program test_new_feature
    call test_should_calculate_bounds()
contains
    subroutine test_should_calculate_bounds()
        ! Arrange, Act, Assert pattern
    end subroutine
end program
```

**⚠️ CRITICAL: ALWAYS REFERENCE EXTERNAL SOURCES WHEN IMPLEMENTING FEATURES ⚠️**

When implementing new features or improving existing ones:
1. **Check matplotlib implementation** in `external/matplotlib/src/lib/matplotlib/` first
2. **Study layout and styling patterns** to ensure consistency with professional plotting standards
3. **Reference pyplot-fortran** in `external/pyplot-fortran/src/pyplot_module.F90` for clean API design patterns
4. **Test against matplotlib output** using `make ref` to generate reference plots for comparison

### Debug Scripts MUST Become Unit Tests

**⚠️ MANDATORY: ALL DEBUG SCRIPTS MUST BE CONVERTED TO TESTS ⚠️**

When creating debug scripts in `app/debug_*.f90` for development:
1. **Debug scripts are temporary** - use them to understand behavior
2. **Extract corner cases** - identify edge cases and boundary conditions  
3. **Convert to unit tests** - move all corner cases to `test/test_*.f90`
4. **Delete debug scripts** - or keep only as examples if educational

Example workflow:
```fortran
! app/debug_tick_format.f90 - TEMPORARY debugging
program debug_tick_format
    ! Test various edge cases...
    call test_small_range()
    call test_large_range()
    call test_zero_crossing()
end program

! MUST become test/test_tick_format.f90 - PERMANENT unit tests
program test_tick_format
    call test_should_handle_small_ranges()
    call test_should_handle_large_ranges()
    call test_should_handle_zero_crossing()
    print *, "All tick format tests passed!"
end program
```

**Corner cases discovered during debugging are GOLD** - they reveal real-world scenarios that must be tested to prevent regressions.

### Code Organization (MANDATORY RULES)

**⚠️ CRITICAL: THESE RULES ARE NON-NEGOTIABLE ⚠️**

**Routine Size**: Max 30 lines, single responsibility - **NO EXCEPTIONS**
**Naming**: Use descriptive verbs (`calculate_bounds` not `calc`) - **REQUIRED**
**Placement**: Helper routines after caller, shared utilities at module end - **ENFORCED**
**Comments**: Only for complex algorithms, let code self-document - **MANDATORY**

### Refactoring Strategy

Extract commented blocks into well-named routines:
```fortran
! BEFORE: Comments explain complex logic
subroutine process_data()
    ! Validate inputs
    ! Transform coordinates  
    ! Apply filters
end subroutine

! AFTER: Routine names explain purpose
subroutine process_data()
    call validate_input_data()
    call transform_coordinates()
    call apply_antialiasing_filters()
end subroutine
```

### State Management - CRITICALLY IMPORTANT

**⚠️ MUTABLE GLOBAL STATE IS THE SOURCE OF ALL EVIL AND MUST BE AVOIDED AT ALL COST ⚠️**

**MANDATORY PRINCIPLES:**
- **NO GLOBAL MUTABLE STATE** - All state must be explicitly passed as parameters
- **IMMUTABLE BY DEFAULT** - Prefer immutable data structures and pure functions
- **EXPLICIT STATE MANAGEMENT** - Always save/restore state when temporarily modifying context
- **STATELESS OPERATIONS** - Functions should not rely on hidden global state
- **CLEAR OWNERSHIP** - Each piece of state must have a clear owner and scope

**Examples of FORBIDDEN patterns:**
```fortran
! BAD: Global mutable state
module bad_module
    real(wp) :: global_line_width = 1.0_wp  ! FORBIDDEN
    logical :: drawing_enabled = .true.     ! FORBIDDEN
end module

! BAD: Hidden state mutations
subroutine bad_draw_text(ctx, text)
    ! Modifies line width globally without restoring
    call ctx%set_line_width(0.5_wp)  ! FORBIDDEN - corrupts state
    ! ... draw text ...
    ! Missing: restore original line width
end subroutine
```

**REQUIRED patterns:**
```fortran
! GOOD: Explicit state save/restore
subroutine good_draw_text(ctx, text)
    real(wp) :: saved_width
    saved_width = ctx%current_line_width  ! Save state
    call ctx%set_line_width(0.5_wp)      ! Modify
    ! ... draw text ...
    call ctx%set_line_width(saved_width) ! Restore - MANDATORY
end subroutine

! GOOD: Pure functions with explicit parameters
pure function calculate_position(x, y, offset) result(new_pos)
    ! No hidden state dependencies
end function
```

### Constants and Magic Numbers - STRICTLY ENFORCED

**⚠️ MAGIC NUMBER CONSTANTS ARE FORBIDDEN ⚠️**

**MANDATORY PRINCIPLES:**
- **NO MAGIC NUMBERS** - If a number has meaning, it MUST be a named constant
- **DESCRIPTIVE NAMES** - Constant names must clearly indicate their purpose
- **CENTRALIZED CONSTANTS** - Group related constants in parameter declarations
- **DOCUMENTED PURPOSE** - Each constant should have a clear comment explaining its meaning

**Examples of FORBIDDEN patterns:**
```fortran
! BAD: Magic numbers scattered throughout code
real(wp) :: margin = 0.15_wp           ! FORBIDDEN - what does 0.15 mean?
call set_font_size(14)                 ! FORBIDDEN - why 14?
if (error < 1e-6_wp) then             ! FORBIDDEN - what precision requirement?
```

**REQUIRED patterns:**
```fortran
! GOOD: Named constants with clear meaning
real(wp), parameter :: DEFAULT_MARGIN_FRACTION = 0.15_wp     ! 15% of plot area
integer, parameter :: TITLE_FONT_SIZE = 14                   ! Points for titles
real(wp), parameter :: NUMERICAL_TOLERANCE = 1.0e-6_wp      ! Floating point comparison threshold

! Usage
real(wp) :: margin = DEFAULT_MARGIN_FRACTION
call set_font_size(TITLE_FONT_SIZE)
if (error < NUMERICAL_TOLERANCE) then
```

### Backend Specialization and Polymorphism - STRICTLY ENFORCED

**⚠️ CRITICAL: NO SCATTERED DISPATCH LOGIC ALLOWED ⚠️**

**MANDATORY PRINCIPLES:**
- **NO SELECT TYPE IN BUSINESS LOGIC** - Backend-specific behavior must be handled in the backend implementations
- **SPECIALIZATION AT INITIALIZATION** - Configure backend-specific behavior when the backend is created
- **POLYMORPHIC INTERFACES** - Use abstract interfaces to hide backend differences from calling code
- **ENCAPSULATED SCALING** - Each backend handles its own coordinate/value scaling internally

**Examples of FORBIDDEN patterns:**
```fortran
! BAD: Scattered dispatch logic in business code
select type (backend => self%backend)
type is (png_context)
    call backend%set_line_width(0.1_wp)  ! FORBIDDEN - dispatch in caller
type is (pdf_context) 
    call backend%set_line_width(2.0_wp)  ! FORBIDDEN - backend knowledge in caller
end select
```

**REQUIRED patterns:**
```fortran
! GOOD: Backend handles its own scaling internally
call self%backend%set_line_width(2.0_wp)  ! Same call for all backends

! In PNG backend implementation:
subroutine png_set_line_width(this, width)
    this%current_line_width = width * 0.05_wp  ! PNG-specific scaling
end subroutine

! In PDF backend implementation:  
subroutine pdf_set_line_width(this, width)
    this%current_line_width = width  ! PDF uses width directly
end subroutine
```

**This ensures:**
- Business logic remains backend-agnostic
- Backend-specific knowledge stays encapsulated
- Easy to add new backends without modifying existing code
- Cleaner, more maintainable architecture

### Misc - STRICTLY ENFORCED

**⚠️ CRITICAL: THESE RULES HAVE NO EXCEPTIONS ⚠️**

- Always explicitly import with `use only`. No wildcard imports allowed. - **MANDATORY**
- Use `implicit none` in all modules and programs - **REQUIRED**
- **ALL variable declarations MUST come before any executable code in routines** - **MANDATORY**
  - Variables, parameters, and type declarations first
  - Then executable statements and assignments
  - Fortran requires this strict ordering
- **cd COMMAND IS FORBIDDEN** - Never use `cd` in bash commands. Use absolute paths instead. - **MANDATORY**

## Known Regressions and Reference Points

**⚠️ IMPORTANT: Track working functionality to prevent regression**

### Working Baseline: Commit f47d82635ae2bf789f3c74ff7ffc9d7e84b56488

This commit represents the last known working state before major refactoring where certain features were functioning correctly:

- **Y-axis text rotation and positioning** - Working correctly in PNG backend
- **FreeType-based character rotation** - Properly implemented  
- **Matplotlib-style label positioning** - Correct placement and orientation

**Use this commit as reference when fixing regressions introduced during refactoring.**

### Recently Fixed Regressions

- **Y-axis label positioning in PNG** - Fixed: Labels were appearing on right side instead of left side
  - **Issue**: Y-axis labels positioned incorrectly after refactoring
  - **Root Cause**: Hardcoded positioning instead of relative to plot_area%left
  - **Fix**: Changed `label_x = real(25, wp)` to `label_x = real(ctx%plot_area%left - 40, wp)`
  - **Status**: ✅ RESOLVED
