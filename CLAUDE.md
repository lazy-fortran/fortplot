# CLAUDE.md

This file provides **MANDATORY** guidance to Claude Code (claude.ai/code) when working with code in this repository.

**⚠️ CRITICAL: YOU MUST ADHERE TO ALL PRINCIPLES BELOW ⚠️**
These are not suggestions - they are strict requirements that MUST be followed in every code change.

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

- `make run` - Build and run examples (default development workflow)
- `make ref` - Generate Python matplotlib reference plots for visual comparison
- `make build` - Compile the project
- `make test` - Run all unit tests in test/ directory
- `make clean` - Clean build artifacts

## File Organization

**Library Sources**: Place library sources in `src/` directory
**Debugging**: Place debugging sources in `app/` directory and execute with `make run`
**Unit Tests**: Place unit tests in `test/` directory  
**Examples**: Place examples in `example/` directory

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

**NO EXCEPTIONS: Every code change MUST follow TDD approach**

1. **Write failing test first** in `test/test_*.f90`
2. **Run with `make run`** to confirm failure
3. **Write minimal code** to make test pass
4. **Refactor** while keeping tests green
5. **Repeat** for next feature

**TDD is not optional** - it is the foundation of all development in this codebase.

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

### Misc - STRICTLY ENFORCED

**⚠️ CRITICAL: THESE RULES HAVE NO EXCEPTIONS ⚠️**

- Always explicitly import with `use only`. No wildcard imports allowed. - **MANDATORY**
- Use `implicit none` in all modules and programs - **REQUIRED**
