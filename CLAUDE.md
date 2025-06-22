# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
- `make build` - Compile the project
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

### SOLID Principles (Adapted for Fortran)

**S - Single Responsibility**: Each routine has one clear purpose, max 30 lines
**O - Open/Closed**: Extend through inheritance/composition, not modification  
**L - Liskov Substitution**: Derived types must work wherever base types do
**I - Interface Segregation**: Keep interfaces focused and minimal
**D - Dependency Inversion**: Depend on abstractions (abstract types), not concrete implementations

### DRY and KISS Principles

**DRY - Don't Repeat Yourself**: Extract common functionality into shared modules
- Create common modules for shared logic (e.g., `fortplot_margins` for margin calculations)
- Use procedure pointers for backend-agnostic operations
- Centralize constants and magic numbers in one place

**KISS - Keep It Simple, Stupid**: Favor simplicity over cleverness
- Write clear, readable code over "clever" optimizations
- Use straightforward algorithms unless performance demands complexity
- Prefer explicit over implicit behavior
- Choose clear variable names over short abbreviations

```fortran
! DRY: Common functionality extracted to shared module
use fortplot_margins, only: plot_margins_t, calculate_plot_area

! KISS: Clear, simple coordinate transformation
pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * plot_width + plot_left
```

### Test-Driven Development

1. **Write failing test first** in `test/test_*.f90`
2. **Run with `make run`** to confirm failure
3. **Write minimal code** to make test pass
4. **Refactor** while keeping tests green
5. **Repeat** for next feature

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

### Code Organization

**Routine Size**: Max 30 lines, single responsibility
**Naming**: Use descriptive verbs (`calculate_bounds` not `calc`)
**Placement**: Helper routines after caller, shared utilities at module end
**Comments**: Only for complex algorithms, let code self-document

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

### Misc

- Always explicitly import with `use only`. No wildcard imports allowed.
- Use `implicit none` in all modules and programs
