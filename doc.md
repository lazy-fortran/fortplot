title: fortplot
summary: Modern Fortran plotting library with multiple backends
---

# Grid defaults and no-args behavior

## Default grid state

The default grid visibility follows the active style:

- **Matplotlib (MPL) mode**: grid is **off** by default (`rcParams['axes.grid'] == .false.`).
- **Vega-Lite mode**: grid is **on** by default.

This matches [`fortplot_spec_config_defaults`](src/spec/fortplot_spec_config_defaults.f90) where
`cfg%axis%grid = .false.` for MPL and `.true.` for Vega-Lite.

## No-args behavior

Calling `grid()` with **no visibility argument** (`visible` or `enabled` absent) and
**no styling kwargs** is a no-op: it leaves the current grid visibility unchanged.

When **any styling kwarg** (`which`, `axis`, `alpha`, `linestyle`) is present without a
visibility argument, the grid is **implicitly enabled** and the styling kwargs are applied.
This lets callers adjust grid appearance while turning the grid on:

```fortran
call grid(which='minor', alpha=0.3_wp)  ! enable grid; style minor lines at 30% opacity
call grid(axis='x')                      ! enable x-axis grid only
```

To explicitly toggle visibility, always pass `visible`:

```fortran
call grid(visible=.true.)   ! turn grid on
call grid(visible=.false.)  ! turn grid off
```

Note: in the figure layer, styling kwargs (`which`, `axis`, `alpha`, `linestyle`) always
set `grid_enabled = .true.` regardless of the `enabled` flag.  This means that calling
`grid(which='minor')` without a visibility argument implicitly enables the grid, while
`grid(visible=.true., which='minor')` is redundant (the styling kwarg already enables it).
