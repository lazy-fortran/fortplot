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

Calling `grid()` with **no visibility argument** (`visible` or `enabled` absent) does **not**
toggle the grid state. Instead, it forwards any styling kwargs (`which`, `axis`, `alpha`,
`linestyle`) to the active style default. This lets callers adjust grid appearance without
changing visibility:

```fortran
call grid(which='minor', alpha=0.3_wp)  ! style minor grid; visibility unchanged
```

To explicitly toggle visibility, always pass `visible`:

```fortran
call grid(visible=.true.)   ! turn grid on
call grid(visible=.false.)  ! turn grid off
```
