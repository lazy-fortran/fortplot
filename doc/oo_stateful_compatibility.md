title: OO vs Stateful Compatibility
---

# OO vs Stateful Compatibility Guide

fortplot intentionally ships two user-facing interfaces:

- **Stateful (matplotlib-style)** — thin wrappers that mirror
  `pyplot-fortran` and maintain positional-call compatibility with
  `matplotlib.pyplot`.
- **Object-oriented (`figure_t`)** — explicit figure instances that provide
  richer composition APIs while sharing the same plotting engine.

This guide documents how the two styles relate, what naming differences exist,
and which keyword arguments must be kept in sync when adding new helpers.

## API Mapping

| Stateful helper | OO helper | Notes |
| --------------- | --------- | ----- |
| `plot`          | `figure_t%add_plot`          | Primary line plotting entry point |
| `scatter`       | `figure_t%add_scatter`       | 3D scatter available via optional `z` array |
| `step`          | `figure_t%add_step`          | Supports legacy `where` positional argument |
| `stem`          | `figure_t%add_stem`          | Mirrors `linefmt`, `markerfmt`, `basefmt` order |
| `fill`          | `figure_t%add_fill`          | Delegates to `add_fill_between` with baseline |
| `fill_between`  | `figure_t%add_fill_between`  | Preserves mask/interpolate keyword semantics |
| `polar`         | `figure_t%add_polar`         | `fmt` positional arg kept for pyplot parity |
| `pie`           | `figure_t%add_pie`           | Uses identical ordering for labels/colors |
| `imshow`        | `figure_t%add_imshow`        | Shares `cmap`, `origin`, `extent` permutations |
| `pcolormesh`    | `figure_t%add_pcolormesh`    | Column-major data layout shared |
| `hist`/`histogram` | `figure_t%hist`           | Density/bin handling identical |
| `boxplot`       | `figure_t%boxplot`           | Optional keyword set aligned |
| `bar`/`barh`    | `figure_t%add_bar` variants | Width/align defaults tracked together |

When introducing a new stateful helper, first add the corresponding
`figure_t%...` routine. Then implement a thin wrapper that calls
`ensure_fig_init()` and forwards arguments without reordering or renaming.

## Argument Ordering Expectations

The stateful façade must continue to accept legacy positional calls copied from
`pyplot-fortran`. Key rules:

- **No reordering** — keep optional arguments in the same order as the OO
  helper. Add new optional arguments to the **end** of both signatures.
- **Keyword parity** — optional keywords supported by the OO API should remain
  available on the stateful wrapper. Unsupported keywords must raise a clear
  warning while leaving existing behavior unchanged.
- **Default consistency** — both paths should derive defaults from a single
  implementation. Avoid duplicating default values in wrappers.

Examples of valid positional calls that must continue compiling:

```fortran
call step(x, y, 'pre', 'sample', '--', 'blue', 2.0_wp)
call stem(x, y, 'r-', 'ro', 'g-', 'baseline', 0.0_wp)
call fill_between(x, y1, y2, mask, 'orange', 0.4_wp, 'band', .false.)
call imshow(z, 'viridis', 0.5_wp, 0.0_wp, 1.0_wp, 'lower', extent, &
            'nearest', 'equal')
```

## Contributor Checklist

Before merging changes that touch plotting helpers:

1. Update both the stateful wrapper and the `figure_t` method together.
2. Verify legacy positional calls compile by running:
   ```bash
   make test ARGS="--target test_pyplot_legacy_order"
   make test ARGS="--target test_legacy_positional_order"
   ```
3. Document any intentional keyword deviations in commit messages and module
   comments.
4. If a keyword is unsupported, emit a warning (not a silent no-op) in both
   code paths.

Following this checklist keeps new helpers compatible with downstream users who
ported code from `pyplot-fortran` or reference matplotlib snippets.
