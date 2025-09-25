title: Bar Chart Demo
---

# Bar Chart Demo

Source: [bar_chart_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/bar_chart_demo/bar_chart_demo.f90)

Grouped bar charts for categorical comparisons using both the pyplot-style
stateful helpers and the `figure_t` object API.

- **Stateful grouped bars**: `call bar(...)` renders side-by-side series for
  quarterly revenue comparisons.
- **Horizontal bars**: `call barh(...)` presents ranked completion percentages
  for training modules.
- **Object-oriented workflow**: `call bar_impl(fig, ...)` builds the same
  grouped view without relying on the global figure state.
- **Multi-format outputs**: Every scenario writes PNG, PDF, and ASCII artefacts
  under `output/example/fortran/bar_chart_demo/`.

## Stateful API Example

```fortran
call figure(figsize=[7.5d0, 5.0d0])
call bar(positions_a, product_a, width=0.4d0, label='Product A')
call bar(positions_b, product_b, width=0.4d0, label='Product B')
call legend()
```

## Object API Example

```fortran
call fig%initialize()
call bar_impl(fig, positions_baseline, baseline, width=0.32d0, label='Baseline')
call bar_impl(fig, positions_projected, projected, width=0.32d0, label='Projected')
call fig%legend()
```

## Running

```bash
make example ARGS="bar_chart_demo"
```

## Output Artefacts

- `stateful_grouped.(png|pdf|txt)` – Vertical grouped bars comparing two series
- `stateful_horizontal.(png|pdf|txt)` – Horizontal ranking chart
- `oo_grouped.(png|pdf|txt)` – Object-oriented grouped comparison

These outputs provide quick evidence that bar charts render correctly across
fortplot's interfaces and export formats.
