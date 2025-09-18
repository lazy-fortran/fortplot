title: Scatter Plot Guide
---

# Scatter Plot Guide

Fortplot provides a single scatter API that scales from quick exploratory
visualisation to publication graphics. This guide consolidates the former
"tutorial" and "advanced" notes into one workflow-oriented reference.

## 1. Quick Start

Create your first scatter plot with either API. Both produce identical output.

```fortran
program quick_start_scatter
    use fortplot
    implicit none

    real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]

    call figure()
    call scatter(x, y, label='Data Points')
    call title('Sample Scatter Plot')
    call xlabel('X values')
    call ylabel('Y values')
    call legend()
    call savefig('scatter_basic.png')
end program quick_start_scatter
```

```fortran
program quick_start_scatter_oo
    use fortplot
    implicit none

    real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
    type(figure_t) :: fig

    call fig%initialize(800, 600)
    call fig%scatter(x, y, label='Data Points')
    call fig%set_title('Sample Scatter Plot')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('scatter_basic_oo.png')
end program quick_start_scatter_oo
```

## 2. Visual Channels at a Glance

| Parameter | Purpose | Typical Range |
|-----------|---------|---------------|
| `s`       | Marker size (area) | `10.0_wp` - `200.0_wp` |
| `c`       | Color values        | Normalise to 0-1 for gradients |
| `marker`  | Shape               | `'circle'`, `'triangle'`, `'square'`, `'star'`, ... |
| `alpha`   | Transparency        | `0.6_wp` - `1.0_wp` |
| `edgecolor` / `facecolor` | Per-point styling | RGB triplets `0.0_wp` - `1.0_wp` |

Combine multiple channels to carry more information without overwhelming the
plot. Always normalise or scale the underlying data so that size and colour
remain legible.

## 3. Bubble and Colour Mapped Plots

```fortran
program bubble_and_colour
    use fortplot
    implicit none

    real(wp) :: temperature(8) = [15.2_wp, 22.1_wp, 8.3_wp, 28.7_wp, &
                                   18.9_wp, 12.4_wp, 31.2_wp, 5.8_wp]
    real(wp) :: population(8) = [2.1_wp, 5.3_wp, 1.2_wp, 8.7_wp, &
                                  3.4_wp, 1.8_wp, 6.2_wp, 0.9_wp]
    real(wp) :: city_area(8) = [100.0_wp, 300.0_wp, 50.0_wp, 500.0_wp, &
                                 200.0_wp, 80.0_wp, 400.0_wp, 30.0_wp]
    real(wp) :: gdp_pc(8) = [45000.0_wp, 55000.0_wp, 38000.0_wp, 62000.0_wp, &
                              48000.0_wp, 41000.0_wp, 58000.0_wp, 35000.0_wp]

    type(figure_t) :: fig
    real(wp) :: sizes(8), colours(8)

    sizes = 0.15_wp * city_area + 12.0_wp        ! keep markers readable
    colours = (gdp_pc - minval(gdp_pc)) / max( &
        (maxval(gdp_pc) - minval(gdp_pc)), 1.0_wp)

    call fig%initialize(900, 600)
    call fig%scatter(temperature, population, s=sizes, c=colours, &
                     colormap='plasma', show_colorbar=.true., &
                     marker='circle', alpha=0.75_wp, &
                     label='Cities (size=area, colour=GDP)')
    call fig%set_title('Multi-dimensional City Analysis')
    call fig%set_xlabel('Average Temperature (deg C)')
    call fig%set_ylabel('Population (millions)')
    call fig%legend()
    call fig%savefig('scatter_multichannel.png')
end program bubble_and_colour
```

**Tips**
- Clip very small or very large `s` values to maintain visual balance.
- Pair `alpha` with dense datasets to avoid saturated blobs.
- Use `show_colorbar=.true.` whenever colour encodes quantitative data.

## 4. Styling Building Blocks

| Goal | Snippet |
|------|---------|
| Categorical markers | `call scatter(x, y, marker='triangle')` |
| Transparent fill with outline | `alpha=0.6_wp`, `edgecolor=[0.0_wp, 0.0_wp, 0.0_wp]` |
| Highlight subset | Call `scatter` twice; first for context, second for highlight |
| Viewer friendly export | `call savefig('plot.pdf', dpi=200)` |
| Terminal preview | `call savefig_ascii('plot.txt')` |

For point-by-point styling pass arrays to `marker`, `facecolor` or `edgecolor`.
Refer to the API signature in `src/figures/scatter_figure.f90` for the complete
set of optional arguments.

## 5. Performance and Validation

- Inputs are validated for consistent dimensions, finite values, and sensible
  marker sizes. The library removes invalid points automatically.
- Rendering remains interactive up to ~10,000 points. For larger datasets
  thin the input or fall back to rasterised exports.
- When working with the stateful API call `figure()` once and reuse the context
  to avoid repeated setup cost.

## 6. Troubleshooting Checklist

| Symptom | Resolution |
|---------|------------|
| Plot is empty | Ensure `x` and `y` arrays contain non-zero size; warnings mention zeros. |
| Missing colourbar | Set `show_colorbar=.true.` when `c=` is provided. |
| Markers look uniform | Normalise `s`/`c` inputs and avoid extreme ranges. |
| Legends duplicate entries | Supply `label` only on the series you want listed. |
| File too large | Save to PDF for vector output or lower `dpi` for PNG. |

## 7. Further Reading

- [Surface Plot Guide](surface_plot_guide.md) for 3D datasets
- [Unicode Support](unicode_support.md) when labelling with symbols
- [Warning System](warning_system.md) for environment-level overrides

This single guide replaces the scattered tutorial and advanced notes, providing
an opinionated path for scatter plots from basics to production-ready figures.
