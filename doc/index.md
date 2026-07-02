title: Documentation
---

# fortplot

Fortran plotting library. No external dependencies. PNG, PDF, and text output
(the text backend renders the ASCII charset by default).

Install with fpm:

```toml
[dependencies]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

Quick start:

```fortran
use fortplot

call figure()
call plot(x, sin(x), label="sin(x)")
call title("My plot")
call savefig("plot.png")
```

See the [API reference](./fortplot/index.html) for the full module documentation.

## Braille text mode

The text backend can render line and scatter data as Unicode braille dots for
higher resolution inside each terminal cell, while axes, ticks, and labels stay
plain text. Each character cell holds a 2-by-4 dot grid encoded as
`U+2800 + bitmask`; the dot-bit layout matches Drawille (left column dots
1,2,3,7 -> `0x01,0x02,0x04,0x40`; right column dots 4,5,6,8 ->
`0x08,0x10,0x20,0x80`).

```fortran
type(figure_t) :: fig
call fig%initialize(80, 24)
call fig%set_text_charset('braille')
call fig%add_plot(x, y)
call fig%savefig('plot.txt')
```

Braille is opt-in: `.txt` output stays ASCII unless `set_text_charset('braille')`
is called.

## Examples

```bash
make example ARGS="example_name"
```

<!-- AUTO_EXAMPLES_START -->

- [3D Animation Demo](./examples/3d_animation_demo.html) - Animate a rotating 3D Lissajous curve. The same `FuncAnimation` plus `save_animation` pipeline writes both MP4...
- [3D Plotting](./examples/3d_plotting.html) - 3D plotting (lines and surfaces) with axes, ticks, and labels.
- [Animation](./examples/animation.html) - Generate an MP4 animation from a sequence of frames.
- [Annotation Demo](./examples/annotation_demo.html) - Add text annotations in data coordinates.
- [Ascii Heatmap](./examples/ascii_heatmap.html) - Render a heatmap to terminal-friendly ASCII output.
- [Bar Chart Demo](./examples/bar_chart_demo.html) - Demonstrates grouped bar charts (vertical and horizontal) via both the stateful API and `figure_t`.
- [Basic Plots](./examples/basic_plots.html) - Basic line plots and saving outputs (PNG, PDF, ASCII).
- [Boxplot Demo](./examples/boxplot_demo.html) - Demonstrates box-and-whisker plots for statistical data visualization.
- [Contour Demo](./examples/contour_demo.html) - Comprehensive contour plotting examples: line contours, filled contours, custom levels, colormaps.
- [Datetime Axis Demo](./examples/datetime_axis_demo.html) - Date and time axes using `datetime_t` values and formatted tick labels.
- [Disconnected Lines](./examples/disconnected_lines.html) - Line plots with gaps created by NaN separators.
- [Display Demo](./examples/display_demo.html) - `show_viewer()` opens a PDF viewer. `show()` uses the viewer when a GUI is available, and ASCII output otherwise.
- [Dpi Demo](./examples/dpi_demo.html) - Control output DPI when saving figures for consistent sizing across formats.
- [Errorbar Demo](./examples/errorbar_demo.html) - Demonstrates error bar plotting with both symmetric and asymmetric errors for scientific data visualization.
- [Fill Between Demo](./examples/fill_between_demo.html) - Create filled regions using both the stateful API and `figure_t`.
- [Grid Demo](./examples/grid_demo.html) - Demonstrates grid functionality for enhanced plot readability.
- [Legend Demo](./examples/legend_demo.html) - Legends, labels, and legend placement.
- [Mathtext Demo](./examples/mathtext_demo.html) - Render math text and symbols in labels and titles.
- [Pcolormesh Demo](./examples/pcolormesh_demo.html) - Pcolormesh heatmaps with colormaps and shading.
- [Pie Chart Demo](./examples/pie_chart_demo.html) - Build pie charts using both the stateful API and `figure_t` (exploded wedges, `autopct`, and start angles).
- [Polar Demo](./examples/polar_demo.html) - Demonstrates fortplot's polar plotting API with custom colors, linestyles, and markers.
- [Probability Animation Demo](./examples/probability_animation_demo.html) - Animate a Gaussian probability distribution as its width changes over time.
- [Quiver Demo](./examples/quiver_demo.html) - Quiver plots for discrete vector fields with scaled arrows and backend outputs.
- [Scale Examples](./examples/scale_examples.html) - Linear, log, and symlog axis scales.
- [Scatter Demo](./examples/scatter_demo.html) - Demonstrates enhanced scatter plotting with color mapping, variable marker sizes, and bubble charts.
- [Streamplot Demo](./examples/streamplot_demo.html) - Streamplots for 2D vector fields.
- [Styling Demo](./examples/styling_demo.html) - Line styles, markers, format strings, and scatter styling.
- [Subplot Demo](./examples/subplot_demo.html) - Demonstration of subplot functionality using the stateful API.
- [Twin Axes Demo](./examples/twin_axes_demo.html) - Demonstrates `twinx` and `twiny` for multiple axes on one figure.
- [Unicode Demo](./examples/unicode_demo.html) - Unicode symbols in labels and titles.

<!-- AUTO_EXAMPLES_END -->
