# fortplot Examples

Each subdirectory is one runnable example: a `.f90` source file and a
`README.md` describing what it shows. Run one by name:

```bash
make example ARGS="basic_plots"
```

Run every example with a bare `make example`. Output lands in
`output/example/fortran/<name>/`.

The rendered gallery, with images and the generated text output for each
example, is at
<https://lazy-fortran.github.io/fortplot/page/examples/index.html>.

## Basics

- [basic_plots](./basic_plots/) — line plots and saving to PNG, PDF, and text
- [styling_demo](./styling_demo/) — line styles, markers, and format strings
- [legend_demo](./legend_demo/) — legend contents and placement
- [grid_demo](./grid_demo/) — major and minor grid lines
- [scale_examples](./scale_examples/) — linear, log, and symlog axes
- [disconnected_lines](./disconnected_lines/) — gaps via NaN separators
- [annotation_demo](./annotation_demo/) — text annotations in data coordinates

## Statistical and categorical

- [scatter_demo](./scatter_demo/) — colour mapping and variable marker sizes
- [errorbar_demo](./errorbar_demo/) — symmetric and asymmetric error bars
- [boxplot_demo](./boxplot_demo/) — box-and-whisker plots
- [bar_chart_demo](./bar_chart_demo/) — grouped, stacked, and horizontal bars
- [pie_chart_demo](./pie_chart_demo/) — exploded wedges and `autopct` labels
- [fill_between_demo](./fill_between_demo/) — shaded regions between curves

## Fields and 3D

- [contour_demo](./contour_demo/) — line and filled contours with colormaps
- [pcolormesh_demo](./pcolormesh_demo/) — pseudocolour mesh plots
- [streamplot_demo](./streamplot_demo/) — streamlines of a 2D vector field
- [quiver_demo](./quiver_demo/) — arrow plots for discrete vector fields
- [3d_plotting](./3d_plotting/) — 3D lines, scatter, and surfaces

## Axes and layout

- [subplot_demo](./subplot_demo/) — multi-panel grids
- [twin_axes_demo](./twin_axes_demo/) — `twinx` and `twiny` with independent scales
- [polar_demo](./polar_demo/) — polar projection
- [datetime_axis_demo](./datetime_axis_demo/) — date and time tick labels
- [dpi_demo](./dpi_demo/) — output resolution control

## Text and symbols

- [unicode_demo](./unicode_demo/) — Unicode symbols in labels and titles
- [mathtext_demo](./mathtext_demo/) — LaTeX-style math in labels and titles
- [ascii_heatmap](./ascii_heatmap/) — heatmaps rendered for the terminal

## Animation

- [animation](./animation/) — MP4 from a frame sequence
- [3d_animation_demo](./3d_animation_demo/) — rotating 3D curve, MP4 and text
- [probability_animation_demo](./probability_animation_demo/) — an evolving
  Gaussian distribution

## Display

- [display_demo](./display_demo/) — `show()` and `show_viewer()` behaviour

## Adding an example

1. Create `example/fortran/<name>/` with `<name>.f90`.
2. Add a `README.md` starting with `title: <Title>`, then `---`, then a
   one-paragraph description. Do not list the generated files or repeat the
   run command — the documentation generator emits both from the actual
   output directory, and a hand-maintained list goes stale.
3. Write output to `output/example/fortran/<name>/`.
4. `make doc` picks the example up automatically.
