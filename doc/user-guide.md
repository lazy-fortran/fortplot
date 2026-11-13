title: User Guide
---

# User guide

Use the `fortplot` module for ordinary plotting. It exports the procedural
interface, the independent `figure_t` object, animation types and the real
kind `wp` used by plotting data. The complete first example is on the
[front page](../index.html).

## Figures and data

Procedural calls such as `figure`, `plot`, `title` and `savefig` operate on
the current figure. Use separate `figure_t` objects when managing several
figures or embedding plotting in a library:

```fortran
use fortplot, only: figure_t, wp

type(figure_t) :: fig
real(wp) :: x(3) = [0.0_wp, 1.0_wp, 2.0_wp]
real(wp) :: y(3) = [0.0_wp, 1.0_wp, 4.0_wp]

call fig%initialize(800, 600)
call fig%add_plot(x, y, label="samples")
call fig%savefig("samples.png")
```

Keep the coordinate and value arrays conformable. The
[examples gallery](./examples/index.html) covers scatter plots, error bars,
contours, colour meshes, subplots and the supported styling options.
The [API compatibility guide](./api_compatibility.html) maps calls used by
pyplot-fortran applications.

## Output and optional dependencies

`savefig` selects the backend from the filename extension: `.png` for raster,
`.pdf` and `.svg` for vector graphics, and `.txt` for terminal output.
Saving a file works without a display server. `show` requests an external
viewer; it is unnecessary in batch applications.

Text output uses ASCII by default. Call
`fig%set_text_charset('braille')` to render line and scatter data with Unicode
braille cells. Movie formats such as MP4 require `ffmpeg`; text animations
do not. The optional `fortplot_fortarray` module accepts labelled arrays from
fortarray. CMake can disable this adapter with
`-DFORTPLOT_BUILD_FORTARRAY_ADAPTER=OFF`.

## Verification limits

The test suite and selected Matplotlib oracles check rendering features, not
complete compatibility with every Matplotlib option. Consult the
[verification scope](./internals/visual-parity.html) for covered features,
known differences and commands for independent comparisons.
