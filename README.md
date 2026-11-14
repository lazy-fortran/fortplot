# fortplot

[![Docs](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24223523.svg)](https://doi.org/10.5281/zenodo.24223523)

Fortran plotting with PNG, PDF, SVG and text output. The plotting core has
no external graphics dependencies. Text output uses ASCII by default.

## Install

**fpm:**
```toml
[dependencies]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

**CMake:** ([full template](https://github.com/lazy-fortran/fortplot/tree/main/example/user_compilation_examples/cmake_project_template))
```cmake
include(FetchContent)
FetchContent_Declare(fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG main)
FetchContent_MakeAvailable(fortplot)
target_link_libraries(your_target PRIVATE fortplot::fortplot)
```

The optional `fortplot::fortarray` target adapts labeled `data_array_t` values
without adding plotting methods or state to fortarray:

```fortran
use fortarray,          only: data_array_t, data_array
use fortplot,           only: figure_t, wp
use fortplot_fortarray, only: plot

type(data_array_t) :: field
type(figure_t) :: fig

field = data_array([2.0_wp, 3.0_wp, 5.0_wp], ["radius"], name="samples")
call fig%initialize()
call plot(fig, field)
```

CMake users can omit that dependency with
`-DFORTPLOT_BUILD_FORTARRAY_ADAPTER=OFF`. fpm dependencies are package-wide,
so fpm fetches fortarray even when an application uses only fortplot's core.

## Usage

```fortran
program first_plot
    use fortplot, only: wp, figure, plot, title, savefig
    implicit none
    integer :: i
    real(wp) :: x(101)

    x = [(0.1_wp * real(i - 1, wp), i = 1, size(x))]
    call figure()
    call plot(x, sin(x), label="sin(x)")
    call title("Sine")
    call savefig("plot.png")
end program first_plot
```

**Object-oriented API** (for multiple independent figures):
```fortran
type(figure_t) :: fig
call fig%initialize(800, 600)
call fig%add_plot(x, y, label="data")
call fig%savefig("plot.png")
```

## Plot Types

```fortran
call plot(x, y, "r--o")                              ! line
call scatter(x, y, s=sizes, c=colors)                ! scatter
call errorbar(x, y, yerr=err)                        ! error bars
call contour(x, y, z)                                ! contour
call pcolormesh(x, y, z, colormap="viridis")         ! heatmap
call bar(x, values)                             ! bar chart
call hist(data, bins=20)                             ! histogram
call polar(theta, r)                                 ! polar
call add_3d_plot(x, y, z)                            ! 3D line
call streamplot(x, y, u, v)                          ! vector field
```

## Styling

```fortran
! Format: "color linestyle marker" e.g. "r--o" = red dashed circles
! Colors: r g b c m y k w
! Lines: - -- : -.
! Markers: o s x + * D

call xlim(0.0_wp, 10.0_wp)
call set_xscale("log")
call title("Greek: \\alpha \\beta \\gamma")          ! LaTeX

! Grid lines (default: off in MPL mode, on in Vega-Lite mode)
call grid(visible=.true.)                            ! enable grid
call grid(which='minor', alpha=0.3_wp)               ! enable grid; style minor lines
call grid(visible=.false.)                           ! turn grid off
```

## Output

```fortran
call savefig("plot.png")   ! raster
call savefig("plot.pdf")   ! vector
call savefig("plot.txt")   ! text backend (ASCII charset by default)
call show()                ! viewer
```

## Animation

```fortran
use fortplot

type(figure_t) :: fig
type(animation_t) :: anim

anim = FuncAnimation(update_frame, frames=100, interval=50, fig=fig)
call anim%save("movie.mp4", fps=24, status=status)   ! ffmpeg
call anim%save("movie.txt", status=status)           ! text frames
```

`fortplot` re-exports `FuncAnimation` and `animation_t`; use `fortplot_animation`
only when you are not using the main `fortplot` module.

Output formats: `.mp4`, `.avi`, `.mkv` (require ffmpeg), and `.txt` (text backend,
ASCII charset by default, no extra dependencies).

Replay a `.txt` animation in the terminal:

```bash
fpm run --target fortplot_play_text -- movie.txt --fps 24 --loop
# fortplot_play_ascii is a compatibility alias for the same player
```

ffmpeg install: `apt install ffmpeg` / `brew install ffmpeg` / `choco install ffmpeg`

## Build

```bash
make build    # or: fpm build
make test
make example
```

## Examples

[Gallery with images](https://lazy-fortran.github.io/fortplot/page/examples/index.html)


## Status and documentation

The test suite covers the public API, file output, coordinate transforms,
state isolation between figures, and selected rendering regressions. Separate
Matplotlib comparisons check geometry, markers, error bars, ticks and math
structure in PNG and PDF output. Complete visual parity with Matplotlib has
not been established; contour/colorbar layouts, mixed 3D rendering and formula
positioning have open comparisons. See the
[verification scope](https://lazy-fortran.github.io/fortplot/page/internals/visual-parity.html) before relying on exact
appearance across plotting systems.

The [documentation map](https://lazy-fortran.github.io/fortplot/page/index.html) links the
[user guide](https://lazy-fortran.github.io/fortplot/page/user-guide.html), [module overview](https://lazy-fortran.github.io/fortplot/page/modules.html),
API compatibility notes and examples. See [CONTRIBUTING.md](https://github.com/lazy-fortran/fortplot/blob/main/CONTRIBUTING.md)
for development checks and [CHANGELOG.md](https://github.com/lazy-fortran/fortplot/blob/main/CHANGELOG.md) for changes.

## Citation and credits

Cite fortplot through its Zenodo record,
[doi:10.5281/zenodo.24223523](https://doi.org/10.5281/zenodo.24223523), which
always resolves to the latest release; release 2026.11.0 is
[doi:10.5281/zenodo.24223524](https://doi.org/10.5281/zenodo.24223524).

Citation metadata is in [CITATION.cff](https://github.com/lazy-fortran/fortplot/blob/main/CITATION.cff). Christopher Albert
maintains fortplot. Ingo Peball contributed font discovery on openSUSE,
local font overrides, and build and Windows test fixes. Third-party fonts,
algorithms and source adaptations retain their notices in
[thirdparty/LICENSE](https://github.com/lazy-fortran/fortplot/tree/main/thirdparty/LICENSE). fortplot is distributed under the
[MIT licence](https://github.com/lazy-fortran/fortplot/blob/main/LICENSE).
