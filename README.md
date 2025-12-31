# fortplot

[![Docs](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)

Fortran plotting. No dependencies. PNG/PDF/ASCII output.

## Install

**fpm:**
```toml
[dependencies]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

**CMake:** ([full template](example/user_compilation_examples/cmake_project_template/))
```cmake
include(FetchContent)
FetchContent_Declare(fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG main)
FetchContent_MakeAvailable(fortplot)
target_link_libraries(your_target PRIVATE fortplot::fortplot)
```

## Usage

```fortran
use fortplot

call figure()
call plot(x, sin(x), label="sin(x)")
call title("Plot")
call savefig("plot.png")
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
call bar(labels, values)                             ! bar chart
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
```

## Output

```fortran
call savefig("plot.png")   ! raster
call savefig("plot.pdf")   ! vector
call savefig("plot.txt")   ! ASCII
call show()                ! viewer
```

## Animation

```fortran
use fortplot_animation

anim = FuncAnimation(update_frame, frames=100, interval=50, fig=fig)
call anim%save("movie.mp4", fps=24, status=status)
```
Requires ffmpeg: `apt install ffmpeg` / `brew install ffmpeg` / `choco install ffmpeg`

## Build

```bash
make build    # or: fpm build
make test
make example
```

## Examples

[Gallery with images](https://lazy-fortran.github.io/fortplot/page/examples/index.html)
