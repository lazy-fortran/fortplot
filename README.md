# fortplot

[![Docs](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)

Fortran plotting. No dependencies. PNG/PDF/ASCII output.

## Install

```toml
# fpm.toml
[dependencies]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

## Usage

```fortran
use fortplot

call figure()
call plot(x, sin(x), label="sin(x)")
call title("Plot")
call savefig("plot.png")
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

## Build

```bash
make build    # or: fpm build
make test
make example
```

## Examples

[Gallery with images](https://lazy-fortran.github.io/fortplot/page/examples/index.html)
