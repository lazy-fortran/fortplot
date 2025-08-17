# ![fortplot logo](media/logo.jpg)

[![codecov](https://codecov.io/gh/lazy-fortran/fortplot/branch/main/graph/badge.svg)](https://codecov.io/gh/lazy-fortran/fortplot)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)

Fortran-native plotting inspired by Python's `matplotlib.pyplot` and https://github.com/jacobwilliams/pyplot-fortran . This library is under active development and API still subject to change. There are no external dependencies. Ironically, it has also Python interface installable via `pip` (see below) `fortplot.fortplot` that can be used as a drop-in replacement for `matplotlib.pyplot` for a limited set of features.

## Usage

```fortran
use fortplot
```

### Stateful API
```fortran
call figure()
call plot(x, y)
call title("Function Plot")
call xlabel("x")
call ylabel("y")
call xlim(0.0d0, 10.0d0)  ! Set x-axis limits
call ylim(-1.0d0, 1.0d0)   ! Set y-axis limits
call savefig("plot.png")
```

### Object-Oriented API
```fortran
type(figure_t) :: fig

call fig%initialize()
call fig%set_title("Function Plot")
call fig%set_xlabel("x")
call fig%set_ylabel("y")
call fig%add_plot(x, yf)
call fig%add_3d_plot(x, y, z, label="3D data")  ! 3D plotting
call fig%savefig("plot_oo.png")
```

### Advanced Examples

#### 3D plotting
```fortran
call figure(800, 600)
call add_3d_plot(x, y, z, label="3D curve")
call title("3D Line Plot")
call savefig("3d_plot.png")
```

#### Multiple plots with legend
```fortran
call figure(800, 600)
call plot(x, sin(x), label="sin(x)", linestyle="b-")
call plot(x, cos(x), label="cos(x)", linestyle="r--")
call plot(x, sin(2*x), label="sin(2x)", linestyle="g:")
call legend()
call savefig("trig_functions.pdf")
```

#### Unicode and Greek letters in scientific plots
```fortran
call figure(800, 600)
call title("Wave Functions: \psi(\omega t) = A e^{-\lambda t} sin(\omega t)")
call xlabel("Time \tau (normalized)")
call ylabel("Amplitude \Psi (V)")
call plot(t, damped_sine, label="\alpha decay")
call plot(t, damped_cosine, label="\beta oscillation")
call legend()
call savefig("unicode_demo.png")  ! Works in PNG, PDF, and ASCII
```

#### Contour plot with colorbar
```fortran
call figure()
call contour_filled(x_grid, y_grid, z_data, colormap="viridis", show_colorbar=.true.)
call title("Temperature Distribution")
call xlabel("X Position")
call ylabel("Y Position")
call savefig("temperature.png")
```

#### Error bars for scientific data
```fortran
type(figure_t) :: fig
call fig%initialize(800, 600)
call fig%errorbar(x, y, yerr=measurement_errors, &
                  marker='o', capsize=5.0_wp, &
                  label='Experimental data')
call fig%errorbar(x, y_theory, label='Theory', linestyle='-')
call fig%legend()
call fig%savefig("scientific_plot.png")
```

#### Log scale plot
```fortran
call figure()
call plot(x, y)
call set_xscale("log")
call set_yscale("symlog", threshold=0.01d0)
call xlim(1.0d-3, 1.0d3)
call ylim(-100.0d0, 100.0d0)
call savefig("log_plot.pdf")
```

#### Animation example
```fortran
type(animation_t) :: anim
integer :: status
anim = FuncAnimation(update_func, frames=100, interval=50, fig=fig)
call anim%save("animation.mp4", fps=24, status=status)
if (status /= 0) then
    print *, "ERROR: Animation save failed. Check ffmpeg installation."
end if
```

**MPEG Validation**: Use comprehensive validation framework to verify animation quality:
```fortran
logical :: is_valid
is_valid = validate_mpeg_comprehensive("animation.mp4")
```

For more examples, see the [example directory](example) and run

```bash
make example
```

to build and run them.

## Setup

### Dependencies

**Required:**
- Modern Fortran compiler (gfortran-11 or newer)
- FPM (Fortran Package Manager) or CMake

**Optional:**
- `ffmpeg` - Required for saving animations in compressed video formats (MP4, AVI, MKV)
  - **5-Layer Validation**: Comprehensive framework prevents false positives (Issue #32)
  - **External validation**: FFprobe integration for format verification
  - **Documentation**: See [MPEG Validation Guide](doc/mpeg_validation.md) for details

### For fpm (Fortran Package Manager) projects

Add to your `fpm.toml`:
```toml
[[dependencies]]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

### For CMake projects

Add to your `CMakeLists.txt`:
```cmake
include(FetchContent)

FetchContent_Declare(
    fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG        main
)
FetchContent_MakeAvailable(fortplot)

target_link_libraries(your_target fortplot::fortplot)
```

### For Python projects
Install the Python package with pip:

```bash
pip install git+https://github.com/lazy-fortran/fortplot.git
```

## Features

### Plot types
- [x] Line plots (`plot`) with customizable line styles and markers
- [x] Error bars (`errorbar`) with symmetric/asymmetric X/Y errors and customization
- [x] 3D line plots (`add_3d_plot`) with automatic projection
- [x] 3D surface plots (`add_surface`) for grid data visualization
- [x] Contour plots (`contour`, `contourf`) with custom levels and colormaps
- [x] Pseudocolor mesh (`pcolormesh`) with color limits and edge colors
- [x] Streamplots (`streamplot`) for vector field visualization
- [ ] Scatter plots (`scatter`)
- [x] Bar charts (`bar`)
- [x] Histograms (`hist`)
- [ ] Images (`imshow`)

### Backends
- [x] PNG (raster graphics)
- [x] PDF (vector graphics)
- [x] ASCII (terminal display)
- [x] Interactive display via system viewer (`show()`)

### Features
- [x] Line styles: solid (`-`), dashed (`--`), dotted (`:`), dashdot (`-.`)
- [x] Markers: circle (`o`), cross (`x`), square (`s`), diamond (`D`), plus (`+`), star (`*`)
- [x] Format strings (`'r-o'`, `'b--'`, `'g:'`) for matplotlib compatibility
- [x] Colormaps: viridis, plasma, inferno, crest, coolwarm, jet, rocket, mako, flare
- [x] Colorbars for contour and pcolormesh plots
- [x] Legends with automatic positioning
- [x] Scales: linear, log, symlog (with configurable threshold)
- [x] Axis limits (`xlim`, `ylim`)
- [x] Interactive display with `show()` (GUI detection for X11, Wayland, macOS, Windows)
- [x] Animation support with `FuncAnimation` (requires `ffmpeg` for video formats)
  - **5-Layer Validation**: Comprehensive framework with size, header, semantic, and external tool checks
  - **False Positive Prevention**: Solves Issue #32 with multi-criteria validation
- [x] Unicode and LaTeX-style Greek letters (`\alpha`, `\beta`, `\gamma`, etc.) in all backends
- [ ] Subplots
- [ ] Annotations


## Why though?

Mostly for the lulz and to help make Fortran great again. In addition,
there is a need for high-quality high-performance plotting directly from Fortran
with the side-effect of a higher-performance limited-feature version of `matplotlib.pyplot`.

Timing comparison
```bash
time make example_matplotlib
5.13s user 0.52s system 91% cpu 6.159 total

time make example_python
1.35s user 0.17s system 97% cpu 1.562 total
```
